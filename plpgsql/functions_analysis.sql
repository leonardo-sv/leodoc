-- Selection of the attributes (region, uf_acronym, centroid latitude, centroid longitude and centroid geometry) using the state Rondonia as reference.
SELECT nm_uf AS uf_name,
    nm_regiao AS region,
    sigla_uf AS uf_acronym,
    ST_Y(ST_CENTROID(geom)) AS latitude ,
    ST_X(ST_CENTROID(geom)) AS longitude,
    ST_CENTROID(geom) AS geom
FROM ibge.uf_2020 
WHERE sigla_uf='RO';

-- Create the table where the ring buffers are stored and the spatial indexing.
DROP TABLE IF EXISTS analysis.ring_buffer;
CREATE TABLE analysis.ring_buffer(
    gid serial PRIMARY KEY, 
    latitude_center float,
    longitude_center float,
    outer_radius integer,
    inner_radius integer,
    geom geometry(POLYGON, 100002)
);
CREATE INDEX idx_geom_ring_buffer ON analysis.ring_buffer USING GIST(geom); 

-- Remove older version of the centroid_ring_buffers function
DROP FUNCTION IF EXISTS centroid_ring_buffers(minimum integer, maximum integer, step integer, size_ring integer,wkt_geom text, srid_geom integer);

-- Create the "plpgsql" function that returns concentric ring buffers considering the centroid of a geometry passed as argument. 
-- The function received a wkt geometry and it use the geometry centroid point to create concentric ring buffers.
CREATE FUNCTION centroid_ring_buffers(
    minimum integer,
    maximum integer,
    step integer,
    size_ring integer,
    wkt_geom text,
    srid_geom integer    
) RETURNS integer AS $$
DECLARE
  quantity integer := 0;
BEGIN
 RAISE NOTICE 'Generating concentric ring buffers from % km to % km, size ring % km, % km steps at the point %.', minimum, maximum, step, size_ring, wkt_geom;
 INSERT INTO analysis.ring_buffer (latitude_center, longitude_center, outer_radius, inner_radius, geom) 
    (
     SELECT ST_Y(ST_CENTROID(ST_GeomFromText(wkt_geom,srid_geom))), ST_X(ST_CENTROID(ST_GeomFromText(wkt_geom,srid_geom))), minimum, 0, ST_Transform  
     (    
         ST_Buffer(ST_Transform(ST_CENTROID(ST_GeomFromText(wkt_geom,srid_geom)), 100002), (minimum * 1000), 'quad_segs=90'), 100002
     )::geometry(Polygon, 100002)  
   );
    
  FOR d IN (minimum + step + size_ring)..maximum BY (step + size_ring) LOOP
    INSERT INTO analysis.ring_buffer (latitude_center, longitude_center, outer_radius, inner_radius, geom) 
    (
     SELECT ST_Y(ST_CENTROID(ST_GeomFromText(wkt_geom,srid_geom))), ST_X(ST_CENTROID(ST_GeomFromText(wkt_geom,srid_geom))), d, d-size_ring, ST_Transform  
     (
       ST_Difference
       (
         ST_Buffer(ST_Transform(ST_CENTROID(ST_GeomFromText(wkt_geom,srid_geom)), 100002), (d * 1000), 'quad_segs=90'), 
         ST_Buffer(ST_Transform(ST_CENTROID(ST_GeomFromText(wkt_geom,srid_geom)), 100002), ((d * 1000) - (size_ring * 1000)), 'quad_segs=90')
       ), 100002
     )::geometry(Polygon, 100002)  
   );
    quantity := quantity + 1;
    RAISE NOTICE 'd % ', d;
    RAISE NOTICE 'step % ', step;
 END LOOP;
  
  RAISE NOTICE 'Generated % concentric ring buffers.', quantity;
  RETURN quantity;
END;
$$ LANGUAGE plpgsql;


-- Example of use the function centroid_ring_buffers(minimum, maximum, step, srid_geom, srid_rings, wkt_geom)
SELECT centroid_ring_buffers(30, 350, 15, 10, (SELECT ST_AsText(ST_CENTROID(geom))::text FROM bdc.bdc_sm_v2 AS grid WHERE tile='013014'), 100002)

DROP FUNCTION IF EXISTS rings_on_grid(grid_tile varchar(254));


CREATE OR REPLACE FUNCTION rings_on_grid(grid_tile varchar(254))
  RETURNS TABLE (gid_grid integer,
                 gid_ring integer,
                 outer_radius integer,
                 inner_radius integer,
                 geom geometry(MULTIPOLYGON, 100002)) AS $$
BEGIN
   RETURN QUERY
   SELECT ring.gid AS gid_ring, grid.gid AS gid_grid, ring.outer_radius AS outer_radius, ring.inner_radius AS inner_radius,
       ST_Intersection(grid.geom, ring.geom) AS geom
   FROM  analysis.ring_buffer AS ring, bdc.bdc_sm_v2 AS grid
   WHERE ST_Intersects(grid.geom, ring.geom) AND grid.tile = grid_tile;
END
$$ LANGUAGE plpgsql;

-- Example how to use the function "ring_of_grid"
SELECT * FROM rings_on_grid('013017');


DROP FUNCTION IF EXISTS rings_on_roi(roi geometry);

CREATE OR REPLACE FUNCTION rings_on_roi(roi geometry)
  RETURNS TABLE (gid_ring integer,
                 outer_radius integer,
                 inner_radius integer,
                 geom geometry(MULTIPOLYGON, 100002)) AS $$
BEGIN
   RETURN QUERY
   SELECT ring.gid AS gid_ring, ring.outer_radius AS outer_radius, ring.inner_radius AS inner_radius,
       ST_Intersection(ST_Transform(roi,100002), ring.geom) AS geom
   FROM  analysis.ring_buffer AS ring
   WHERE ST_Intersects(ST_Transform(roi,100002), ring.geom);
END
$$ LANGUAGE plpgsql;



DROP FUNCTION IF EXISTS grids_by_roi(roi geometry);
CREATE OR REPLACE FUNCTION grids_by_roi(roi geometry)
  RETURNS TABLE (gid integer,
                 id integer,
                 tile varchar(254),
                 geom geometry(MultiPolygon, 100001)) AS $$
BEGIN
   RETURN QUERY
   SELECT grid.gid AS gid, grid.id AS id, grid.tile AS tile,
       grid.geom AS geom
   FROM bdc.bdc_sm AS grid
   WHERE ST_INTERSECTS(grid.geom, ST_TRANSFORM(roi,100001));
END
$$ LANGUAGE plpgsql;

SELECT * FROM bdc.bdc_sm_v2
WHERE ST_INTERSECTS(bdc.bdc_sm_v2.geom, ST_TRANSFORM((SELECT geom FROM ibge.uf_2020 WHERE gid = 1)::geometry (MultiPolygon,4674),100002));



DROP FUNCTION IF EXISTS prodes_by_roi(roi geometry, roi_srid integer, year_target integer);
CREATE OR REPLACE FUNCTION prodes_by_roi(roi geometry, roi_srid integer, year_target integer)
  RETURNS TABLE (gid integer,
                 id integer,
                 origin_id integer,
                 state character varying(99),
                 path_row character varying(20),
                 main_class character varying(254),
                 class_name character varying(254),
                 def_cloud numeric,
                 julian_day integer,
                 image_date date,
                 year integer,
                 area_km numeric,
                 scene_id numeric,
                 source character varying(50),
                 satellite character varying(50),
                 sensor character varying(50),
                 geom geometry(Polygon,4674)) AS $$
DECLARE
   v_table text;
   wkt_geom_roi text;
BEGIN
   wkt_geom_roi := (SELECT ST_AsText(roi, roi_srid));
   FOR v_table IN
      SELECT table_name  
      FROM   information_schema.tables 
      WHERE  table_catalog = 'geodb' 
      AND    table_schema = 'prodes'
   LOOP
      RETURN QUERY EXECUTE format('SELECT prodes.gid AS gid, prodes.id AS id, prodes.origin_id AS origin_id, prodes.state AS state,
                             prodes.path_row AS path_row, prodes.main_class AS main_class, prodes.class_name AS class_name,
                             prodes.def_cloud AS def_cloud, prodes.julian_day AS julian_day, prodes.image_date AS image_date,
                             prodes.year AS year, prodes.area_km AS area_km, prodes.scene_id AS scene_id, prodes.source AS source,
                             prodes.satellite AS satellite, prodes.sensor AS sensor,
                             ST_TRANSFORM(ST_INTERSECTION(
                                 ST_BUFFER(prodes.geom::geometry(Polygon,4674), 0),
                                 ST_TRANSFORM(ST_GeomFromText(%L, %s), 4674)
                            ),100001)  AS geom
                   FROM prodes.%I AS prodes
                   WHERE ST_INTERSECTS(prodes.geom::geometry(Polygon,4674), ST_TRANSFORM(ST_GeomFromText(%L, %s),4674))
                                   AND prodes.year = %L::INTEGER;'  
                   ,wkt_geom_roi, roi_srid, v_table, wkt_geom_roi, roi_srid, year_target);
    END LOOP;
END
$$ LANGUAGE plpgsql;

SELECT * FROM prodes_by_roi((SELECT geom FROM bdc.bdc_sm_v2 WHERE bdc.bdc_sm_v2.gid = 223),100002);


DROP FUNCTION IF EXISTS prodes_tables();
CREATE FUNCTION prodes_tables()
    RETURNS TABLE(table_prodes text) 
    AS $$
BEGIN
    RETURN QUERY
    SELECT table_name::text AS table_prodes
    FROM   information_schema.tables 
    WHERE  table_catalog = 'geodb' 
           AND    table_schema = 'prodes';
END
$$ LANGUAGE plpgsql;


DROP FUNCTION IF EXISTS prodes_on_grid(grid_tile varchar(254), year_target integer);
CREATE OR REPLACE FUNCTION prodes_on_grid(grid_tile varchar(254), year_target integer)
  RETURNS TABLE (gid integer,
                 id integer,
                 origin_id integer,
                 state character varying(99),
                 path_row character varying(20),
                 main_class character varying(254),
                 class_name character varying(254),
                 def_cloud numeric,
                 julian_day integer,
                 image_date date,
                 year integer,
                 area_km numeric,
                 scene_id numeric,
                 source character varying(50),
                 satellite character varying(50),
                 sensor character varying(50),
                 geom geometry(Polygon,4674)) AS $$
DECLARE
   v_table text;
BEGIN
   FOR v_table IN
      SELECT table_name  
      FROM   information_schema.tables 
      WHERE  table_catalog = 'geodb' 
      AND    table_schema = 'prodes'
   LOOP
      RETURN QUERY EXECUTE format('SELECT prodes.gid AS gid, prodes.id AS id, prodes.origin_id AS origin_id, prodes.state AS state,
                             prodes.path_row AS path_row, prodes.main_class AS main_class, prodes.class_name AS class_name,
                             prodes.def_cloud AS def_cloud, prodes.julian_day AS julian_day, prodes.image_date AS image_date,
                             prodes.year AS year, prodes.area_km AS area_km, prodes.scene_id AS scene_id, prodes.source AS source,
                             prodes.satellite AS satellite, prodes.sensor AS sensor,
                             ST_INTERSECTION(
                                 ST_BUFFER(prodes.geom::geometry(Polygon,4674), 0),
                                 (SELECT ST_TRANSFORM(grid.geom, 4674) FROM bdc.bdc_sm_v2 AS grid WHERE grid.tile = %L)
                            ) AS geom
                   FROM prodes.%I AS prodes 
                   WHERE ST_INTERSECTS(prodes.geom::geometry(Polygon,4674), (SELECT ST_UNION(ST_TRANSFORM(geom, 4674)) FROM rings_on_grid(%L)))
                                   AND prodes.year = %L::INTEGER;'  
                   ,grid_tile, v_table, grid_tile, year_target);
    END LOOP;
END
$$ LANGUAGE plpgsql;

SELECT * FROM prodes_by_ring_grid('013017'); text ARRAYtext ARRAYRETURNS TABLE (
BEGIN
   FOR v_table IN
      RETURN QUERY     END LOOP;
END
$$ LANGUAGE plpgsql;

SELECT * FROM prodes_by_ring_grid('013017');

    
    
DROP FUNCTION IF EXISTS prodes_by_ring_grid(grid_tile text, table_label varchar(254), year_target integer);
CREATE OR REPLACE FUNCTION prodes_by_ring_grid(grid_tile text, table_label varchar(254), year_target integer)
  RETURNS TABLE (gid integer,
                 id integer,
                 origin_id integer,
                 state character varying(99),
                 path_row character varying(20),
                 main_class character varying(254),
                 class_name character varying(254),
                 def_cloud numeric,
                 julian_day integer,
                 image_date date,
                 year integer,
                 area_km numeric,
                 scene_id numeric,
                 source character varying(50),
                 satellite character varying(50),
                 sensor character varying(50),
                 geom geometry(Polygon,4674)) AS $$
BEGIN
    RETURN QUERY EXECUTE format('SELECT prodes.gid AS gid, prodes.id AS id, prodes.origin_id AS origin_id, prodes.state AS state,
                             prodes.path_row AS path_row, prodes.main_class AS main_class, prodes.class_name AS class_name,
                             prodes.def_cloud AS def_cloud, prodes.julian_day AS julian_day, prodes.image_date AS image_date,
                             prodes.year AS year, prodes.area_km AS area_km, prodes.scene_id AS scene_id, prodes.source AS source,
                             prodes.satellite AS satellite, prodes.sensor AS sensor,
                             ST_TRANSFORM(ST_INTERSECTION(
                                 ST_BUFFER(prodes.geom::geometry(Polygon,4674), 0),
                                 (SELECT ST_UNION(ST_TRANSFORM(geom, 4674)) FROM rings_on_grid(%L))
                            ), 100002) AS geom
                   FROM prodes.%I AS prodes
                   WHERE ST_INTERSECTS(prodes.geom::geometry(Polygon,4674), (SELECT ST_UNION(ST_TRANSFORM(geom, 4674)) FROM rings_on_grid(%L)))
                                AND prodes.year = %L::INTEGER
                                ORDER BY prodes.image_date;'  
                   ,grid_tile, table_label, grid_tile, year_target);
END
$$ LANGUAGE plpgsql;

SELECT * FROM prodes_by_ring_grid('013017', 'incremento_anual', 2018);
    

DROP FUNCTION IF EXISTS prodes_by_tile(grid_tile text, table_label varchar(254), year_target integer);
CREATE OR REPLACE FUNCTION prodes_by_tile(grid_tile text, table_label varchar(254), year_target integer)
  RETURNS TABLE (gid integer,
                 id integer,
                 origin_id integer,
                 state character varying(99),
                 path_row character varying(20),
                 main_class character varying(254),
                 class_name character varying(254),
                 def_cloud numeric,
                 julian_day integer,
                 image_date date,
                 year integer,
                 area_km numeric,
                 scene_id numeric,
                 source character varying(50),
                 satellite character varying(50),
                 sensor character varying(50),
                 geom geometry(Polygon,4674)) AS $$
BEGIN
      RETURN QUERY EXECUTE format('SELECT prodes.gid AS gid, prodes.id AS id, prodes.origin_id AS origin_id, prodes.state AS state,
                             prodes.path_row AS path_row, prodes.main_class AS main_class, prodes.class_name AS class_name,
                             prodes.def_cloud AS def_cloud, prodes.julian_day AS julian_day, prodes.image_date AS image_date,
                             prodes.year AS year, prodes.area_km AS area_km, prodes.scene_id AS scene_id, prodes.source AS source,
                             prodes.satellite AS satellite, prodes.sensor AS sensor,
                             ST_TRANSFORM(ST_INTERSECTION(
                                 ST_BUFFER(prodes.geom::geometry(Polygon,4674), 0),
                                 (SELECT ST_TRANSFORM(grid.geom, 4674) FROM bdc.bdc_sm AS grid WHERE grid.tile = %L)
                            ), 100001) AS geom
                   FROM prodes.%I AS prodes 
                   WHERE ST_INTERSECTS(prodes.geom::geometry(Polygon,4674), (SELECT ST_TRANSFORM(grid.geom, 4674) FROM bdc.bdc_sm AS grid WHERE grid.tile = %L))
                                   AND prodes.year = %L::INTEGER;'  
                   ,grid_tile, table_label, grid_tile, year_target);
END
$$ LANGUAGE plpgsql;