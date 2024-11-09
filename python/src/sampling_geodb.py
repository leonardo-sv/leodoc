from datetime import datetime
import geopandas as gpd
import json
import matplotlib
import numpy as np
import pandas as pd
import pyproj
import psycopg2
import rasterio
# import stac
from typing import List, Dict
from matplotlib import pyplot as plt
from osgeo import gdal
from rasterio import features
from shapely import wkt
from matplotlib import pyplot
from rasterio.mask import mask
from pyproj import Transformer


# Handle the Vectorial data
crs_bdc = pyproj.CRS("+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs")

def rule_5x5(xs: List[int], ys: List[int], image_data: np.array):
    rows, cols = image_data.shape
    mask = []
    for x, y in zip(xs, ys):
        if (x not in [0, 1, rows - 1, rows - 2]) & (y not in [0, 1, cols - 1, cols - 2]):
            flag =  np.all(image_data[x-2:x+3, y-2:y+3] == image_data[x, y])
        else:
            flag = False
        mask.append(flag)
    return mask

def sampling(raster_path: str, label: int, start_date: str, end_date:str, method="all", rule_test=False, n_samples=0):
    map_raster = rasterio.open(raster_path)
    map_array = map_raster.read(1)
    xy_samples = np.where(map_array == label)
    idx = None
    if method == "random":
        np.random.seed(42)
        idx = np.random.choice(xy_samples[0].size, n_samples, replace=True)
    elif method == "all":
        idx = np.arange(0, len(xy_samples[0]), 1)

    if rule_test:
        mask = rule_5x5(xy_samples[0][idx], xy_samples[1][idx], map_array)
        idx = idx[mask]

    rows = xy_samples[0][idx]
    cols = xy_samples[1][idx]

    xs, ys = rasterio.transform.xy(map_raster.transform, rows, cols, offset='center')#4326
    points = []
    latitude = []
    longitude = []
    
    transformer = Transformer.from_crs(crs_bdc, 4676)
    
    for c1, c2 in zip(xs, ys):
        points.append((c1, c2))
    
    for pt in transformer.itransform(points): 
        latitude.append(pt[0])
        longitude.append(pt[1])
    
    _df = pd.DataFrame()
    _df['longitude'] = [long for long in longitude]
    _df['latitude'] = [lat for lat in latitude]
    _df['start_date'] = start_date
    _df['end_date'] = end_date
    _df['label'] =  "Forest" if label == 0 else "Deforest Area"

    return _df


def get_bbox(tile):
    dic_tile = {'077095': '-65.2, -10.0, -63.86, -10.86',
                '078095': '-63.55, -10.06, -62.3, -10.83',
                '079095': '-62.05, -10.01, -60.76, -10.92',
                '077094': '-65.0, -9.0, -63.8, -9.8',
                '078094': '-63.5, -9.1, -62.35, -9.87',
                '079094': '-60.54, -9.07, -62.12, -10.09',
                '077093': '-65.06, -8.04, -63.8, -8.84',
                '078093': '-63.5, -8.12, -62.29, -8.87',
                '079093': '-61.96, -8.2, -60.68, -9.0'}
    return dic_tile[tile]



#Return a connection of geodb database
def conn_db(json_pass):
    with open(json_pass) as json_file:
        my_pass = json.load(json_file)

        conn = psycopg2.connect(
            user=my_pass['USER'],
            password=my_pass['PASS'],
            database=my_pass['DB'],
            host=my_pass['HOST'],
            port=int(my_pass['PORT'])
        )

    return conn

def concentric_rings(minimum: int, maximum: int, step: int, size_ring:int, wkt_point:str, srid_geom: int, connection: psycopg2.connect):
    cur = connection.cursor()
    sql = "SELECT centroid_ring_buffers({}, {}, {}, {}, \'{}\', {})".format(minimum, maximum, step, size_ring, wkt_point, srid_geom)
    cur.execute(sql)
    return cur.fetchone()

def get_concentric_rings(connection: psycopg2.connect):
    sql = "SELECT * FROM analysis.ring_buffer; "
    return gpd.read_postgis(sql=sql, crs = 4674, con=connection)

def grids_by_roi(wkt_geom:str, crs_geom: int, connection: psycopg2.connect):
    sql_params = [crs_geom, wkt_geom, crs_geom]
    
    sql = "SELECT gid, id, tile, ST_TRANSFORM(geom, {}) AS geom FROM grids_by_roi(ST_GeomFromText(\'{}\',{}))".format(*sql_params)
    if(crs_geom == 100002):
        crs_geom = crs_bdc
    return gpd.read_postgis(sql=sql, crs=crs_geom, con=connection)
    

def rings_on_grid(tile: str, connection: psycopg2.connect):
    sql = "SELECT * FROM rings_on_grid(\'{}\')".format(tile)
    return gpd.read_postgis(sql=sql, crs=crs_bdc, con=connection)

def prodes_on_ring(tile: str, table: str, year: int, connection: psycopg2.connect):
    sql = "SELECT * FROM prodes_by_ring_grid(\'{}\', \'{}\', {})".format(tile, table, year)
    
    return gpd.read_postgis(sql=sql, crs=crs_bdc, con=connection)

def prodes_by_roi(wkt_geom: str, srid: int, year: int, connection: psycopg2.connect):
    sql = "SELECT * FROM prodes_by_roi(ST_Transform(ST_GeomFromText(\'{}\', {}), 4674), 4674, {})".format(wkt_geom, srid, year)
    return gpd.read_postgis(sql=sql, crs=crs_bdc, con=connection)

def prodes_on_grid(tile: str, year: int, connection: psycopg2.connect):
    sql = "SELECT * FROM prodes_on_grid(\'{}\', {})".format(tile, year)
    return gpd.read_postgis(sql=sql, crs=4674, con=connection)


def rings_on_roi(wkt_geom:str, srid: int, connection: psycopg2.connect):
    sql = "SELECT * FROM rings_on_roi(ST_GeomFromText(\'{}\', {}))".format(wkt_geom, srid)
    return gpd.read_postgis(sql=sql, crs=crs_bdc, con=connection)

def getFeatures(gdf):
    """Function to parse features from GeoDataFrame in such a manner that rasterio wants them"""
    import json
    return [json.loads(gdf.to_json())['features'][0]['geometry']]


# Use the parameters url, token and collection_name to obtain the BDC collection data
def get_bdc_collection(url, token, collection_name):
    bdc_stac_service = stac.STAC(url, access_token=token)
    collection = bdc_stac_service.collection(collection_name)

    return collection

#Parse the items from collection to obtain the raster data
def get_items(collection, **kwargs):
    with open('../data/json/my_pass.json') as json_file:
        my_pass = json.load(json_file)
    
    access_token = my_pass["STAC_TOKEN"]
    
    items = collection.get_items(**kwargs)
    
    for i in range(0, len(items.features)):
        item = items.features[i]
        
        for j in item["assets"].keys():
            items.features[i]["assets"][j]["href"] += f"?access_token={access_token}"
    return items


# Use a gpd to obtain a raster data considering the classes of vectorial data
def rasterize_gdf(geom_gdf: gpd.GeoDataFrame, column_classes: str, path_raster_ref: str,
                  path_raster_output: str) -> np.array:
    """Create a raster data from GeoDataFrame based in column classes
            Args:
                geom_gdf (gpd.GeoDataFrame): GeoDataFrame with labeled polygon

                column_classes (str): GeoDataFrame column with value to create a raster

                path_raster_ref (str): Image path used as a reference

                path_raster_output (str): Image path used to save a raster based in geom_gdf
            Returns:
                burned (np.array): numpy array with data saved in raster
            """
    raster_ref = rasterio.open(path_raster_ref)
    meta = raster_ref.meta.copy()
    meta.update(compress='lzw')

    with rasterio.open(path_raster_output, 'w+', **meta) as out:
        out_arr = out.read(1)

        shapes = ((geom, value) for geom, value in zip(geom_gdf.geometry, geom_gdf[column_classes]))

        burned = features.rasterize(shapes=shapes, fill=0, out=out_arr, transform=out.transform)
        
        out.write_band(1, burned)
        
# This function define a rule considering a 3x3 grid to apply in the sampling approach. 
# Return TRUE when all pixels in 3x3 neighborhood have the same class.
# Return FALSE if at least one pixel class is different to others.
def rule_3x3(xs: List[int], ys: List[int], image_data: np.array):
    rows, cols = image_data.shape
    mask = []
    for x, y in zip(xs, ys):
        if (x not in [0, 1, rows - 1, rows - 2]) & (y not in [0, 1, cols - 1, cols - 2]):
            flag = np.all(image_data[x - 1: x + 2, y - 1: y + 2] == image_data[x, y])
        else:
            flag = False
        mask.append(flag)
    return mask

# This function define a rule considering a 5x5 grid to apply in the sampling approach. 
# Return TRUE when all pixels in 5x5 neighborhood have the same class.
# Return FALSE if at least one pixel class is different to others.
def rule_5x5(xs: List[int], ys: List[int], image_data: np.array):
    rows, cols = image_data.shape
    mask = []
    for x, y in zip(xs, ys):
        if (x not in [0, 1, rows - 1, rows - 2]) & (y not in [0, 1, cols - 1, cols - 2]):
            flag =  np.all(image_data[x-2:x+3, y-2:y+3] == image_data[x, y])
        else:
            flag = False
        mask.append(flag)
    return mask

# Use the raster map to sort coordinates of samples.
def coord_samples(raster_path: str, label: int, method="all", rule_test=False, n_samples=0):
    map_raster = rasterio.open(raster_path)
    map_array = map_raster.read(1)
    xy_samples = np.where(map_array == label)
    idx = None
    if method == "random":
        np.random.seed(42)
        idx = np.random.choice(xy_samples[0].size, n_samples, replace=False)
    elif method == "all":
        idx = np.arange(0, len(xy_samples[0]), 1)

    if rule_test:
        mask = rule_5x5(xy_samples[0][idx], xy_samples[1][idx], map_array)
        idx = idx[mask]

    rows = xy_samples[0][idx]
    cols = xy_samples[1][idx]

    xs, ys = rasterio.transform.xy(map_raster.transform, rows, cols, offset='center')#4326

    return xs, ys

# Use a list of coordinates to extract time series from data cube.
def ts_extract(stac_features: List[dict], bands: List[str], #joblib
               coords: np.ndarray, label: str, label_2: str) -> pd.DataFrame:
    """Extract Timeseries from STAC Features
    Args:
        stac_feature (List[Dict]): Features extracted from STAC-API

        band_name (str): Band to extract value

        coords (List[list]): List of positions (In native CRS) where data is from

        use_matrix_colrow (bool): Use matrix col and row position to get timseries
    Returns:
        pd.Series: Values extracted
        :param coords:
        :param label:
        :param cube:
        :param bands:
        :param stac_features:
    """
    time_index = []
    time_series_per_band = {}
    print(label)
    for band in bands:
        time_series_per_band[band] = []
        for stac_feature in reversed(stac_features):
            _ds = rasterio.open(stac_feature['assets'][band]['href'])

            time_series_per_band[band].append(np.concatenate(list(_ds.sample(coords)), axis=0))
            time_index.append((stac_feature['properties']['datetime']))
        # time_series_per_band[band] = np.stack(time_series_per_band[band], axis=1)
    dates = pd.DatetimeIndex(time_index).unique().strftime('%Y-%m-%d').tolist()
    print(dates)
    _df = pd.DataFrame()
    _df['Coord1'] = [item[0] for item in coords]
    _df['Coord2'] = [item[1] for item in coords]
    _df['start_date'] = dates[0]
    _df['end_date'] = dates[-1]
    _df['label'] = label
    _df['label_raster'] = label_2
    for band in bands:
        for i in range(len(time_series_per_band[band])):
            df_name = band + "_t" + str(i)
            _df[df_name] = time_series_per_band[band][i]
    return _df

# Use a raster reference to apply the sampling approach rule and obtain the samples
def sample_ts(raster_ref, items, labels, bands, n_samples, rule, output):

    raster_ref = '../data/raster/LC8_30_PRODES_2017_2018_porto_velho.tif'
    labels = {175:"Desmatamento", 176:"Floresta"}

    frames = []
    for id_label in labels.keys():
        xs, ys = coord_samples(raster_ref, id_label, "random", rule, n_samples)
        xy = np.stack((xs, ys), axis=1)
        samples = ts_extract(items["features"], bands, xy, labels[id_label], str(id_label))
        frames.append(samples)
    total_samples = pd.concat(frames, ignore_index=True)
    total_samples.to_csv(output, index=False)
    return total_samples

