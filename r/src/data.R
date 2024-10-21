library(dplyr)
library(sf)

# =============== Functions ====================================================

# The follow functions used to prepare and manipulate the data to used in the 
# other functions.


# This function allows filter a time series data set ("S2-SEN2COR_10_16D_STK-1")
# based in a specific date, label, tile, value, at NDVI band. The objective is 
# to search for possible outliers in data set. 
filter_vband <- function(input_data,
                         obs_date,
                         min_value,
                         max_value,
                         label_target,
                         tile_target){
  pre_filter <- filter(input_data, label == label_target, tile == tile_target)
  
  if(obs_date %in% input_data$time_series[[1]]$Index){
    filtered <- filter(pre_filter, unlist(
      lapply(pre_filter$time_series, 
             function(x) 
               (any(  x$Index == as.Date(obs_date) &
                        x$NDVI > min_value & 
                        x$NDVI < max_value)))))
      print(length(filtered$label))
      if(length(filtered$label) > 0){
      return(filtered)  
    }
  }
  return(input_data)
}
# Function that realize a subset of a time-series tibble. It split the time-series
# considering a start and end dates generating a new data.
subset_by_date <- function(input_data, start_date_, end_date_) {
  start_pos <- which(input_data$time_series[[1]]$Index == start_date_)
  end_pos <- which(input_data$time_series[[1]]$Index == end_date_)
  
  return(
    input_data %>%
      dplyr::mutate(start_date = start_date_, end_date = end_date_, time_series
                    = lapply(input_data$time_series, '[', start_pos:end_pos,)) %>%
      dplyr::select(latitude,longitude,start_date,
                    end_date,label,cube, tile,time_series)
  )
}

# Function that realize a filter using a minimum and maximum limit of the band
# value.
filter_band <- function(input_data, obs_date, min_value,
                        max_value, label_target){
  pre_filter <- filter(input_data, label == label_target)
  return(
    filter(pre_filter, unlist(
      lapply(pre_filter$time_series, 
             function(x) 
               (any(  x$Index == as.Date(obs_date) &
                        x$NDVI > min_value & 
                        x$NDVI < max_value))))))
  
}


find_outiers <- function(samples){
  forest_samples <- samples[samples$label == "Forest",]
  ts <- forest_samples$time_series
  dt <- data.table::data.table(dplyr::bind_rows(ts))
  f_summary <- summary(dt)
  
} 

# Example: a<-find_outliers(input_data.tb, "2018-06-26", -0.81, 0.1, Desmatamento, 079093)

# This function adds the tile column in a S2-SEN2COR_10_16D_STK-1 cube data set.
add_tile <- function(data, shapefile){
  nc <- st_read(shapefile)
  print("points")
  points <- mapply(function(x,y) st_point(c(x,y), 4326),
                   data$longitude,data$latitude,SIMPLIFY = FALSE)
  print("lp")
  lp <- lapply(points, st_within, nc, sparse=FALSE)
  print("wlp")
  wlp <- lapply(lp, which)
  print("ltiles")
  ltiles <- lapply(wlp, function(s) nc$tile[s])
  tiles <- rapply(ltiles, function(x) head(x, 1))
  return(data %>% mutate(tile=tiles))
  
}
add_tile_2 <- function(data.df, shapefile){
  nc <- st_read(shapefile)
  points <- mapply(function(x,y) st_point(c(x,y), 4326),
                   data.df$longitude,data.df$latitude,SIMPLIFY = FALSE)
  points <- st_sfc(points, crs=4326)
  
  return(data.df %>% mutate(tile = case_when(
    (st_within(points,nc[nc$tile == "012014",],sparse = FALSE)) ~ "012014",
    (st_within(points,nc[nc$tile == "012015",],sparse = FALSE)) ~ "012015",
    (st_within(points,nc[nc$tile == "012016",],sparse = FALSE)) ~ "012016",
    (st_within(points,nc[nc$tile == "013014",],sparse = FALSE)) ~ "013014",
    (st_within(points,nc[nc$tile == "013015",],sparse = FALSE)) ~ "013015",
    (st_within(points,nc[nc$tile == "013016",],sparse = FALSE)) ~ "013016",
    (st_within(points,nc[nc$tile == "014014",],sparse = FALSE)) ~ "014014",
    (st_within(points,nc[nc$tile == "014015",],sparse = FALSE)) ~ "014015",
    (st_within(points,nc[nc$tile == "014016",],sparse = FALSE)) ~ "014016"
    
  )))
  
}

within_pos <- function(point, geom){
  lp <- lapply(point, st_within, geom, sparse=FALSE)
  
}

add_tile_1 <- function(data.df) {
  return(
    data.df %>% mutate(tile =
                         case_when(
                           (latitude <= -8.968547882953816 & latitude >= -10.003125268820506) & (longitude <= -63.645285163429676 & longitude >= -65.23432383171814) ~ "077094", 
                           (latitude <= -9.947158110497963 & latitude >= -10.981203537048179) & (longitude <= -63.680330341114505 & longitude >= -65.27527994811312) ~ "077095",
                           (latitude <= -7.988961804961198 & latitude >= -9.024359680174772) & (longitude <= -63.610492715308375 & longitude >= -65.19366401670857) ~ "077093", 
                           (latitude <= -8.04463477904458 & latitude >= -9.071876595685131) & (longitude <= -62.067103113531694 & longitude >= -63.645285163429676) ~ "078093", 
                           (latitude <= -8.092032760400222 & latitude >= -9.111095461705832) & (longitude <= -60.52322216034859 & longitude >= -62.09631492244112) ~ "079093", 
                           (latitude <= -9.024359680174772 & latitude >= -10.05077521190122) & (longitude <= -62.09631492244112 & longitude >= -63.68033034111452) ~ "078094", 
                           (latitude <= -10.003125268820506 & latitude >= -11.029000806386007) & (longitude <= -62.12573899841475 & longitude >= -63.71563101099978) ~ "078095", 
                           (latitude <= -9.071876595685131 & latitude >= -10.090104398980037) & (longitude <= -60.546847976099244 & longitude >= -62.12573899841475) ~ "079094", 
                           (latitude <= -10.05077521190122 & latitude >= -11.068452127014156) & (longitude <= -60.57064551836915 & longitude >= -62.15537766288313) ~ "079095")
    ))
}

# This function select a specific image in sits cube
select_raster <- function(cube, date_view, tile_){
  return(cube %>% 
           dplyr::filter(tile == tile_) %>% 
           mutate(file_info = lapply(file_info, 
                                     function(x) filter(x, date == date_view))))
  
}

download_raster <- function(cube, date_, bands_, tile_, dir) {
  new_path <- paste(dir, tile_, sep = "")
  if (!dir.exists(new_path)){
    dir.create(new_path)
    print(new_path)
  }
  
  file_info_tile <-cube[cube$tile == tile_,]$file_info[[1]]
  
  for (b in bands_){
    path <- filter(file_info_tile, date==date_, band==b)$path
    #print(path)
    url <- strsplit(path, "l/")[[1]][2] 
    splited_url <- strsplit(url, "/")
    name_file <- tail(splited_url[[1]], n=1)
    name_file <- strsplit(name_file, ".tif")[[1]][1]
    name_file <- paste(name_file, ".tif", sep="")
    path_file <- paste(new_path, name_file, sep="/")
    print(path_file)
    print(url)
    download.file(url, path_file)
  }
}

filter_NDVI <- function(sent_cube, samples){
  
  tiles <- unique(samples$tile)
  
  input_all_filtered <- samples[0, ] 
  
  for (tile in tiles){
    print(tile)
    row <- dplyr::filter(sent_cube, tile == tile)
    dates <-  (row$file_info[[1]]$date[row$file_info[[1]]$cloud_cover < 5.0 & row$file_info[[1]]$band == "NDVI"])
    dates <- dates[dates %in% samples$time_series[[1]]$Index]
    
    input_forest <- filter_vband(samples, dates[1], -1, 1,"Forest", tile)
    input_deforestation <- filter_vband(samples, dates[1], -1, 0.865,"Deforested Areas", tile)
    for (i in 2:length(dates)){
      input_forest <- filter_vband(input_forest, dates[i], -1, 1,"Forest", tile)
      
      input_deforestation <- filter_vband(input_deforestation, dates[i], -1, 0.865,"Deforested Areas", tile)
    } 
    input_all_filtered <- rbind(input_all_filtered, input_forest)
    input_all_filtered <- rbind(input_all_filtered, input_deforestation)
  }
  return(input_all_filtered)
}


