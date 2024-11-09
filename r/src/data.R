library(dplyr)
library(sf)

# =============== Functions ====================================================

# The follow functions used to prepare and manipulate the data to used in the 
# other functions.
within_pos <- function(point, geom){
  lp <- lapply(point, sf::st_within, geom, sparse=FALSE)
  
}

# This function adds the tile column in a cube data set.
add_tile <- function(data, shapefile){
  nc <- sf::st_read(shapefile)
  print("points")
  points <- mapply(function(x,y) sf::st_point(c(x,y), 4326),
                   data$longitude,data$latitude,SIMPLIFY = FALSE)
  print("lp")
  lp <- lapply(points, sf::st_within, nc, sparse=FALSE)
  print("wlp")
  wlp <- lapply(lp, which)
  print("ltiles")
  ltiles <- lapply(wlp, function(s) nc$tile[s])
  tiles <- rapply(ltiles, function(x) head(x, 1))
  return(data %>% mutate(tile=tiles))
  
}


# This function allows filter a time series data set
# based in a specific date, label, tile, value, at NDVI band. The objective is 
# to search for possible outliers in data set. 
filter_NDVI <- function(input_data,
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

# This function select a specific image in sits cube
select_raster <- function(cube, date_view, tile_){
  return(cube %>% 
           dplyr::filter(tile == tile_) %>% 
           dplyr::mutate(file_info = lapply(file_info, 
                                     function(x) dplyr::filter(x, date == date_view))))
  
}

download_raster <- function(cube, date_, bands_, tile_, dir) {
  new_path <- paste(dir, tile_, sep = "")
  if (!dir.exists(new_path)){
    dir.create(new_path)
    print(new_path)
  }
  
  file_info_tile <-cube[cube$tile == tile_,]$file_info[[1]]
  print(bands_)
  for (b in bands_){
    print(b)
    path <- dplyr::filter(file_info_tile, date==date_, band==b)$path
    url <- strsplit(path, "l/")[[1]][2] 
    splited_url <- strsplit(url, "/")
    name_file <- tail(splited_url[[1]], n=1)
    name_file <- strsplit(name_file, ".tif")[[1]][1]
    name_file <- paste(name_file, ".tif", sep="")
    path_file <- paste(new_path, name_file, sep="/")
    download.file(url, path_file)
  }
}

apply_water_mask <- function(cube, file_path, output_dir){
  
  water_value = 6
  
  
  splited_path <- strsplit(file_path, "/")[[1]]
  file <- splited_path[length(splited_path)]
  file_no_extension <- strsplit(file, '[.]')[[1]][1]
  splited_file <- strsplit(file_no_extension,"_")[[1]]
  last_date <- splited_file[5]
  tile <- splited_file[3]
  dates <- cube$file_info[[1]]$date
  
  file_info <- cube[cube$tile == tile,]$file_info[[1]]
  last_SLC <- dplyr::filter(file_info,
                            date == last_date,
                            band=="CLOUD")
  
  
  i <- length(dates)
  date_used <- last_date
  
  while(last_SLC$cloud_cover >= 1.0){
    
    if (i == 23) {
      last_SLC <- dplyr::filter(file_info,
                                date == last_date,
                                band=="CLOUD")
      date_used <- last_date
      break
    }
    
    last_SLC <- dplyr::filter(file_info,
                              date == dates[length(dates) - i],
                              band=="CLOUD")
    
    date_used <- dates[length(dates) - i]
    
    i <- i + 1
    
  }
  
  path <- last_SLC$path
  
  class_img <- terra::rast(file_path)
  class_img_values <- terra::values(class_img)

  cloud_img <- terra::rast(path)
  cloud_img_values <- terra::values(cloud_img)

  class_img_values[cloud_img_values == water_value] <- water_value
  terra::values(class_img) <- class_img_values

  f <- paste(file_no_extension, "water_mask", sep="_")
  f<- paste(f, date_used, sep="_")
  f<- paste(f, "tif", sep=".")
  f <- paste(output_dir, f, sep="/")
  
  terra::writeRaster(class_img, f, overwrite=TRUE)

}


filter_outliers <- function(sent_cube, samples){
  
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

to_qgis <- function(samples_predicted){
  return(samples_predicted %>% 
           mutate(
             label_compare = 
               case_when(
                 (cluster_labeled == label)~cluster_labeled,
                 (cluster_labeled != label)&cluster_labeled=="Forest" ~ "FN",
                 (cluster_labeled != label)&cluster_labeled=="Deforestation"~"FP"
               )
           )
  )
}
