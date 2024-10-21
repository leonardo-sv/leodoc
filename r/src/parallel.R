


# ========================= Parallel ===========================================

block_as_value <- function(r_obj, block){
  terra::readStart(x = r_obj)
  values <- terra::readValues(
    x = r_obj,
    row = block[["row"]],
    nrows = block[["nrows"]],
    col = block[["col"]],
    ncols = block[["ncols"]],
    mat = TRUE
  )
  terra::readStop(x = r_obj)
  return(values)
}



chunk_as_raster <- function(chunk, nlayers_){

  if(is.character(chunk[["crs"]])){
    crs <- chunk[["crs"]]
  }
  else{
    crs <- paste0("EPSG:", chunk["crs"]) 
  }

  return(terra::rast(
    nrows = as.integer(chunk["nrows"]),
    ncols = as.integer(chunk["ncols"]),
    xmin = as.integer(chunk["xmin"]),
    xmax = as.integer(chunk["xmax"]),
    ymin = as.integer(chunk["ymin"]),
    ymax = as.integer(chunk["ymax"]),
    nlyrs = nlayers_,
    crs = crs))
}

raster_crop_metadata <- function(r_obj,block) {
  
  xmin <- terra::xFromCol(
    object = r_obj,
    col = block[["col"]]
  )
  xmax <- terra::xFromCol(
    object = r_obj,
    col = block[["col"]] + block[["ncols"]] - 1
  )
  ymax <- terra::yFromRow(
    object = r_obj,
    row = block[["row"]]
  )
  ymin <- terra::yFromRow(
    object = r_obj,
    row = block[["row"]] + block[["nrows"]] - 1
    )

  extent <- terra::ext(x = c(xmin, xmax, ymin, ymax))
  
  # crop raster
  suppressWarnings(
    terra::crop(x = r_obj, y = extent, snap = "out")
  )
}

image_as_r_obj <- function(image){
  image_bbox <- terra::ext(image)[]
  image_bbox['crs'] <- terra::crs(image)
  image_size <- c(ncols = terra::nrow(image), nrows = terra::nrow(image))
  entire_image <- c(image_size, image_bbox)
  n_lyr <- terra::nlyr(cloud_images)
  
  t_obj <- chunk_as_raster(chunk = entire_image, nlayers = n_lyr)
  return(t_obj)
}

create_chuncks <- function(image, block = c(ncols = 528, nrows = 528)){
  image_bbox <- terra::ext(image)[]
  image_bbox['crs'] <- terra::crs(image)
  image_size <- c(ncols = terra::nrow(image), nrows = terra::nrow(image))
  
  chunks <- tidyr::expand_grid(
    col = seq(1, image_size['ncols'], block['ncols']),
    row = seq(1, image_size['nrows'], block['nrows'])
  )
  
  chunks[["nrows"]] <- as.integer(pmin(as.integer(image_size['nrows']), chunks[['row']] +
                                         as.integer(block['nrows'])) -  chunks[['row']]) 
  chunks[["ncols"]] <- as.integer(pmin(as.integer(image_size['ncols']), chunks[['col']] +
                                         as.integer(block['ncols'])) -  chunks[['col']])
  
  entire_image <- c(image_size, image_bbox)
  
  t_obj <- chunk_as_raster(chunk = entire_image, nlayers = 1)
  
  chunks <- slider::slide_dfr(chunks, function(chunk) {
    # Crop block from template
    block_to_crop <- tibble::tibble(col = as.integer(chunk['col']),
                                    row = as.integer(chunk['row']),
                                    ncols = as.integer(chunk['ncols']),
                                    nrows = as.integer(chunk['nrows']))
    r_obj <- raster_crop_metadata(r_obj = t_obj, block = block_to_crop)
    # Add bbox information
    extension <- terra::ext(r_obj)[]
    chunk[["xmin"]] <- as.integer(extension["xmin"])
    chunk[["xmax"]] <- as.integer(extension["xmax"])
    chunk[["ymin"]] <- as.integer(extension["ymin"])
    chunk[["ymax"]] <- as.integer(extension["ymax"])
    chunk[["crs"]]<- terra::crs(r_obj)
    chunk
  })
  
  return(chunks)
    
}

path_bimages <- function(fi, chr_band){
  return(filter(fi, band == chr_band) %>% arrange(date))
}

cloud_mask <- function(values){
  yaml_user_config <- yaml.load_file(Sys.getenv("SITS_CONFIG_USER_FILE"))
  
  cloud_cfg <- 
    yaml_user_config$sources$BDC$collections$`S2-SEN2COR_10_16D_STK-1`$bands$CLOUD$interp_values
  return(values %in% cloud_cfg)
}

raster_values <- function(rasters_ref, start_row, n_rows, start_col, n_col){
  return(terra::values(rasters_ref,
                row=start_row,
                nrows=n_rows,
                col=start_col,
                ncols=n_col,
                mat=TRUE))
}

interpolation_cloud <- function(cube, tile, bands){
  tile_cube <- cube[cube$tile == tile]
  fi <- tile_cube$file_info[[1]]
  cloud_img_paths <- path_bimages(fi, "CLOUD")
  cloud_images <- terra::rast(cloud_img_paths$path)
  
  images <- purrr::map(bands, function(b){
    band_img_paths <- path_bimages(fi, b)
    images <- terra::rast(band_img_paths$path)
    return(images)
  })
  
  v<- purrr::map(lvalues, function(l){
    do.call(cbind,l)
  })
  
  cloud <- raster_values(cloud_images, 1,2,1,1)
  i <- cloud_mask(cloud)
  
  values <- lapply(images, function(image){
    v <- raster_values(image, 1,2,1,1)
    v[i] <- NA
    interpolated_values <- sits_impute_linear(v)
    print(interpolated_values)
    return(interpolated_values)
    } )
  return(values)
}



# ========================== Machine Learning ==================================

path_classification <- function(path, block){
  file_path <- paste(path, block$xmin, sep = "/")
  file_path <- paste(file_path, block$xmax, sep = "_")
  file_path <- paste(file_path, block$ymin, sep = "_")
  file_path <- paste(file_path, block$ymax, sep = "_")
  file_path <- paste(file_path, "tif", sep = ".")
  return(file_path)
}

band_unlist <- function(blist, n_col){
  print(blist)
  return(matrix(unlist(blist), ncol=n_col))
}


ts_values <- function(images, block, mask){
  lvalues <- purrr::map(images, function(i){
    images_values <- block_as_value(i, block)
    images_values[mask] <- NA
    interpolated_values <- sits_impute_linear(images_values)
    interpolated_values <- interpolated_values/10000
    values <- as.matrix(interpolated_values)
    return(tslist(values, simplify = TRUE))
  })
  values <-lvalues[[1]]
  if(length(lvalues) > 1){
    for(i in 2:length(lvalues)){
      for(j in 1:length(lvalues[[1]])){
        values[[j]] <- cbind(values[[j]], lvalues[[i]][[j]])
      }  
    }
  }
  return(values)
}

predict_parallel <- function(cube,
                             model,
                             tile, 
                             bands, 
                             block_size = c(ncols = 1056, nrows = 1056),
                             path){
  
  # Create dir based in tile and set paths
  dir.create(file.path(path, tile), showWarnings = FALSE)
  file_path <- paste(path, tile, sep ="/")
  
  dir.create(file.path(file_path, "tmp"), showWarnings = FALSE)
  tmp_path <- paste(file_path, "tmp", sep ="/")
  
  tile_cube <- cube[cube$tile == tile,]
  
  fi <- tile_cube$file_info[[1]]
  
  images <- purrr::map(bands, function(band){
    image_paths <- path_bimages(fi, band)
    return(terra::rast(image_paths$path))
  })

  cloud_img_paths <- path_bimages(fi, "CLOUD")
  cloud_images <- terra::rast(cloud_img_paths$path)

  template_image <- images[[1]]
  chuncks <- create_chuncks(image=template_image, block_size)


  # Open log
  # tmp <- file.path(tempdir(), "test.log")
  # 
  # # Open log
  # lf <- log_open(tmp)
  # parallel::mclapply(1:nrow(chuncks), mc.cores = 14, function(pos_block){
  lapply(1:nrow(chuncks), function(pos_block){
    # log_print(pos_block)
    block <- chuncks[pos_block,]
    cloud_block_values <- block_as_value(cloud_images, block)
    mask <- cloud_mask(cloud_block_values)


    values <- ts_values(images,block, mask)
    predicted <- dtwclust::predict(model, values)
    r_obj <- chunk_as_raster(block,1)

    terra::values(x = r_obj) <- as.matrix(predicted)
    f <- path_classification(tmp_path, chuncks[pos_block,])
    terra::writeRaster(r_obj, f, overwrite=TRUE)
    terra::readStop(x = r_obj)

  })

  tmp_files <- list.files(tmp_path)
  files <- paste(tmp_path,tmp_files, sep="/")
  out_file <- paste(file_path, tile, sep="/")
  out_file <- paste(out_file, "tif", sep=".")
  
  gdalUtilities::gdalwarp(
    srcfile = files,
    dstfile = out_file,
    #wo = paste0("NUM_THREADS=", multicores),
    #multi = TRUE,
    #q = TRUE,
    overwrite = FALSE
  )
}

cloud_percent <- function(cube, band_query){
  lcloud_cover = tibble::tibble(fid = character(),
                                date =as.Date(character()),
                                cloud_cover = numeric()
                                )
  
  for(tile in cube$tile){

    file_info <- cube[cube$tile == tile,]$file_info[[1]] %>%
      filter(band == band_query) %>%
      select(c("fid", "date", "cloud_cover")) 

    lcloud_cover <- rbind(lcloud_cover,file_info)
  }
  
  return(lcloud_cover)
  
}

