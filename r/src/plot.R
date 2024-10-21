



plot_box <- function(dt){
  
  plot.df <- purrr::pmap_dfr(
    list(dt$label, dt$time_series),
    function(label, ts) {
      lb <- as.character(label)
      # extract the time series and convert
      df <- tibble::tibble(Time = ts$Index, ts[-1], Band = lb)
      return(df)
    }
  )
  
  
  plot.df <- tidyr::pivot_longer(plot.df, cols = sits_bands(dt))
  
  gp <- ggplot(plot.df, aes(x=name, y=value, color=name)) +
        geom_boxplot()  + 
        ggplot2::facet_wrap(~Band) +
        theme(text = element_text(size=12,family="Roboto"),
              title = element_text(size=12,family="Roboto"),
              axis.text=element_text(size=11, family="Roboto")) +
        ggplot2::guides(colour = ggplot2::guide_legend(title = "Bands")) +
        ggplot2::ylab("Bands")
  
  p <- graphics::plot(gp)
  
  return(invisible(p))
  #return(p)
  
}

plot_corr_classes <- function(corr, patterns, title){
  Groups <- c("A","A","A","A","A","B","B","B","B","B","C","C","D","D")
  corr2 <- cbind(corr, Groups)
  gp <- ggplot(corr2, aes(x=Band, y = Correlation, fill=Correlation)) + 
    geom_col() +
    ggtitle(title) +
    scale_fill_gradient2(low = "blue", high = "red", mid="white") +
    theme(text = element_text(size=12,family="Roboto"),
                  title = element_text(size=12,family="Roboto",hjust = 0.5),
                  axis.text=element_text(size=11, family="Roboto")) +
    ggplot2::geom_text(aes(label=Correlation), vjust=-0.3, size=4) +
    ggplot2::labs(x ="Bands", y = "Correlation", fill="Correlation") +
    ggplot2::facet_grid(~ Groups, 
               scales = "free_x",
               space = "free_x",
               switch = "x") 
    
    

  
  p <- graphics::plot(gp)

  
  return(invisible(p))
  
}

plot_corr_matrix <- function(corr){
  
  gp <- ggplot(data = corr, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile(color = "white",
              lwd = 0.5,
              linetype = 1) + ggplot2::facet_wrap(~label) +
    scale_fill_gradient2(low = "blue", high = "red", mid="white") +
    labs(x ="Bands", y = "Bands", fill="Correlation") +
    theme(text = element_text(size=12,family="Roboto"),
          title = element_text(size=12,family="Roboto"),
          axis.text=element_text(size=10, family="Roboto"))
  
  p <- graphics::plot(gp)
  
  return(invisible(p))
  
  
}

plot_stats <- function(dt, stats, bands){
  dt <- dt[band %in% bands & Stats %in% stats]
  
  gp <- ggplot2::ggplot(data = dt, ggplot2::aes(
    x = .data[["Time"]],
    y = .data[["Value"]],
    color = .data[["band"]],
    linetype= .data[["Stats"]]
    
  )) +  ggplot2::facet_wrap(~label) +
    ggplot2::geom_line()
  
  gp <- gp + ggplot2::facet_wrap(~label)
  
  gp <- gp +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(colour = ggplot2::guide_legend(title = "Bands")) +
    ggplot2::ylab("Value")
  
  p <- graphics::plot(gp)
  
  return(invisible(p))
  
}

plot_cloud_cover_percent <- function(cube){
  cloud_cover_by_tile <- list()
  tiles <- cube$tile
  for(t in tiles){
    fi <- cube[cube$tile == t,]$file_info[[1]]
    cbind(fi[fi$band == "NDVI",]$fid,
          fi[fi$band == "NDVI",]$cloud_cover,
          fi[fi$band == "NDVI",]$date)
    cloud_cover_by_tile <- append(cloud_cover_by_tile,
                                  list(data.frame(
                                    fid = fi[fi$band == "NDVI",]$fid,
                                    Dates  = fi[fi$band == "NDVI",]$date,
                                    Cloud_Cover = fi[fi$band == "NDVI",]$cloud_cover))
    )
  }
  df <- do.call("rbind", cloud_cover_by_tile)
  p <- ggplot(df, aes(x=Dates, y=Cloud_Cover, color=Cloud_Cover)) + geom_point(size=2) + 
    scale_x_date(date_labels="%m/%d/%Y", breaks = unique(df$Dates)) +
    scale_colour_gradient(low = "#239B56", high = "#E74C3C") + 
    theme(legend.title = element_text("% Cloud", size=12),
          axis.text=element_text(size=12, family="Roboto"),
          axis.title=element_text(size=12, family="Roboto", face="bold"),
          axis.text.x = element_text(size=11, angle = 90, vjust = 0.5, hjust=0.5)) +
    labs(x ="Dates", y = "", color="% Cloud")
  p <- graphics::plot(p)
  return(invisible(p))
}

plot_patterns <- function(x, year_grid = FALSE){
  plot.df <- purrr::pmap_dfr(
    list(x$label, x$time_series),
    function(label, ts) {
      lb <- as.character(label)
      # extract the time series and convert
      df <- tibble::tibble(Time = ts$Index, ts[-1], Pattern = lb)
      return(df)
    }
  )
  
  
  
  plot.df <- tidyr::pivot_longer(plot.df, cols = sits_bands(x))
  
  # Plot temporal patterns
  gp <- ggplot2::ggplot(plot.df, ggplot2::aes(
    x = .data[["Time"]],
    y = .data[["value"]],
    colour = .data[["name"]]
  )) +
    ggplot2::geom_line()
  
  if (year_grid)
    gp <- gp + ggplot2::facet_grid(year ~ Pattern) + 
          ggplot2::theme(legend.title = element_text(size=12, family="Roboto"))
  else
    gp <- gp + ggplot2::facet_wrap(~Pattern) +
    ggplot2::theme(legend.title = element_text(size=12, family="Roboto"))
  
  gp <- gp +
    ggplot2::theme(legend.position = "right",
                   legend.title = element_text(size=12, family="Roboto"),
                   axis.text=element_text(size=12, family="Roboto"),
                   axis.title=element_text(size=12, family="Roboto", face="bold")) +
    ggplot2::guides(colour = ggplot2::guide_legend(title = "Bands")) +
    ggplot2::ylab("GAM Patterns")
  
  p <- graphics::plot(gp)
  
  return(invisible(p))
}

save_plot_ts <- function(input_data, band, tile_, dir_work, label_){
  new_path <- paste(dir_work, band, sep = "")
  if (!dir.exists(new_path)){
    dir.create(new_path)
  }
  new_path_band <- paste(new_path, label_, sep = "/")
  if (!dir.exists(new_path_band)){
    dir.create(new_path_band)
  }
  
  path_png <- paste(new_path_band, tile_, sep = "/")
  path_png <- paste(path_png, ".png", sep = "")
  png(filename=path_png)
  s.df <- input_data[input_data$label == label_,]
  s.df[s.df$tile == tile_,] %>% sits_select(bands = band)%>% plot()
  dev.off()
  
}
#
