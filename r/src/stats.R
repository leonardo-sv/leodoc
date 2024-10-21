



## calculate some statistics over time series data set.
create_iqr <- function(dt, band) {
  V1 <- NULL # to avoid setting global variable
  
  data.table::setnames(dt, band, "V1")
  dt_med <- dt[, stats::median(V1), by = Index]
  data.table::setnames(dt_med, "V1", "Median")
  dt_qt25 <- dt[, stats::quantile(V1, 0.25), by = Index]
  data.table::setnames(dt_qt25, "V1", "quantile-25")
  dt_qt75 <- dt[, stats::quantile(V1, 0.75), by = Index]
  data.table::setnames(dt_qt75, "V1", "quantile-75")
  dt_mean <- dt[, mean(V1), by = Index]
  data.table::setnames(dt_sd, "V1", "Mean")
  dt_var <- dt[, stats::var(V1), by = Index]
  data.table::setnames(dt_var, "V1", "Variance")
  dt_sd <- dt[, stats::sd(V1), by = Index]
  data.table::setnames(dt_sd, "V1", "Std Deviation")
  
  
  dt_qts <- merge(dt_med, dt_qt25)
  dt_qts <- merge(dt_qts, dt_qt75)
  dt_qts <- merge(dt_qts, dt_mean)
  dt_qts <- merge(dt_qts, dt_var)
  dt_qts <- merge(dt_qts, dt_sd)
  data.table::setnames(dt, "V1", band)
  return(dt_qts)
}

stats_ts <- function(ts_tibble) {
  V1 <- NULL
  dt_stats <- data.table(
    "Index" = structure(numeric(0), class = "Date"),
    "Value" = numeric(),
    "Stats" = character(),
    "StatsBand" = character(),
    "label" = character(),
    "band" = character()
    
  )
  labels <- sits_labels(ts_tibble)
  bands <- sits_bands(ts_tibble)
  
  for (label in labels){
    dt_bylabel <- ts_tibble[ts_tibble$label == label,]
    ts <- dt_bylabel$time_series
    dt <- data.table::data.table(dplyr::bind_rows(ts))
    
    for(band in bands){
      dt_qts <- data.table(
        Index = structure(numeric(0), class = "Date"),
        "Value" = numeric(),
        "Stats" = character(), 
        "StatsBand" = character()
      )
      data.table::setnames(dt, band, "V1")
      dt_med <- dt[, stats::median(V1), by = Index]
      data.table::setnames(dt_med, "V1", "Value")
      dt_med[ , `:=` (StatsBand = paste(band, "Median)", sep="("),
                      Stats ="Median")]
      
      dt_qt25 <- dt[, stats::quantile(V1, 0.25), by = Index]
      data.table::setnames(dt_qt25, "V1", "Value")   
      dt_qt25[ , `:=` (StatsBand = paste(band, "Q25)", sep="("),
                       Stats ="Q25")]
      
      dt_qt75 <- dt[, stats::quantile(V1, 0.75), by = Index]
      data.table::setnames(dt_qt75, "V1","Value")
      dt_qt75[ , `:=` (StatsBand = paste(band, "Q75)", sep="("),
                       Stats ="Q75")]
      
      dt_var <- dt[, stats::var(V1), by = Index]
      data.table::setnames(dt_var, "V1", "Value")
      dt_var[ , `:=` (StatsBand = paste(band, "Variance)", sep="("),
                      Stats ="Variance")]
      
      dt_mean <- dt[, mean(V1), by = Index]
      data.table::setnames(dt_mean, "V1", "Value")
      dt_mean[ , `:=` (StatsBand = paste(band, "Mean)", sep="("),
                       Stats ="Mean")]
      
      dt_sd <- dt[, stats::sd(V1), by = Index]
      data.table::setnames(dt_sd, "V1", "Value")
      dt_sd[ , `:=` (StatsBand = paste(band, "Standard Deviation)", sep="("),
                     Stats ="Standard Deviation")]
      
      dt_qts <- rbind(dt_qts, dt_med)
      dt_qts <- rbind(dt_qts, dt_qt25)
      dt_qts <- rbind(dt_qts, dt_qt75)
      dt_qts <- rbind(dt_qts, dt_var)
      dt_qts <- rbind(dt_qts, dt_mean)
      dt_qts <- rbind(dt_qts, dt_sd)
      dt_qts[ , `:=` (band=band, label=label)]
      data.table::setnames(dt, "V1", band)
      dt_stats <- rbind(dt_stats, dt_qts)
    }
  }
  data.table::setnames(dt_stats, "Index", "Time")
  return(dt_stats)
  
}

stats_ts2 <- function(ts_tibble) {
  V1 <- NULL
  dt_stats <- data.table(
    "Index" = structure(numeric(0), class = "Date"),
    "Q25" = numeric(),
    "Median" = numeric(),
    "Q75" = numeric(),
    "Variance" = numeric(),
    "label" = character(),
    "band" = character()
  )
  labels <- sits_labels(ts_tibble)
  bands <- sits_bands(ts_tibble)
  
  for (label in labels){
    dt_bylabel <- ts_tibble[ts_tibble$label == label,]
    ts <- dt_bylabel$time_series
    dt <- data.table::data.table(dplyr::bind_rows(ts))
    
    for(band in bands){
      dt_qts <- data.table(
        Index = ts_tibble$time_series[[1]]$Index
      )
      data.table::setnames(dt, band, "V1")
      dt_med <- dt[, stats::median(V1), by = Index]
      dt_qt25 <- dt[, stats::quantile(V1, 0.25), by = Index]
      dt_qt75 <- dt[, stats::quantile(V1, 0.75), by = Index]
      dt_var <- dt[, stats::var(V1), by = Index]
      data.table::setnames(dt_qt25, "V1", "Q25")
      data.table::setnames(dt_med, "V1", "Median")
      data.table::setnames(dt_var, "V1", "Variance")
      data.table::setnames(dt_qt75, "V1","Q75")
      dt_qts <- merge(dt_qts, dt_med)
      dt_qts <- merge(dt_qts, dt_qt25)
      dt_qts <- merge(dt_qts, dt_qt75)
      dt_qts <- merge(dt_qts, dt_var)
      dt_qts[ , `:=` (band=band, label=label)]
      data.table::setnames(dt, "V1", band)
      dt_stats <- rbind(dt_stats, dt_qts)
    }
  }
  return(dt_stats)
  
}

 calculate some statistics over time series data set.
create_iqr <- function(dt, band) {
  V1 <- NULL # to avoid setting global variable
  
  data.table::setnames(dt, band, "V1")
  dt_med <- dt[, stats::median(V1), by = Index]
  data.table::setnames(dt_med, "V1", "Median")
  dt_qt25 <- dt[, stats::quantile(V1, 0.25), by = Index]
  data.table::setnames(dt_qt25, "V1", "quantile-25")
  dt_qt75 <- dt[, stats::quantile(V1, 0.75), by = Index]
  data.table::setnames(dt_qt75, "V1", "quantile-75")
  dt_mean <- dt[, mean(V1), by = Index]
  data.table::setnames(dt_sd, "V1", "Mean")
  dt_var <- dt[, stats::var(V1), by = Index]
  data.table::setnames(dt_var, "V1", "Variance")
  dt_sd <- dt[, stats::sd(V1), by = Index]
  data.table::setnames(dt_sd, "V1", "Std Deviation")
  
  
  dt_qts <- merge(dt_med, dt_qt25)
  dt_qts <- merge(dt_qts, dt_qt75)
  dt_qts <- merge(dt_qts, dt_mean)
  dt_qts <- merge(dt_qts, dt_var)
  dt_qts <- merge(dt_qts, dt_sd)
  data.table::setnames(dt, "V1", band)
  return(dt_qts)
}

stats_ts <- function(ts_tibble) {
  V1 <- NULL
  dt_stats <- data.table(
    "Index" = structure(numeric(0), class = "Date"),
    "Value" = numeric(),
    "Stats" = character(),
    "StatsBand" = character(),
    "label" = character(),
    "band" = character()
    
  )
  labels <- sits_labels(ts_tibble)
  bands <- sits_bands(ts_tibble)
  
  for (label in labels){
    dt_bylabel <- ts_tibble[ts_tibble$label == label,]
    ts <- dt_bylabel$time_series
    dt <- data.table::data.table(dplyr::bind_rows(ts))
    
    for(band in bands){
      dt_qts <- data.table(
        Index = structure(numeric(0), class = "Date"),
        "Value" = numeric(),
        "Stats" = character(), 
        "StatsBand" = character()
      )
      data.table::setnames(dt, band, "V1")
      dt_med <- dt[, stats::median(V1), by = Index]
      data.table::setnames(dt_med, "V1", "Value")
      dt_med[ , `:=` (StatsBand = paste(band, "Median)", sep="("),
                      Stats ="Median")]
      
      dt_qt25 <- dt[, stats::quantile(V1, 0.25), by = Index]
      data.table::setnames(dt_qt25, "V1", "Value")   
      dt_qt25[ , `:=` (StatsBand = paste(band, "Q25)", sep="("),
                       Stats ="Q25")]
      
      dt_qt75 <- dt[, stats::quantile(V1, 0.75), by = Index]
      data.table::setnames(dt_qt75, "V1","Value")
      dt_qt75[ , `:=` (StatsBand = paste(band, "Q75)", sep="("),
                       Stats ="Q75")]
      
      dt_var <- dt[, stats::var(V1), by = Index]
      data.table::setnames(dt_var, "V1", "Value")
      dt_var[ , `:=` (StatsBand = paste(band, "Variance)", sep="("),
                      Stats ="Variance")]
      
      dt_mean <- dt[, mean(V1), by = Index]
      data.table::setnames(dt_mean, "V1", "Value")
      dt_mean[ , `:=` (StatsBand = paste(band, "Mean)", sep="("),
                       Stats ="Mean")]
      
      dt_sd <- dt[, stats::sd(V1), by = Index]
      data.table::setnames(dt_sd, "V1", "Value")
      dt_sd[ , `:=` (StatsBand = paste(band, "Standard Deviation)", sep="("),
                     Stats ="Standard Deviation")]
      
      dt_qts <- rbind(dt_qts, dt_med)
      dt_qts <- rbind(dt_qts, dt_qt25)
      dt_qts <- rbind(dt_qts, dt_qt75)
      dt_qts <- rbind(dt_qts, dt_var)
      dt_qts <- rbind(dt_qts, dt_mean)
      dt_qts <- rbind(dt_qts, dt_sd)
      dt_qts[ , `:=` (band=band, label=label)]
      data.table::setnames(dt, "V1", band)
      dt_stats <- rbind(dt_stats, dt_qts)
    }
  }
  data.table::setnames(dt_stats, "Index", "Time")
  return(dt_stats)
  
}

stats_ts2 <- function(ts_tibble) {
  V1 <- NULL
  dt_stats <- data.table(
    "Index" = structure(numeric(0), class = "Date"),
    "Q25" = numeric(),
    "Median" = numeric(),
    "Q75" = numeric(),
    "Variance" = numeric(),
    "label" = character(),
    "band" = character()
  )
  labels <- sits_labels(ts_tibble)
  bands <- sits_bands(ts_tibble)
  
  for (label in labels){
    dt_bylabel <- ts_tibble[ts_tibble$label == label,]
    ts <- dt_bylabel$time_series
    dt <- data.table::data.table(dplyr::bind_rows(ts))
    
    for(band in bands){
      dt_qts <- data.table(
        Index = ts_tibble$time_series[[1]]$Index
      )
      data.table::setnames(dt, band, "V1")
      dt_med <- dt[, stats::median(V1), by = Index]
      dt_qt25 <- dt[, stats::quantile(V1, 0.25), by = Index]
      dt_qt75 <- dt[, stats::quantile(V1, 0.75), by = Index]
      dt_var <- dt[, stats::var(V1), by = Index]
      data.table::setnames(dt_qt25, "V1", "Q25")
      data.table::setnames(dt_med, "V1", "Median")
      data.table::setnames(dt_var, "V1", "Variance")
      data.table::setnames(dt_qt75, "V1","Q75")
      dt_qts <- merge(dt_qts, dt_med)
      dt_qts <- merge(dt_qts, dt_qt25)
      dt_qts <- merge(dt_qts, dt_qt75)
      dt_qts <- merge(dt_qts, dt_var)
      dt_qts[ , `:=` (band=band, label=label)]
      data.table::setnames(dt, "V1", band)
      dt_stats <- rbind(dt_stats, dt_qts)
    }
  }
  return(dt_stats)
  
}

corr_classes <- function(patterns){
  bands <- sits_bands(patterns)
  pattern_classes <- patterns$label
  df <- data.frame(Correlation = numeric(0),
                   Band = character(0),
                   Classes = character(0))
  
  ts <- patterns$time_series
  comb_2 <- combn(pattern_classes,2)
  for (b in bands){
    for (x in 1:ncol(comb_2)){
      pattern1 <- patterns[patterns$label == comb_2[,x][1],]$time_series[[1]][b]
      pattern2 <-patterns[patterns$label == comb_2[,x][2],]$time_series[[1]][b]
      df_corr <- data.frame(
        Correlation=round(cor(pattern1, pattern2)[1,1],2),
        Band = b,
        Classes = paste(comb_2[,x][1], comb_2[,x][2], sep=" x ")
      )
      df <- rbind(df, df_corr)  
    }
  }
  return(df)
}

corr_ts <- function(ts_tibble){
  
  col_names <- c("Var1", "Var2", "Value", "label")
  df <- read.table(text = "",
                   col.names = col_names)
  labels <- ts_tibble %>% dplyr::pull(label)
  for (l in labels){
    dt_bylabel <- ts_tibble[ts_tibble$label == l,]
    ts <- dt_bylabel$time_series
    cormat <- round(cor(ts[[1]][2:15]),2)
    melted_cormat <- melt(cormat)
    melted_cormat$label <- l
    df <- rbind(df, melted_cormat)
  }
  return(df)
  
}


