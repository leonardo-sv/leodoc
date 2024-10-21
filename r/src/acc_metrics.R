


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


