


voting_nbands <- function(score, n_bands, fuzzy_score = FALSE, no_hierarchical = FALSE){
  score_eval <- score[lengths(strsplit(score$bands, '-')) == n_bands,]
  best_model <- voting_best(score_eval, fuzzy_score, no_hierarchical)
  bands <- score_eval[best_model,]$bands
  return(which(score$bands == bands))
}

voting_best <- function(score, fuzzy_score = FALSE, no_hierarchical = FALSE){

  eval_metrics <- c("RI", "ARI", "J", "VI","FM", "Sil", "D", 
                    "ACC", "BACC","PREC", "RECL","SENS", "F1", "SPEC")
  fuzzy_metrics <- c("MPC", "K", "T", "SC", "PBMF","NMIM")
  no_hierarchical_metrics <- c("DB", "DBstar", "CH", "SF","COP")
  
  if (fuzzy_score){
    eval_metrics <- c(eval_metrics, fuzzy_metrics)
  }
  
  if (no_hierarchical){
    eval_metrics <- c(eval_metrics, no_hierarchical_metrics)
  }
  
  score_eval <- score[, which(names(score) %in% eval_metrics)]
  
  
  best_by_metrics <- apply(score_eval, 2L, which.max)
  
  majority <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  majority(best_by_metrics)
  
}

best_model_voting <- function(score, n_bands = "all"){
  best_model <- numeric(nrow(score))
  index <- c(1:nrow(score))
  score <- cbind(score, best_model)
  score <- cbind(score, index)
  
  minimized <- c("VI","COP", "DB", "DBstar", "K", "T")
  maximized <- c("RI","ARI", "J", "FM", "Sil", "CH","D",  "SF", 
                 "ACC", "BACC" ,"PREC", "SENS", "F1", "SPEC", "RECL")
  
  if(is.numeric(n_bands)){
    compute_score <- score[score$n_bands == n_bands,]
  }
  else{
    compute_score <- score
  }
  
  for(i in colnames(score)){
    if (i %in% maximized){
      index <-compute_score[which.max(compute_score[i][,1]),]$index
      score[score$index == index,]$best_model <- 
        score[score$index == index,]$best_model +1
      
    }
    else if (i %in% minimized){
      index <-compute_score[which.min(compute_score[i][,1]),]$index
      score[score$index == index,]$best_model <- 
        score[score$index == index,]$best_model +1
      
    }
    
  }
  colnames(score)[which(names(score) == "best_model")] <- 
    paste("best_model", n_bands, sep="_")
  
  score <- (subset(score, select = -index))
  score <- score[order(score$best_model_all, decreasing = TRUE), ]
  return(score)
  
  
}

define_label_cluster <- function(predicted, ground_truth){
  df <- data.frame (labels = ground_truth,
                    cluster = predicted
  )
  
  cluster_label <- df %>% group_by(cluster, labels) %>% 
    summarise(total_count=n(),.groups = "drop_last") %>%
    mutate(freq_by_cluster = total_count / sum(total_count)) %>%
    filter(freq_by_cluster == max(freq_by_cluster)) 
  
  cluster_label <- cluster_label[, -which(names(cluster_label) 
                                          %in% c("total_count","freq_by_cluster"))]
  cluster_label <- as.data.frame(cluster_label)
  return(cluster_label)
  
}

cluster_predicted_label <- function(model, predicted){
  for (c in model$cluster_label$cluster){
    clabel <- filter(model$cluster_label, cluster == c)
    predicted[predicted==clabel$cluster]<-as.character(clabel$labels)
    
  }
  return(factor(predicted))
  
}

define_label_cluster2 <- function(predicted, gt){
  df <- data.frame (labels = gt,
                    cluster = predicted
  )
  
  cluster_label <- df %>% group_by(cluster, labels) %>% 
    summarise(total_count=n(),.groups = "drop_last") %>%
    mutate(freq_by_cluster = total_count / sum(total_count)) %>%
    filter(freq_by_cluster == max(freq_by_cluster)) 
  
  return(sapply(df$cluster, 
                function(s) cluster_label[cluster_label$cluster == s,]$labels))
  
}

accuracy_model <- function(model, gt, samples){
  predict_clust <- dtwclust::predict(model, model$datalist)
  cluster_labeled <- cluster_predicted_label(model, predict_clust)
  hits <- as.integer(gt == cluster_labeled)
  acc <- sum(hits)/length(hits)
  return(acc)
}

accuracy_model2 <- function(model, values){
  predict_clust <- dtwclust::predict(model, values)
  return(predict_clust)
}


model_by_combination_bands <- function(model, samples, bands, r){
  
  combns_hcluster <- list()
  
  combinations <- combn(bands,r)
    
  for(c in 1:length(combinations[1,])){
      comb_bands <- combinations[,c]
      print(comb_bands)
      values <- sits_values(samples, comb_bands, format = "cases_dates_bands")
      
      
      dendro <- dtwclust::tsclust(
        series = values,
        type = "hierarchical",
        k = max(nrow(values)-1, 3),
        distance = "dtw_basic",
        control = dtwclust::hierarchical_control(method = "ward.D2")
        
      )
      dendro$bands <- comb_bands
      ground_truth <- factor(samples$label)
      predicted <- dtwclust::predict(dendro, values)
      dendro$cluster_label <- define_label_cluster(predicted, ground_truth)
      
      combns_hcluster <- append(combns_hcluster,list(dendro))
      
    }
    return(combns_hcluster)
}

cluster_by_band <- function(clusters, bands){
  count = 1
  for(c in clusters){
    tband <- paste(c$bands, collapse = "/")
    if(tband == bands){
      return(count)
    }
    count <- count + 1
  }
  return(NULL)
}


best_clusters <- function(score, clusters){
  bclusters <- list()
  voting <- data.frame(select(score,contains("best")))
  cols <- colnames(voting)
  for(col in cols){
    bbands<- score[which.max(voting[, col]),]$cbands
    
    cpos <- cluster_by_band(clusters, bbands)
    model <- clusters[[cpos]]
    bclusters <- append(bclusters, list(model))
  }
  return(bclusters)
}



accuracy_analysis <- function(samples, clusters, start_date, end_date, path){
  tiles <- unique(samples$tile)
  
  acc_df <- data.frame(band=character(0),
                       acc=numeric(0),
                       tile=character(0),
                       start_date=date,
                       end_date=date)
  
  for(model in clusters){
    for(tile in tiles){
      tsamples <- samples[samples$tile == tile,]
      tsamples <-subset_by_date(tsamples, start_date,end_date)
      ts <- sits_values(tsamples, model$bands, format = "cases_dates_bands")
      gt <- factor(tsamples$label)
      predict_clust <- accuracy_model2(model, ts)
      cluster_labeled <- cluster_predicted_label(model, predict_clust)
      hits <- as.integer(gt == cluster_labeled)
      acc_cluster <- sum(hits)/length(hits)
      bands_cluster <- paste(model$bands, collapse = "/") 
      acc_save <- data.frame(
        band=bands_cluster,
        acc = acc_cluster,
        tile = tile,
        start_date = start_date,
        end_date = end_date
      )
      acc_df <- rbind(acc_df, acc_save)
      
      
      
      tosave <- tsamples[, -which(names(samples) == "time_series")]
      tosave$cluster <- predict_clust
      tosave$cluster_label <- cluster_labeled
      nametosave <- paste(model$bands, collapse = "-")
      nametosave <- paste("cluster",nametosave,sep="_")
      nametosave <- paste(nametosave,tile, sep="_")
      nametosave <- paste(nametosave, start_date, end_date, sep="-")
      nametosave <- paste(path, nametosave,sep="")
      nametosave <- paste(nametosave,"csv",sep=".")
      write.csv(tosave, nametosave, row.names=TRUE)
      
      nameacc <- paste("acc", start_date, end_date, sep="_")
      nameacc <- paste(path,nameacc, sep="")
      nameacc <- paste(nameacc,"csv", sep=".")
      tosave <- as.data.frame(tosave)
      write.csv(acc_df, nameacc, row.names=TRUE)
      
      
    }
    
  }
  return(acc_df)
  
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

bynary_classification_metrics <- function(predicted, actual){
  cm <- table(predicted, actual)
  VP <- cm[1]
  FN <- cm[2]
  FP <- cm[3]
  VN <- cm[4]
  ACC <- sum(cm[1], cm[4]) / sum(cm[1:4])
  PREC <- cm[4] / sum(cm[4], cm[2])
  SENS <- cm[4] / sum(cm[4], cm[3])
  F1 <- (2 * (SENS * PREC))/(SENS + PREC)
  SPEC <- cm[1] / sum(cm[1], cm[2])
  
  ACC_F <- -1
  ACC_D <- -1
  if("Forest" %in% colnames(cm)){
    ACC_F <-  cm["Forest", "Forest"] / sum(cm["Forest",])   
  }
  if("Deforestation" %in% colnames(cm)){
    ACC_D <-  cm["Deforestation", "Deforestation"] / sum(cm["Deforestation",])  
  }
  
  metrics <- data.frame(ACC,PREC,SENS,F1,SPEC,ACC_F,ACC_D,VP,FN,FP,VN)

  return(metrics)
  
}

clustering_metrics <- function(model, ground_truth){
  suppressMessages(vi_evaluators <- cvi_evaluators("valid", ground.truth = ground_truth))
  score_fun <- vi_evaluators$score
  metrics <- score_fun(list(model))
  metrics = data.frame(metrics)
  metrics$bands <- paste(model$bands, collapse = "-")
  return(metrics[,c("bands", "RI", "ARI", "J", "FM", "Sil", "D","CH")])
  
}

classification_eval <- function(predicted, ground_truth){
  cm <-confusionMatrix(predicted, ground_truth)
  
  Sensitivity <- 
  
  VP <- cm[1]
  FN <- cm[2]
  FP <- cm[3]
  VN <- cm[4]
  ACC <- sum(cm[1], cm[4]) / sum(cm[1:4])
  PREC <- cm[4] / sum(cm[4], cm[2])
  SENS <- cm[4] / sum(cm[4], cm[3])
  F1 <- (2 * (SENS * PREC))/(SENS + PREC)
  SPEC <- cm[1] / sum(cm[1], cm[2])
  
  ACC_F <- -1
  ACC_D <- -1
  
  metrics <- data.frame(ACC,PREC,SENS,F1,SPEC,ACC_F)
}

call_classification_metrics <- function(model, ground_truth, samples){
  bands <- model$bands
  values <- sits_values(samples, bands, format = "cases_dates_bands")
  predicted <- dtwclust::predict(model, values)
  predicted <- cluster_predicted_label(model, predicted)
  cmetrics <- classification_metrics(predicted, ground_truth)
  bands <- paste(model$bands, collapse = "/") 
  cmetrics <- cbind(bands,cmetrics)
  return(cmetrics)
}

confusion_matrix <- function(predicted, groud_truth){
  return(confusionMatrix(predicted, groud_truth))
}

classification_metrics <- function(predicted, ground_truth, bands_model){
  bands <- paste(bands_model, collapse = "-")
  cm <- confusionMatrix(predicted, ground_truth)
  if(length(unique(ground_truth)) > 2){
    ACC <- cm$overall["Accuracy"][[1]]
    BACC <- mean(cm$byClass[,"Balanced Accuracy"])
    PREC <- if (is.na(mean(cm$byClass[,"Precision"]))) 0 else mean(cm$byClass[,"Precision"])
    RECL <- mean(cm$byClass[,"Recall"])
    SENS <- mean(cm$byClass[,"Sensitivity"])
    F1 <- if (is.na(mean(cm$byClass[,"F1"]))) 0 else mean(cm$byClass[,"F1"]) 
    SPEC <- mean(cm$byClass[,"Specificity"])
    
    
  }
  else{

    ACC <- cm$overall["Accuracy"][[1]]
    BACC <- cm$byClass["Balanced Accuracy"][[1]]
    PREC <- cm$byClass["Precision"][[1]]
    RECL <- cm$byClass["Recall"][[1]]
    SENS <- cm$byClass["Sensitivity"][[1]]
    F1 <- cm$byClass["F1"][[1]]
    SPEC <- cm$byClass["Specificity"][[1]]
  }
  metrics <- data.frame(bands,ACC,BACC,PREC,RECL,SENS,F1,SPEC)
  return(metrics)
}


models_metrics <- function(models, samples){
  ground_truth <- factor(samples$label)
  cluster_metrics <- lapply(models, clustering_metrics, ground_truth)
  cluster_metrics <- bind_rows(cluster_metrics)
  class_metrics  <- lapply(models, call_classification_metrics, ground_truth)
  class_metrics <- bind_rows(bclass_metrics)
  metrics <- bind_cols(cluster_metrics, bclass_metrics, .id = "bands")
  names(metrics)[names(metrics) == "bands...1"] <- "bands"
  metrics <- metrics[, -which(names(metrics) %in% c(".id", "bands...14"))]
  return(metrics)
  
}


model_metrics <- function(hclusters, samples){
  gt <- factor(samples$label)
  vi_evaluators <- cvi_evaluators("valid", ground.truth = gt)
  score_fun <- vi_evaluators$score
  metrics <- score_fun(hclusters)
  ACC <- sapply(hclusters,accuracy_model,gt=gt)
  metrics <- cbind(metrics, ACC)
  score <- as.data.frame(metrics)
  cbands <- c()
  n_bands <- c()
  inte <- 0
  for(nf in 1:n_comb){
    comb <-combn(bands,nf)
    for(col in 1:ncol(comb)){
      cbands <- append(cbands, paste(comb[,col],collapse='/'))
      n_bands <- append(n_bands, nf)
    }
  }
  score <- cbind(score, cbands)
  score <- cbind(score, n_bands)
  
  score <- best_model_voting(score)
  
  for(i in 1:n_comb){
    score <- best_model_voting(score,i)
  }
  
  return(score)
}

prediction_test <- function(samples,
                            model,
                            start_date,
                            end_date,
                            path,
                            tile_train = NULL){
  
  prediction_df <- data.frame(
    start_date = start_date,
    end_date = end_date,
    bands = paste(model$bands, collapse = "-")
  )
  
  tsamples <-subset_by_date(samples, start_date,end_date)
  ts <- sits_values(tsamples, model$bands, format = "cases_dates_bands")
  predicted <- dtwclust::predict(model, ts)
  ground_truth <- factor(tsamples$label)
  predicted_labeled <- cluster_predicted_label(model, predicted)
  
  
  samples_save <- tsamples[, -which(names(tsamples) == "time_series")]
  samples_save$cluster_prediction <- predicted
  samples_save$cluster_labeled <- predicted_labeled
  nametosave <- paste(model$bands, collapse = "-")
  nametosave <- paste("cluster",nametosave,sep="_")
  nametosave <- paste(nametosave, start_date, end_date, sep="-")
  nametosave <- paste(path, nametosave,sep="/")
  nametosave <- paste(nametosave,"csv",sep=".")
  write.csv(samples_save, nametosave, row.names=TRUE)
  
  test_samples <- samples_save
  
  if(!is.null(tile_train)){
    test_samples <- samples_save[samples_save$tile != tile_train,]
    train_samples <- samples_save[samples_save$tile == tile_train,]
    
    metrics <- bynary_classification_metrics(samples_save$cluster_labeled, samples_save$label)
    prediction_df$type <- "Train"
    train <- cbind(prediction_df, metrics)
  }
  
  metrics <- bynary_classification_metrics(test_samples$cluster_labeled, test_samples$label)
  prediction_df$type <- "Generalization(Test)"  
  test <- cbind(prediction_df, metrics)
  
  if(!is.null(tile_train)){
    return_df <- rbind(train, test)
  }
  else{
    return_df <- test
  }
  
  return(return_df)
  
}

prediction_metrics_by_tile <- function(samples_predicted, start_date, end_date, bands){
  tiles <- unique(samples_predicted$tile)
  prediction_df <- data.frame(
    start_date = start_date,
    end_date = end_date,
    bands = bands
  )
  test_acc <- data.frame(tile=character(0),
                         start_date=date,
                         end_date=date,
                         band=character(0),
                         ACC=numeric(0),
                         PREC=numeric(0),
                         SENS=numeric(0),
                         F1=numeric(0),
                         SPEC=numeric(0),
                         ACC_F=numeric(0),
                         ACC_D=numeric(0)
  )
  
  for (tile in tiles){
    samples_by_tile <- samples_predicted[samples_predicted$tile == tile,]
    metrics <- bynary_classification_metrics(
      samples_by_tile$cluster_labeled, samples_by_tile$label)
    tile_metric <- data.frame(tile=tile,
                              start_date=start_date,
                              end_date=end_date,
                              band=bands)
    tile_metric <-cbind(tile_metric, metrics)
    test_acc <- rbind(test_acc,tile_metric)
  }
  return(test_acc)
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

predict_to_sits <- function(model, predicted){
  clabels <- model$cluster_label
  
  pred_sits = matrix(0, length(predicted), length(clabels$labels))
  colnames(pred_sits) = clabels$labels
  
  for (label in clabels$labels){
    log_pred <- (predicted == clabels[clabels$labels==label,]$cluster)
    pred_sits[, label] <- as.double(log_pred)

  }
  return(pred_sits)
  
  
}


