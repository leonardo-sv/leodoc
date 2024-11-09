library(sits)
library(caret)

predict_label <- function(model, predicted){
  labels <- levels(model$cluster_label$labels)
  predictions <- matrix(0L, 
                        length(predicted),
                        length(model$cluster_label$labels))
  colnames(predictions) <- labels
  
  
  
  for (c in model$cluster_label$cluster){
    clabel <- filter(model$cluster_label, cluster == c)
    predictions[, clabel$labels] <- as.numeric(clabel$cluster == predicted)
    
  }
  return(predictions)
  
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

predict_clabel <- function(model, predicted){
  for (c in model$cluster_label$cluster){
    clabel <- dplyr::filter(model$cluster_label, cluster == c)
    predicted[predicted==clabel$cluster]<-as.character(clabel$labels)
    
  }
  return(factor(predicted,levels = sort(unique(samples$label))))
  
}

define_label_cluster <- function(predicted, ground_truth){
  df <- data.frame (labels = ground_truth,
                    cluster = predicted
  )
  
  cluster_label <- df %>% dplyr::group_by(cluster, labels) %>% 
    dplyr::summarise(total_count=dplyr::n(),.groups = "drop_last") %>%
    dplyr::mutate(freq_by_cluster = total_count / sum(total_count)) %>%
    dplyr::filter(freq_by_cluster == max(freq_by_cluster)) 
  
  cluster_label <- cluster_label[, -which(names(cluster_label) 
                                          %in% c("total_count","freq_by_cluster"))]
  cluster_label <- as.data.frame(cluster_label)
  return(cluster_label)
  
}

hcdtw <- function(samples = NULL, ...){
  sits:::.check_set_caller("hcdtw")
  train_fun <- function(samples) {
    # does not support working with DEM or other base data
    if (inherits(samples, "sits_base"))
      stop(sits:::.conf("messages", "sits_train_base_data"), call. = FALSE)
    
    n_clusters <- length(unique(samples$label))
    
    labels <- sort(unique(samples[["label"]]), na.last = TRUE)
    model <- dtwclust::tsclust(
      series = sits::sits_values(samples, format = "cases_dates_bands"),
      type = "hierarchical",
      k = n_clusters,
      distance = "dtw_basic",
      control = dtwclust::hierarchical_control(method = "ward.D2"),
      seed = 76L
    )
    model$bands <- sits::sits_bands(samples)
    predicted <- dtwclust::predict(model, 
                                   sits::sits_values(
                                     samples, 
                                     format = "cases_dates_bands"))
    model$cluster_label<-define_label_cluster(predicted, factor(samples$label))
    predict_fun <- function(values) {
      # Verifies if dtwclust package is installed
      sits:::.check_require_packages("dtwclust")
      # Used to check values (below)
      input_pixels <- nrow(values)
      # Do classification
      values <- stats::predict(
        object = model, 
        newdata = sits::sits_values(values, format = "cases_dates_bands")
      )
      
      values <- predict_clabel(model, values)
      
      # Are the results consistent with the data input?
      # sits:::.check_processed_values(values, input_pixels)
      # Reorder matrix columns if needed
      
      
      # if (any(labels != colnames(values))) {
      #   values <- values[, labels]
      # }
      return(values)
    }
    
    # Set model class
    
    predict_fun <- sits:::.set_class(
      predict_fun,"hcdtw", "sits_model", class(predict_fun)
    )
    
    return(predict_fun)
  }
  
  # if no data is given, we prepare a
  # function to be called as a parameter of other functions
  if (!sits:::.has(samples)) {
    result <- train_fun
  } else {
    # ...otherwise compute the result on the input data
    result <- train_fun(samples)
  }
  return(result)
  
}

models_predict <- function(model, samples){
  predicted <- NULL
  if ("rfor_model" %in% class(model)) {
    predicted_tibble <- sits::sits_classify(
      data = samples, 
      ml_model = model,
      progress = FALSE)
    predicted <- factor(unlist(
                    lapply(
                      predicted_tibble$predicted,
                      function(x) x$class)), sort(unique(samples$label)))
  } else if ("hcdtw" %in% class(model)) {
    predicted <- model(samples)
  }
  
  return(predicted)
}

rf <- function(samples=NULL){
  model <- sits::sits_rfor(samples)
  return(model)
}

all_combinations <- function(set_values){
  return(
    do.call(
      "c",
      lapply(
        seq_along(set_values),
        function(i) combn(set_values, i, FUN = list)
      )
    )
  )
  
}

clustering_metrics <- function(model, ground_truth){
  model <- environment(model)[["model"]]
  suppressMessages(
    vi_evaluators <- dtwclust::cvi_evaluators("valid",
                                              ground.truth = ground_truth))
  score_fun <- vi_evaluators$score
  metrics <- score_fun(list(model))
  metrics = data.frame(metrics)
  metrics$bands <- paste(model$bands, collapse = "-")
  return(metrics[,c("bands", "RI", "ARI", "J", "FM", "Sil", "D","CH")])
  
}

classification_metrics <- function(model, predicted, ground_truth){
  bands <- sits::sits_bands(model)
  bands <- paste(bands, collapse = "-")
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

empty_metric_df <- function(type){
  
  if(type == "supervised"){
    df <- data.frame(
            bands=character(),
            ACC=double(),
            BACC=double(),
            PREC=double(),
            RECL=double(),
            SENS=double(),
            F1=double(),
            SPEC=double())
  }
  else if (type == "unsupervised"){
    df <- data.frame(
      bands=character(),
      RI =double(),
      ARI=double(),
      J=double(),
      FM=double(),
      Sil=double(),
      D=double(),
      CH=double(),
      ACC=double(), 
      BACC=double(),
      PREC=double(),
      RECL=double(),
      SENS=double(),
      F1=double(),
      SPEC=double())
  }
  else {
    df <- NULL
  }
  return(df)
}

confusion_matrix <- function(predicted, groud_truth){
  return(confusionMatrix(predicted, groud_truth))
}

apply_metrics <- function(model, ground_truth, pground_truth, predicted){
  if ("rfor_model" %in% class(model)) {
    metrics <- classification_metrics(model, predicted, pground_truth)
    
  }
  else if ("hcdtw" %in% class(model)) {
    clus_metrics <- clustering_metrics(model, ground_truth)
    clas_metrics <- classification_metrics(model, predicted, pground_truth)
    metrics <- cbind(clus_metrics, clas_metrics[,-1])
  }
  return(metrics)
}

kfold_model <- function(algorithm, samples, folds){
  
  if (algorithm == "hcdtw") metrics <- empty_metric_df("unsupervised") 
  else metrics <- empty_metric_df("supervised")

  
  for (k in 1:max(folds)) {
    if (algorithm == "hcdtw") model <- hcdtw() else model <- rf()
  
    train <- dplyr::filter(samples, folds!=k)
    test <- dplyr::filter(samples, folds==k)
    
    gt_test <- factor(test$label, levels = sort(unique(train$label)))
    gt_train <- factor(train$label, levels = sort(unique(train$label)))
    model<-sits::sits_train(train, model)
    
    predict_test <- models_predict(model, test)
    
    metrics_k <- apply_metrics(model, gt_train, gt_test, predict_test)

    metrics <- rbind(metrics, metrics_k)
    
  }
  
  return(
    metrics %>%
      dplyr::group_by(bands) %>%
        dplyr::summarise(across(dplyr::everything(), mean)))
  
}

run_metrics_path_models <- function(path_models, samples, path_save){
  metrics <- empty_metric_df("supervised")
  metrics <- cbind(metrics, model_algorithm=character())
  for(file in list.files(path_models)){
    path_file <- paste(path_models, file, sep="/")
    print(path_file)

    model <- readRDS(path_file)
    samples_test <- sits_select(samples, sits_bands(model))
    
    ground_truth <- factor(samples_test$label,
                           levels = sort(unique(samples_test$label)))
    

    predict_test <- models_predict(model, samples_test)

    cmetrics <- classification_metrics(model,
                                       predict_test, 
                                       ground_truth)
    cmetrics$model_algorithm <- strsplit(file, "_")[[1]][2]
    metrics <- rbind(metrics, cmetrics)
    
    predict_file <- paste(strsplit(file, '[.]')[[1]][1],"csv", sep=".")
    path_file_predict <- paste(path_save,predict_file, sep="/")
    print(path_file_predict)
    
    write.csv(predict_test,path_file_predict,row.names=F)
  }
  return(metrics)
}

best_model_voting <- function(score, n_bands = "all"){
  best_model <- numeric(nrow(score))
  index <- c(1:nrow(score))
  score <- cbind(best_model, score)
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
    paste("score", n_bands, sep="_")
  
  score <- (subset(score, select = -index))
  score <- score[order(score$score, decreasing = TRUE), ]
  return(score)
  
  
}

prediction_metrics_by_tile <- function(samples_predicted,
                                       start_date, 
                                       end_date, 
                                       bands){
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


voting_nbands <- function(score, n_bands, 
                          fuzzy_score = FALSE, 
                          no_hierarchical = FALSE){
  score_eval <- score[lengths(strsplit(score$bands, '-')) == n_bands,]
  best_model <- voting_best(score_eval, fuzzy_score, no_hierarchical)
  bands <- score_eval[best_model,]$bands
  return(which(score$bands == bands))
}

voting_best <- function(score, fuzzy_score = FALSE,
                        no_hierarchical = FALSE){

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


accuracy_analysis <- function(samples,
                              clusters,
                              start_date, 
                              end_date, 
                              path){
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
      cluster_labeled <- predict_clabel(model, predict_clust)
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

