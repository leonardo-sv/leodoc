setwd("./src")
source("data.R")
source("stats.R")
source("plot.R")
source("ml_models.R")

# =============== Sampling Time Series =========================================

# Create a S2-16D-2 sits cube
sent_cube <- sits::sits_cube(
  source        = "BDC",
  collection    = "S2-16D-2",
  bands         = c("B01", "B02","B03","B04","B05","B06", "B07","B08", "B8A",
                    "B09","B11", "B12", "NDVI", "EVI"),
  tiles         = c("013015"),
  start_date    = "2019-07-28", 
  end_date      = "2020-07-11"
)


samples_file <- "../../data/samples/short_test_samples.csv"
tmp_dir <- "../data/tmp"
cores <- 16

samples <- sits::sits_get_data(
  cube = sent_cube,
  samples = samples_file,
  bands      = c("B01", "B02","B03","B04","B05","B06", "B07","B08", "B8A",
                 "B09","B11", "B12", "NDVI", "EVI", "CLOUD"),
  multicores = cores,
  output_dir = tmp_dir
)


saveRDS(samples_raw, file = "./data/samples/samples_raw.rds")

# =============== Preparation Data =============================================

samples_raw <- readRDS("./data/samples/samples_raw.rds")

samples_tile <- add_tile(samples_raw,"./data/shp/roi_4326/roi.shp")

saveRDS(samples_tile, file = "./data/samples/samples_013015.rds")

samples <- filter_NDVI(samples_tile,
             "2018-07-28",
             0.75,
             1.0,
             "Forest",
             "013015")

samples_013015_2018 <- subset_by_date(samples_tile, "2018-07-28", "2019-07-28")

saveRDS(samples_013015_2018, 
        file = "./data/samples/samples_013015_2018-07-28_2019-07-28.rds")

# =============== Statistics of Data ===========================================

create_iqr(samples, "Forest","NDVI")

stats <- stats_ts(samples)

stats2 <- stats_ts2(samples)

patterns <- sits_patterns(samples)

inter_corr <- inter_class_correlation(patterns)

intra_corr <- intra_class_correlation(patterns)

# =============== Plot Data ====================================================

plot_stats(stats, c("Median", "Q25", "Q75"), c("NDVI", "EVI"))

plot_box(samples)

plot_patterns(patterns)

plot_inter_correlation(inter_corr)

plot_intra_correlation(intra_corr)

plot_cloud_cover_percent(sent_cube)

# =============== ML functions =================================================

samples <- rbind(head(samples), tail(samples))
all_combinations(sits_bands(samples))

model_hcdtw <- hcdtw(samples)
model_rf <- rf(samples)


models_predict(model_hcdtw, samples)
models_predict(model_rf, samples)

ground_truth <- factor(samples$label, levels = sort(unique(samples$label)))

clustering_metrics(model_hcdtw, ground_truth)
classification_metrics(model_rf,models_predict(model_rf, samples), ground_truth)

empty_metric_df("supervised")
empty_metric_df("unsupervised")

apply_metrics(model_rf, ground_truth, models_predict(model_rf, samples))
apply_metrics(model_hcdtw, ground_truth, models_predict(model_hcdtw, samples))

folds <- caret::createFolds(samples$label, 5, FALSE)

kfold_model("rf", samples, folds)
kfold_model("hcdtw", samples, folds)

