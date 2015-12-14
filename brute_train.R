source("xgb_model.R")
ld <- load_data()
train <- ld$train
test <- ld$test

features_default <- features

features <- features_default
clf <- train_model(train, nrounds = 4000, seed = 32, outlier_cutoff = 0.99)
make_predictions(clf, test, file = "mix/xgb_out_best_seed32.csv")

features <- features_default
clf <- train_model(train, nrounds = 4000, seed = 999, outlier_cutoff = 0.99)
make_predictions(clf, test, file = "mix/xgb_out_best_seed999.csv")

features <- features_default
clf <- train_model(train, nrounds = 4000, seed = 5899, outlier_cutoff = 0.99)
make_predictions(clf, test, file = "mix/xgb_out_best_seed5899.csv")