source("xgb_model.R")
ld <- load_data()
train <- ld$train
test <- ld$test

features_default <- features

features <- features_default
clf <- train_model(train, nrounds = 3500, outlier_cutoff = 0.99)
make_predictions(clf, test, file = "~/Mount/xgb_out_best_3500.csv")

features <- features_default[!features_default %in% "DayOfYear"]
clf <- train_model(train, nrounds = 3500, outlier_cutoff = 0.99)
make_predictions(clf, test, file = "~/Mount/xgb_out_best_3500_DayOfYear.csv")

features <- features_default
clf <- train_model(train, nrounds = 4000, max_depth = 9, outlier_cutoff = 0.99)
make_predictions(clf, test, file = "~/Mount/xgb_out_best_depth9_4000.csv")

features <- features_default
clf <- train_model(train, nrounds = 4000, outlier_cutoff = 0.98)
make_predictions(clf, test, file = "~/Mount/xgb_out_best_098_4000.csv")

features <- features_default[!features_default %in% "DayOfYear"]
clf <- train_model(train, nrounds = 4000, outlier_cutoff = 0.99)
make_predictions(clf, test, file = "~/Mount/xgb_out_best_4000_DayOfYear.csv")