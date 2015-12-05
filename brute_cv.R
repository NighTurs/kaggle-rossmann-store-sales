source("xgb_model.R")
ld <- load_data()
train <- ld$train
test <- ld$test

run_cv(train, test, file = "~/Mount/002_10_09_07_NA.csv", 
       eta = 0.02, 
       max_depth = 10, 
       subsample = 0.9,
       colsample = 0.7, 
       outlier_cutoff = NA)

run_cv(train, test, file = "~/Mount/002_10_09_07_856_NA.csv", 
       eta = 0.02, 
       max_depth = 10, 
       subsample = 0.9,
       colsample = 0.7, 
       n_store = 856,
       outlier_cutoff = NA)

run_cv(train, test, file = "~/Mount/002_10_09_07_099.csv", 
       eta = 0.02, 
       max_depth = 10, 
       subsample = 0.9,
       colsample = 0.7, 
       outlier_cutoff = 0.99)

run_cv(train, test, file = "~/Mount/002_10_09_07_097.csv", 
       eta = 0.02, 
       max_depth = 10, 
       subsample = 0.9,
       colsample = 0.7, 
       outlier_cutoff = 0.97)

run_cv(train, test, file = "~/Mount/002_10_09_07_095.csv", 
       eta = 0.02, 
       max_depth = 10, 
       subsample = 0.9,
       colsample = 0.7, 
       outlier_cutoff = 0.95)