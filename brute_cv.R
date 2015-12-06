source("xgb_model.R")
ld <- load_data()
train <- ld$train
test <- ld$test

better_cv(train, test, file = "~/Mount/002_10_09_07_NA_v2.csv", 
        eta = 0.02, 
        max_depth = 10, 
        subsample = 0.9,
        colsample = 0.7, 
        outlier_cutoff = NA)

better_cv(train, test, file = "~/Mount/002_10_09_07_099_v2.csv", 
       eta = 0.02, 
       max_depth = 10, 
       subsample = 0.9,
       colsample = 0.7, 
       outlier_cutoff = 0.99)

better_cv(train, test, file = "~/Mount/001_10_07_07_099_v2.csv", 
          eta = 0.01, 
          max_depth = 10, 
          subsample = 0.7,
          colsample = 0.7, 
          outlier_cutoff = 0.99)

better_cv(train, test, file = "~/Mount/001_08_07_07_099_v2.csv", 
          eta = 0.01, 
          max_depth = 8, 
          subsample = 0.7,
          colsample = 0.7, 
          outlier_cutoff = 0.99)

features <- c("Store", "DayOfWeek", "Promo", "StoreType", "Assortment", 
              "DayOfMonth", "Year", "Month", "SchoolHoliday", "CompetitionDistance",
              "CompetitionOpenSinceMonth", "CompetitionOpenSinceYear", "Promo2", 
              "Promo2SinceWeek", "Promo2SinceYear", "PromoInterval", "CompetitionOpen")

better_cv(train, test, file = "~/Mount/002_10_09_07_099_removed_median_promo_year_day_v2.csv", 
          eta = 0.02, 
          max_depth = 10, 
          subsample = 0.9,
          colsample = 0.7,
          outlier_cutoff = 0.99)