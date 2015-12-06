source("xgb_model.R")
ld <- load_data()
train <- ld$train
test <- ld$test

features_default <- features

log <- data.table(desc = "null row", 
                  iter = 0, 
                  train.RMPSE.mean = 0, 
                  train.RMPSE.std = 0, 
                  test.RMPSE.mean = 0, 
                  test.RMPSE.std = 0)


report <- function(res, desc = "no desc") {
    iter <- which.min(res$test.RMPSE.mean)
    print(tail(res[1:iter], 10))
    l <- cbind(desc = "nothing", iter = iter, res[iter])
    log <<- rbind(log, l)
    write.csv(log, file = "brute_cv_light.csv", row.names = F, quote = F)
}

report(run_cv(train, test, n_store = 10, outlier_cutoff = 0.99))

report(run_cv(train, test, n_store = 100, 
              outlier_cutoff = 0.99), 
       "Current best. All defaults")

report(run_cv(train, test, n_store = 100, 
              eta = 0.1,
              max_depth = 10, 
              subsample = 0.9,
              colsample = 0.7,
              outlier_cutoff = 0.99), 
       "ETA 0.1")

report(run_cv(train, test, n_store = 100, 
              eta = 0.1,
              max_depth = 6, 
              subsample = 0.9,
              colsample = 0.7,
              outlier_cutoff = 0.99), 
       "ETA 0.1 max_depth 6")

report(run_cv(train, test, n_store = 100, 
              eta = 0.2,
              max_depth = 10, 
              subsample = 0.9,
              colsample = 0.9,
              outlier_cutoff = 0.99), 
       "colsample 0.9")

report(run_cv(train, test, n_store = 100, 
              eta = 0.1,
              max_depth = 8, 
              subsample = 0.9,
              colsample = 0.9,
              outlier_cutoff = 0.99), 
       "ETA 0.1 max_depth 8 colsample 0.9")

report(run_cv(train, test, n_store = 100, 
              eta = 0.05,
              max_depth = 10, 
              subsample = 0.9,
              colsample = 0.7,
              outlier_cutoff = 0.99), 
       "ETA 0.05")

report(run_cv(train, test, n_store = 100, 
              eta = 0.05,
              max_depth = 15, 
              subsample = 0.9,
              colsample = 0.7,
              outlier_cutoff = 0.99), 
       "ETA 0.05 max_depth 15")

report(run_cv(train, test, n_store = 100, 
              eta = 0.2,
              max_depth = 10, 
              subsample = 0.7,
              colsample = 0.9,
              outlier_cutoff = 0.99), 
       "subsample 0.7")

report(run_cv(train, test, n_store = 100, 
              min_child_weight = 2,
              outlier_cutoff = 0.99), 
       "min_child_weight 2")

report(run_cv(train, test, n_store = 100, 
              min_child_weight = 3,
              outlier_cutoff = 0.99), 
       "min_child_weight 3")

report(run_cv(train, test, n_store = 100, 
              min_child_weight = 5,
              outlier_cutoff = 0.99), 
       "min_child_weight 5")

report(run_cv(train, test, n_store = 100, 
              max_depth = 12, 
              outlier_cutoff = 0.99), 
       "max_depth 12")

features <- c(features_default, "Bavarian")
report(run_cv(train, test, n_store = 100, 
              outlier_cutoff = 0.99), 
       "+ Bavarian")

features <- c(features_default, "StoreClosedInTest")
report(run_cv(train, test, n_store = 100, 
              outlier_cutoff = 0.99), 
       "+ StoreClosedInTest")

features <- c(features_default, "WorkingOnSundays")
report(run_cv(train, test, n_store = 100, 
              outlier_cutoff = 0.99), 
       "+ WorkingOnSundays")

features <- c(features_default, "Week")
report(run_cv(train, test, n_store = 100, 
              outlier_cutoff = 0.99), 
       "+ Week")

features <- features_default[!features_default %in% "DayOfYear"]
report(run_cv(train, test, n_store = 100, 
              outlier_cutoff = 0.99), 
       "- DayOfYear")

features <- features_default[!features_default %in% 
                                 c("WeekDaySalesPromoMedianLog", "WeekDaySalesNonPromoMedianLog")]
report(run_cv(train, test, n_store = 100, 
              outlier_cutoff = 0.99), 
       "- WeekDaySalesPromoMedianLog WeekDaySalesNonPromoMedianLog")

features <- features_default[!features_default %in% c("Promo2", 
                                                      "Promo2SinceWeek", 
                                                      "Promo2SinceYear", 
                                                      "PromoInterval")]
report(run_cv(train, test, n_store = 100, 
              outlier_cutoff = 0.99), 
       "- All Promo2")

features <- features_default[!features_default %in% "CompetitionOpenSinceMonth"]
report(run_cv(train, test, n_store = 100, 
              outlier_cutoff = 0.99), 
       "- CompetitionOpenSinceMonth")

features <- features_default[!features_default %in% "SchoolHoliday"]
report(run_cv(train, test, n_store = 100, 
              outlier_cutoff = 0.99), 
       "- SchoolHoliday")

features <- features_default[!features_default %in% "Month"]
report(run_cv(train, test, n_store = 100, 
              outlier_cutoff = 0.99), 
       "- Month")