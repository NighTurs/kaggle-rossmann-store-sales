source("read_clean_transform.R")
require(dplyr)
require(xgboost)
require(lubridate)
require(outliers)
require(data.table)

RMPSE<- function(preds, dtrain) {
    labels <- getinfo(dtrain, "label")
    elab<-exp(as.numeric(labels))-1
    epreds<-exp(as.numeric(preds))-1
    err <- sqrt(mean((epreds/elab-1)^2))
    return(list(metric = "RMPSE", value = err))
}


features <- c("Store", "DayOfWeek", "Promo", "StoreType", "Assortment", 
              "DayOfMonth", "Year", "Month", "SchoolHoliday", "CompetitionDistance",
              "CompetitionOpenSinceMonth", "CompetitionOpenSinceYear", "Promo2", 
              "Promo2SinceWeek", "Promo2SinceYear", "PromoInterval", 
              "WeekDaySalesPromoMedianLog", "WeekDaySalesNonPromoMedianLog",
              "DayOfYear", "CompetitionOpen")

load_data <- function() {
    train <- load_tidy_train()
    test <- load_tidy_test()
    
    train <- mutate(train, Promo = as.numeric(Promo))
    test <- mutate(test, Promo = as.numeric(Promo))
    list(train = train, test = test)
}

train_model <- function(train,
                        eta = 0.02, 
                        max_depth = 10, 
                        subsample = 0.9,
                        colsample = 0.7,
                        nrounds = 2000,
                        outlier_cutoff = NA) {
    gtrain <- train[Open == "Open" & Sales > 0 & Month != 12]
    
    set.seed(12)
    if (!is.na(outlier_cutoff)) {
        gtrain[, Outlier := scores(LogSales, type = "chisq", prob = outlier_cutoff), 
               by = list(Store, DayOfWeek, Promo)]
        gtrain <- gtrain[Outlier == F]
    }
    
    dtrain <- xgb.DMatrix(data.matrix(gtrain[, features, with = F]), 
                          label=gtrain$LogSales)
    param <- list(  objective           = "reg:linear", 
                    eta                 = eta,
                    max_depth           = max_depth,
                    subsample           = subsample,
                    colsample_bytree    = colsample
    )
    
    set.seed(12)
    
    clf <- xgb.train(   params              = param, 
                        data                = dtrain, 
                        nrounds             = nrounds,
                        verbose             = 2, 
                        maximize            = F,
                        feval               = RMPSE)
    clf
}

make_predictions <- function(clf, test) {
    test_open <- test[Open == "Open"]
    z <- predict(clf, data.matrix(test_open[, features, with = F]))
    test <- mutate(test, Sales = 0)
    test <- mutate(test, Sales = replace(Sales, Open == "Open", 
                                         exp(z) - 1))
    test <- mutate(test, Sales = replace(Sales, Open == "Closed", 0))
    
    out <- test[, list(Id, Sales)]
    write.csv(out, file = "xgb_out.csv", row.names = F, quote = F) 
}

run_cv <- function(train, test, file = "cv.results.csv", 
                   eta = 0.02, 
                   max_depth = 10, 
                   subsample = 0.9,
                   colsample = 0.7,
                   n_store = NA,
                   outlier_cutoff = NA) {
    set.seed(12)
    if (is.na(n_store)) {
        gtrain <- train[Open == "Open" & Sales > 0 & Month != 12]
    } else {
        smpl_stores <- sample(unique(test$Store), n_store)
        gtrain <- train[Open == "Open" & Sales > 0 & Month != 12 & Store %in% smpl_stores]
    }
    set.seed(12)
    if (!is.na(outlier_cutoff)) {
        gtrain[, Outlier := scores(LogSales, type = "chisq", prob = outlier_cutoff), 
               by = list(Store, DayOfWeek, Promo)]
        gtrain <- gtrain[Outlier == F]
    }
    dtrain <- xgb.DMatrix(data.matrix(gtrain[, features, with = F]), 
                          label=gtrain$LogSales)
    
    min_date <- min(train$Date)
    folds <- split(1:nrow(gtrain), factor(as.integer(gtrain$Date - min_date) %/% 95))
    set.seed(12)
    param <- list(  objective           = "reg:linear", 
                    eta                 = eta,
                    max_depth           = max_depth,
                    subsample           = subsample,
                    colsample_bytree    = colsample
    )
    b <- xgb.cv(params = param, 
           data = dtrain, 
           nrounds = 5000, 
           nfold = 10, 
           early.stop.round = 50,
           maximize = F,
           folds = folds,
           feval = RMPSE)
    write.csv(b, file = file, row.names = F, quote = F)
}
