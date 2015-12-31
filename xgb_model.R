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
                        seed = 12,
                        outlier_cutoff = NA) {
    gtrain <- train[Open == "Open" & Sales > 0 & Month != 12]
    
    set.seed(seed)
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
    
    set.seed(seed)
    
    clf <- xgb.train(params              = param, 
                     data                = dtrain, 
                     nrounds             = nrounds,
                     verbose             = 2, 
                     maximize            = F,
                     feval               = RMPSE)
}

make_predictions <- function(clf, test, file = "xgb_out.csv") {
    test_open <- test[Open == "Open"]
    z <- predict(clf, data.matrix(test_open[, features, with = F]))
    test <- mutate(test, Sales = 0)
    test <- mutate(test, Sales = replace(Sales, Open == "Open", 
                                         exp(z) - 1))
    test <- mutate(test, Sales = replace(Sales, Open == "Closed", 0))
    
    out <- test[, list(Id, Sales)]
    write.csv(out, file = file, row.names = F, quote = F) 
}

run_cv <- function(train, test, file = "cv.results.csv", 
                   eta = 0.02, 
                   max_depth = 10, 
                   subsample = 0.9,
                   colsample = 0.7,
                   min_child_weight = 1,
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
                    colsample_bytree    = colsample,
                    min_child_weight    = min_child_weight
    )
    b <- xgb.cv(params = param, 
           data = dtrain, 
           nrounds = 10000, 
           nfold = 10, 
           early.stop.round = 50,
           maximize = F,
           folds = folds,
           feval = RMPSE)
    write.csv(b, file = file, row.names = F, quote = F)
    b
}

better_cv <- function(train, test, file = "cv.results.csv", 
                      eta = 0.02, 
                      max_depth = 10, 
                      subsample = 0.9,
                      colsample = 0.7,
                      nrounds = 5000,
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
    } else {
        gtrain[, Outlier:=F]
    }
    
    folds <- list(
         seq(as.Date("2014-08-25"), as.Date("2014-09-17"), "day"),
         seq(as.Date("2013-08-01"), as.Date("2013-08-24"), "day"),
         seq(as.Date("2013-08-25"), as.Date("2013-09-17"), "day"),
         seq(as.Date("2014-08-01"), as.Date("2014-08-24"), "day"))
    
    # First fold is easiest one, so we will keep number of rounds from it
    first <- T
    
    res <- data.table(iter = -1, eval = 0, train = 0)
    for (i in 1:length(folds)) {
        print(paste("processing fold", i, sep = " "))
        test_dates <- folds[[i]]
        train_train <- gtrain[!(Date %in% test_dates) & Outlier == F]
        train_test <- gtrain[Date %in% test_dates & Store %in% unique(test$Store)]
        dtrain <- xgb.DMatrix(data.matrix(train_train[, features, with = F]), 
                                          label=train_train$LogSales)
        dval <- xgb.DMatrix(data.matrix(train_test[, features, with = F]), 
                                          label=train_test$LogSales)
        watchlist <- list(eval = dval, train = dtrain)
        param <- list(  objective           = "reg:linear", 
                        eta                 = eta,
                        max_depth           = max_depth,
                        subsample           = subsample,
                        colsample_bytree    = colsample
        )
        
        set.seed(12)
        
        if (first) {
            output <- capture.output(
                           clf <- xgb.train(   params              = param, 
                                               data                = dtrain, 
                                               nrounds             = nrounds,
                                               verbose             = 2, 
                                               watchlist           = watchlist,
                                               early.stop.round    = 150,
                                               maximize            = F,
                                               feval               = RMPSE))
            first <- F
        } else {
            output <- capture.output(
                clf <- xgb.train(   params              = param, 
                                    data                = dtrain, 
                                    nrounds             = max(res_cur$iter) + 1,
                                    verbose             = 2, 
                                    watchlist           = watchlist,
                                    maximize            = F,
                                    feval               = RMPSE))
        }
        chrs <- tstrsplit(output[seq(2, length(output), 2)], "[\t:]", fixed=F)
        res_cur <- data.table(iter = as.integer(gsub("(\\[|\\])", "", chrs[[1]])), 
                         eval = as.numeric(chrs[[3]]), 
                         train = as.numeric(chrs[[5]]))
        print(res_cur)
        res <- rbind(res, res_cur)
    }
    b <- res %>% group_by(iter) %>% summarise(train_mean = mean(train), train_min = min(train),
                                         train_max = max(train), train_sd = sd(train),
                                         test_mean = mean(eval), test_min = min(eval),
                                         test_max = max(eval), test_sd = sd(eval), n = n())
    write.csv(b, file = file, row.names = F, quote = F)
    b
}

suggest_iterations <- function(res) {
    tail(res[1:which.min(res$test_mean[-1])], 10)
}