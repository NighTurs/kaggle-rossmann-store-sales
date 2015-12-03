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

train <- load_tidy_train()
test <- load_tidy_test()

features <- c("Store", "DayOfWeek", "Promo", "StoreType", "Assortment", 
                             "DayOfMonth", "Year", "Month", "SchoolHoliday", "CompetitionDistance",
                             "CompetitionOpenSinceMonth", "CompetitionOpenSinceYear", "Promo2", 
                           "Promo2SinceWeek", "Promo2SinceYear", "PromoInterval")

train <- mutate(train, Promo = as.numeric(Promo))
test <- mutate(test, Promo = as.numeric(Promo))

gtrain <- train[Open == "Open" & Sales > 0]
#train_outliers <- scores(gtrain$WeekDayPromoMedianSalesResidue, 
#                         type = "chisq", prob = 0.95)
#gtrain <- gtrain[!train_outliers]

test_dates <- c(seq(as.Date(ymd("20130801")), as.Date(ymd("20130917")), by = "day"))
train_train <- gtrain[!(Date %in% test_dates)]
train_test <- gtrain[Date %in% test_dates & Store %in% unique(test$Store)]

dtrain <- xgb.DMatrix(data.matrix(train_train[, features, with = F]), 
                      label=train_train$LogSales)
dval <- xgb.DMatrix(data.matrix(train_test[, features, with = F]), 
                    label=train_test$LogSales)
watchlist <- list(eval = dval, train = dtrain)

param <- list(  objective           = "reg:linear", 
                eta                 = 0.02,
                max_depth           = 10,
                subsample           = 0.9,
                colsample_bytree    = 0.7
)

set.seed(12)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 3000,
                    verbose             = 2, 
                    early.stop.round    = 40,
                    watchlist           = watchlist,
                    maximize            = F,
                    feval               = RMPSE)

test_open <- test[Open == "Open"]
z <- predict(clf, data.matrix(test_open[, features, with = F]))
test <- mutate(test, Sales = 0)
test <- mutate(test, Sales = replace(Sales, Open == "Open", 
                                     exp(z) - 1))
test <- mutate(test, Sales = replace(Sales, Open == "Closed", 0))

out <- test[, list(Id, Sales)]
write.csv(out, file = "xgb_out.csv", row.names = F, quote = F) 


# CV

set.seed(12)
smpl_stores <- sample(unique(test$Store), 100)
gtrain <- train[Open == "Open" & Sales > 0 & Store %in% smpl_stores]
#train_outliers <- scores(gtrain$WeekDayPromoMedianSalesResidue, 
#                         type = "chisq", prob = 0.95)
#gtrain <- gtrain[!train_outliers]
dtrain <- xgb.DMatrix(data.matrix(gtrain[, features, with = F]), 
                      label=gtrain$LogSales)
min_date <- min(train$Date)
folds <- split(1:nrow(gtrain), factor(as.integer(gtrain$Date - min_date) %/% 95))
set.seed(12)
param <- list(  objective           = "reg:linear", 
                eta                 = 0.02,
                max_depth           = 10,
                subsample           = 0.9,
                colsample_bytree    = 0.7
)
b <- xgb.cv(params = param, 
       data = dtrain, 
       nrounds = 3000, 
       nfold = 10, 
       early.stop.round = 20,
       maximize = F,
       folds = folds,
       feval = RMPSE)
