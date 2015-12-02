source("read_clean_transform.R")
require(dplyr)
require(xgboost)
require(lubridate)
require(outliers)

RMPSE<- function(preds, dtrain) {
    labels <- getinfo(dtrain, "label")
    weight <- getinfo(dtrain, "weight")
    elab<-exp(as.numeric(weight + labels))-1
    epreds<-exp(as.numeric(weight + preds))-1
    err <- sqrt(mean((epreds/elab-1)^2))
    return(list(metric = "RMPSE", value = err))
}

train <- load_tidy_train()
test <- load_tidy_test()

features <- c("Store", "DayOfWeek", "Promo", "StoreType", "Assortment", 
                             "DayOfYear", "Year", "Week", "Month", "SchoolHoliday", "CompetitionDistance",
                             "CompetitionOpenSinceMonth", "CompetitionOpenSinceYear", "Promo2", 
                           "Promo2SinceWeek", "Promo2SinceYear", "PromoInterval")

#features <- c("Store", "DayOfWeek", "Promo", "StoreType", "Assortment", 
#              "Year", "Month", "SchoolHoliday", "CompetitionDistance",
#              "CompetitionOpenSinceMonth", "CompetitionOpenSinceYear", "Promo2", 
#              "Promo2SinceWeek", "Promo2SinceYear", "PromoInterval", 
#              "Promo2Started", "CompetitionOpen")


train <- mutate(train, Promo = as.numeric(Promo))
test <- mutate(test, Promo = as.numeric(Promo))

gtrain <- train[Open == "Open" & Sales > 0]
train_outliers <- scores(gtrain$WeekDayPromoMedianSalesResidue, 
                         type = "chisq", prob = 0.95)
gtrain <- gtrain[!train_outliers]

set.seed(100)
sml <- sample(nrow(gtrain), 20000)
train_train <- gtrain[-sml]
train_test <- gtrain[sml]

#test_dates <- c(seq(as.Date(ymd("20150110")), as.Date(ymd("20150410")), by = "day"))
#train_train <- train[!(Date %in% test_dates) & Open == "Open" & Sales > 0 & Store < 100]
#train_test <- train[Date %in% test_dates & Open == "Open" & Sales > 0 & Store < 100]

dtrain <- xgb.DMatrix(data.matrix(train_train[, features, with = F]), 
                      label=train_train$WeekDayPromoMedianSalesResidue,
                      weight=train_train$WeekDayPromoMedianSales)
dval <- xgb.DMatrix(data.matrix(train_test[, features, with = F]), 
                    label=train_test$WeekDayPromoMedianSalesResidue,
                    weight=train_test$WeekDayPromoMedianSales)
watchlist <- list(eval = dval, train = dtrain)

param <- list(  objective           = "reg:linear", 
                eta                 = 0.01,
                max_depth           = 8,
                subsample           = 0.9,
                colsample_bytree    = 0.8
)

set.seed(12)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 2000,
                    verbose             = 2, 
                    early.stop.round    = 50,
                    watchlist           = watchlist,
                    maximize            = F,
                    feval               = RMPSE)

test_open <- test[Open == "Open"]
z <- predict(clf, data.matrix(test_open[, features, with = F]))
test <- mutate(test, Sales = 0)
test <- mutate(test, Sales = replace(Sales, Open == "Open", 
                                     exp(WeekDayPromoMedianSales[Open == "Open"] + z) - 1))
test <- mutate(test, Sales = replace(Sales, Open == "Closed", 0))

out <- test[, list(Id, Sales)]
write.csv(out, file = "xgb_out.csv", row.names = F, quote = F) 


# CV

set.seed(12)
gtrain <- train[Open == "Open" & Sales > 0]
train_outliers <- scores(gtrain$WeekDayPromoMedianSalesResidue, 
                         type = "chisq", prob = 0.95)
gtrain <- gtrain[!train_outliers]
dtrain <- xgb.DMatrix(data.matrix(gtrain[, features, with = F]), 
                      label=gtrain$WeekDayPromoMedianSalesResidue,
                      weight=gtrain$WeekDayPromoMedianSales)
min_date <- min(train$Date)
folds <- split(1:nrow(gtrain), factor(as.integer(gtrain$Date - min_date) %/% 236))
set.seed(12)
b <- xgb.cv(params = param, 
       data = dtrain, 
       nrounds = 3000, 
       nfold = 4, 
       early.stop.round = 2000,
       maximize = F,
       folds = folds,
       feval = RMPSE)
