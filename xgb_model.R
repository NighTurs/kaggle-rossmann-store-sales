source("read_clean_transform.R")
require(dplyr)
require(xgboost)
require(lubridate)

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
              "DayOfYear", "Year", "Week", "Month", "SchoolHoliday", "CompetitionDistance",
              "CompetitionOpenSinceMonth", "CompetitionOpenSinceYear", "Promo2", 
              "Promo2SinceWeek", "Promo2SinceYear", "PromoInterval", 
              "WeekEven", "Promo2Days", "Promo2Started", "CompetitionOpenDays", "CompetitionOpen",
              "StateHoliday")


train <- mutate(train, Promo = as.numeric(Promo))
test <- mutate(test, Promo = as.numeric(Promo))

set.seed(100)
sml <- sample(nrow(train), 20000)
train_train <- train[-sml][Open == "Open" & Sales > 0]
train_test <- train[sml][Open == "Open" & Sales > 0]

#test_dates <- c(seq(as.Date(ymd("20150110")), as.Date(ymd("20150410")), by = "day"))
#train_train <- train[!(Date %in% test_dates) & Open == "Open" & Sales > 0]
#train_test <- train[Date %in% test_dates & Open == "Open" & Sales > 0]

dtrain <- xgb.DMatrix(data.matrix(train_train[, features, with = F]), 
                      label=train_train$LogSales)
dval <- xgb.DMatrix(data.matrix(train_test[, features, with = F]), 
                    label=train_test$LogSales)
watchlist <- list(eval = dval, train = dtrain)

param <- list(  objective           = "reg:linear", 
                eta                 = 0.02,
                max_depth           = 10,
                subsample           = 0.7,
                colsample_bytree    = 0.8
)

set.seed(12)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 3000,
                    verbose             = 2, 
                    early.stop.round    = 100,
                    watchlist           = watchlist,
                    maximize            = F,
                    feval               = RMPSE)

test_open <- test[Open == "Open"]
z <- predict(clf, data.matrix(test_open[, features, with = F]))
test <- mutate(test, Sales = 0)
test <- mutate(test, Sales = replace(Sales, Open == "Open", exp(z) - 1))
test <- mutate(test, Sales = replace(Sales, Open == "Closed", 0))

out <- test[, list(Id, Sales)]
write.csv(out, file = "xgb_out.csv", row.names = F, quote = F) 
