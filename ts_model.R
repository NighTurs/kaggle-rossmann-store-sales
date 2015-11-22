source("read_clean_transform.R")
require(forecast)
require(plyr)
set.seed(123)

RMPSE <- function(actual, preds) {
    y <- actual[actual > 0 & preds > 0]
    y_hat <- preds[actual > 0 & preds > 0]
    return(sqrt(mean(((y-y_hat)/y)^2)))
}

data <- load_tidy_data()
cj <- CJ(Store = unique(data$Store), 
   Date = seq(from = as.Date("2013-01-01"), to = as.Date("2015-07-13"), by = "day"))
data <- data.table(full_join(cj, data))
data$DayOfYear = as.integer(format(data$Date, "%j"))
data$Year = as.integer(format(data$Date, "%Y"))
data$Week = as.integer(format(data$Date, "%U"))
data$DayOfWeek = as.integer(format(data$Date, "%u"))
data[is.na(Sales),]$Sales <- 0
data <- arrange(data, desc(Date))

data_cl <- data[((data$Year == 2013 & data$DayOfYear %in% c(35:77, 155:265)) | 
                     (data$Year == 2014 & data$DayOfYear %in% c(34:76, 154:208, 216:271)) |
                     (data$Year == 2015 & data$DayOfYear %in% c(33:75, 153:212)))]

data_cl[data_cl$Year == 2013]$DayOfYear <- 
    mapvalues(data_cl[data_cl$Year == 2013]$DayOfYear, 
              from = c(35:77, 155:265), 
              to = 1:154)
data_cl[data_cl$Year == 2014]$DayOfYear <- 
    mapvalues(data_cl[data_cl$Year == 2014]$DayOfYear, 
              from = c(34:76, 154:208, 216:271), 
              to = 1:154)
data_cl[data_cl$Year == 2015]$DayOfYear <- 
    mapvalues(data_cl[data_cl$Year == 2015]$DayOfYear, 
              from = c(33:75, 153:212), 
              to = 1:103)

data_cl[, MeanDay:=mean(Sales[Sales > 0], na.rm = T), by = list(Store, DayOfYear)]
data_cl[, MeanWeek:=mean(Sales[Sales > 0], na.rm = T), by = list(Store, DayOfWeek)]
data_cl <- data_cl %>% mutate(Sales = 
                             replace(Sales, Sales == 0, 
                                     ifelse(is.na(data_cl[Sales == 0, MeanDay]) | 
                                                data_cl[Sales == 0, MeanDay] == 0, 
                                            data_cl[Sales == 0, MeanWeek], 
                                            data_cl[Sales == 0, MeanDay])))
data_cl$LogSales <- log(data_cl$Sales + 1)

#estimate errors

stores <- unique(data_cl$Store)
actual <- c()
preds <- c()
N <- 60
for (store in stores) {
    print(paste("Forecast for store", store, sep = " "))
    store_dt <- data_cl[Store == store & DayOfWeek != 7,]

    test <- store_dt[1:N]
    train <- store_dt[(N + 1):nrow(store_dt)]
    
    ts1 <- ts(rev(train$LogSales), frequency = 12)
    fit <- stl(ts1, s.window="periodic")
    ans <- forecast(fit, method = "ets", h = N)
#    plot(ans)
#    lines(ts(rev(store_dt$LogSales), frequency = 12), col = "red")
    actual <- c(actual, rev(test$Sales))
    preds <- c(preds, ans$mean)
    print(RMPSE(rev(test$Sales), exp(ans$mean) - 1))
}
RMPSE(actual, exp(preds) - 1)

#make predictions

test <- fread("data/test.csv", sep = ",")
test <- mutate(test, Data <- as.Date(Data))
test <- data.table(mutate(test, Sales = 0))
N <- 41

stores <- unique(data_cl$Store)
for (store in stores) {
    print(paste("Forecast for store", store, sep = " "))
    store_dt <- data_cl[Store == store & DayOfWeek != 7]
    
    ts1 <- ts(rev(store_dt$LogSales), frequency = 12)
    fit <- stl(ts1, s.window="periodic")
    ans <- forecast(fit, method = "ets", h = N)
    test <- mutate(test, Sales = replace(Sales, Store == store & DayOfWeek != 7, 
                                         exp(ans$mean) - 1))
    test <- mutate(test, Sales = replace(Sales, Store == store & DayOfWeek == 7, 
                                         mean(data[Store == store & 
                                                          DayOfWeek == 7 & 
                                                          Sales != 0, Sales])))
}

test <- mutate(test, Sales = replace(Sales, Open == 0, 0))
write.csv(test[,list(Id, Sales)], file = "ts_out.csv", row.names = F)

