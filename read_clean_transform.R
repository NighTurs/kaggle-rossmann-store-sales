require(data.table)
require(dplyr)
require(lubridate)

read_data <- function(file = "data/train.csv") {
    train <- fread(file, head = T, sep = ',')
    stores <- fread("data/store.csv", head = T, sep = ',')
    merge(train, stores, by = "Store")
}

clean_data <- function(data) {
    data$Open = factor(data$Open, levels = c(0, 1), labels = c("Closed", "Open"))
    data$Promo = factor(data$Promo, levels = c(0, 1), labels = c("None", "Promo"))
    data$StateHoliday = factor(data$StateHoliday, levels = c("0", "a", "b", "c"), 
                               labels = c("None", "Public", "Easter", "Christmas"))
    data$SchoolHoliday = factor(data$SchoolHoliday, levels = c("0", "1"), 
                                labels = c("None", "Holiday"))
    data$StoreType = factor(data$StoreType, levels = c("c", "a", "d", "b"), 
                            labels = c("C", "A", "D", "B"))
    data$Assortment = factor(data$Assortment, levels = c("a", "c", "b"), 
                             labels = c("Basic", "Extended", "Extra"))
    data$Promo2 = factor(data$Promo2, levels = c(0, 1), 
                         labels = c("None", "Promo"))
    data$PromoInterval = factor(data$PromoInterval, 
                                levels = c("", "Jan,Apr,Jul,Oct", "Feb,May,Aug,Nov", "Mar,Jun,Sept,Dec"), 
                                labels = c("None", "Jan,Apr,Jul,Oct", "Feb,May,Aug,Nov", "Mar,Jun,Sept,Dec"))
    data$Promo2SinceWeek[is.na(data$Promo2SinceWeek)] <- -1
    data$Promo2SinceYear[is.na(data$Promo2SinceYear)] <- -1
    data$CompetitionOpenSinceMonth[is.na(data$CompetitionOpenSinceMonth)] <- -1
    data$CompetitionOpenSinceYear[is.na(data$CompetitionOpenSinceYear)] <- -1
    data$CompetitionDistance[is.na(data$CompetitionDistance)] <- -1
    data$Date = as.Date(data$Date)
    data <- mutate(data, Open = replace(Open, is.na(Open), "Open"))
    data
}

transform_data <- function(data) {
    data$DayOfYear = as.integer(format(data$Date, "%j"))
    data$DayOfMonth = as.integer(format(data$Date, "%d"))
    data$Year = as.integer(format(data$Date, "%Y"))
    data$Week = as.integer(format(data$Date, "%U"))
    data$Month = as.integer(format(data$Date, "%m"))
    
    data <- mutate(data, WeekEven = factor(Week %% 2, levels = c(0, 1), 
                                             labels = c("Even", "Odd")))
    data <- mutate(data, CompetitionOpenSinceDate = 
                        as.Date(ymd(paste(data$CompetitionOpenSinceYear, 
                                          data$CompetitionOpenSinceMonth, 
                                          rep("01", nrow(data)), sep = "-"))))
    data <- mutate(data, Promo2Date = 
                        as.Date(parse_date_time(paste(data$Promo2SinceYear, 
                                                      data$Promo2SinceWeek, 
                                                      rep("1", nrow(data)), 
                                                      sep = "-"), "%Y %U %u")))
    #add Promo2Days
    data <- data %>% mutate(Promo2Days =
                                as.integer(difftime(Date, Promo2Date, units = "days"))) %>%
        mutate(Promo2Days = replace(Promo2Days, is.na(Promo2Days) | Promo2Days < 0, -1))
    #add Promo2Started
    data <- data %>% mutate(Promo2Started = factor(Date >= Promo2Date, levels = c(T, F), 
                                                   labels = c("Started", "Not"))) %>%
        mutate(Promo2Started = replace(Promo2Started, is.na(Promo2Started), "Not"))
    #add CompetitionOpenDays
    data <- data %>% mutate(CompetitionOpenDays = 
                                as.integer(difftime(Date, 
                                                    CompetitionOpenSinceDate, 
                                                    units = "days"))) %>%
        mutate(CompetitionOpenDays = replace(CompetitionOpenDays, 
                                             is.na(CompetitionOpenDays) | CompetitionOpenDays < 0, 
                                             -1))
    #add CompetitionOpen
    data <- data %>% mutate(CompetitionOpen = factor(Date >= CompetitionOpenSinceDate, 
                                                     levels = c(T, F), 
                                                     labels = c("Open", "Not"))) %>%
        mutate(CompetitionOpen = replace(CompetitionOpen, is.na(CompetitionOpen), "Not"))
    data
}

transform_data_dataset_specific <- function(train, test) {
    # WeekDaySalesHighMedian and WeekDaySalesHighMedian
    data <- copy(train)
    plusWeekSales <- data.table(Store = data$Store, 
                                Date = data$Date + 7, PlusWeekSales = data$Sales)
    minusWeekSales <- data.table(Store = data$Store,
                                 Date = data$Date - 7, MinusWeekSales = data$Sales)
    data <- left_join(data, plusWeekSales, by = c("Store", "Date"))
    data <- left_join(data, minusWeekSales, by = c("Store", "Date"))
    data <- mutate(data, PlusWeekSales = replace(PlusWeekSales, is.na(PlusWeekSales), 0),
                   MinusWeekSales = replace(MinusWeekSales, is.na(MinusWeekSales), 0))
    data <- mutate(data, SalesLevel = factor((PlusWeekSales == 0 | PlusWeekSales > Sales) & 
                                                 (MinusWeekSales == 0 | MinusWeekSales > Sales), 
                                             levels = c(T, F), labels = c("Low", "High")))
    levelSales <- data %>% group_by(Store, DayOfWeek) %>% 
        summarise(WeekDaySalesHighMedian = as.double(median(Sales[SalesLevel == "High" & Sales > 0])), 
                  WeekDaySalesLowMedian = as.double(median(Sales[SalesLevel == "Low" & Sales > 0])))
    train <- left_join(train, levelSales, by = c("Store", "DayOfWeek"))
    test <- left_join(test, levelSales, by = c("Store", "DayOfWeek"))
    # WeekDaySalesHighMedianLog and WeekDaySalesHighMedianLog
    train <- mutate(train, WeekDaySalesHighMedianLog = log(WeekDaySalesHighMedian + 1), 
                    WeekDaySalesLowMedianLog = log(WeekDaySalesLowMedian + 1))
    test <- mutate(test, WeekDaySalesHighMedianLog = log(WeekDaySalesHighMedian + 1), 
                    WeekDaySalesLowMedianLog = log(WeekDaySalesLowMedian + 1))
    train <- mutate(train, WeekDaySalesHighMedianLog = 
                        replace(WeekDaySalesHighMedianLog, 
                                is.na(WeekDaySalesHighMedianLog), -1))
    test <- mutate(test, WeekDaySalesHighMedianLog = 
                        replace(WeekDaySalesHighMedianLog, 
                                is.na(WeekDaySalesHighMedianLog), -1))
    # LogSales for train
    train$LogSales <- log(train$Sales + 1)
    # WeekDayPromoMedianSales
    feature_group <- train %>% group_by(Store, DayOfWeek, Promo) %>% 
        summarise(WeekDayPromoMedianSales = median(as.double(LogSales)))
    train <- left_join(train, feature_group)
    test <- left_join(test, feature_group)
    # WeekDayPromoMedianSalesResidue    
    train <- mutate(train, WeekDayPromoMedianSalesResidue = 
                        LogSales - WeekDayPromoMedianSales)
    # WeekDaySalesPromoMedianLog and WeekDaySalesNonPromoMedianLog
    data <- copy(train)
    promoSales <- data %>% group_by(Store, DayOfWeek) %>% 
        summarise(WeekDaySalesPromoMedian = as.double(median(Sales[Promo == "Promo" & Sales > 0])), 
                  WeekDaySalesNonPromoMedian = as.double(median(Sales[Promo == "None" & Sales > 0])))
    promoSales[is.na(promoSales)] <- -1
    train <- left_join(train, promoSales, by = c("Store", "DayOfWeek"))
    test <- left_join(test, promoSales, by = c("Store", "DayOfWeek"))
    train <- mutate(train, WeekDaySalesPromoMedianLog = log(WeekDaySalesPromoMedian + 1), 
                    WeekDaySalesNonPromoMedianLog = log(WeekDaySalesNonPromoMedian + 1))
    test <- mutate(test, WeekDaySalesPromoMedianLog = log(WeekDaySalesPromoMedian + 1), 
                   WeekDaySalesNonPromoMedianLog = log(WeekDaySalesNonPromoMedian + 1))
    list(train = train, test = test)
}

save_tidy_data <- function() {
    train <- read_data()
    train <- clean_data(train)
    train <- transform_data(train)
    test <- read_data("data/test.csv")
    test <- clean_data(test)
    test <- transform_data(test)
    pair <- transform_data_dataset_specific(train, test)
    train <- pair$train
    test <- pair$test
    write.csv(test, file = "data/test_tidy.csv", row.names = F)
    write.csv(train, file = "data/train_tidy.csv", row.names = F)
}

restore_after_load <- function(data) {
    data$Date = as.Date(data$Date)
    data$Open = as.factor(data$Open)
    data$Promo = as.factor(data$Promo)
    data$StateHoliday = as.factor(data$StateHoliday)
    data$SchoolHoliday = as.factor(data$SchoolHoliday)
    data$StoreType = as.factor(data$StoreType)
    data$Assortment = as.factor(data$Assortment)
    data$Promo2 = as.factor(data$Promo2)
    data$PromoInterval = as.factor(data$PromoInterval)
    data$WeekEven = as.factor(data$WeekEven)
    data$CompetitionOpen = as.factor(data$CompetitionOpen)
    data$Promo2Started = as.factor(data$Promo2Started)
    data
}

load_tidy_train <- function() {
    data <- fread("data/train_tidy.csv", head = T, sep = ',')
    restore_after_load(data)
}

load_tidy_test <- function() {
    data <- fread("data/test_tidy.csv", head = T, sep = ',')
    restore_after_load(data)
}


