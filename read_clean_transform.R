require(data.table)
require(dplyr)

read_data <- function(file = "data/train.csv") {
    train <- fread("data/train.csv", head = T, sep = ',')
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
    
    
    
    data$DayOfYear = as.integer(format(data$Date, "%j"))
    data$Year = as.integer(format(data$Date, "%Y"))
    data$Week = as.integer(format(data$Date, "%U"))
    data$Month = as.integer(format(data$Date, "%m"))
    tmp <- paste(data$Promo2SinceYear, data$Promo2SinceWeek, rep("1", nrow(data)), sep = " ")
    data$Promo2Date <- do.call(c, lapply(tmp, function(d) {as.Date(d, format = "%Y %U %u")}))
    data$Promo2Days <- as.integer(difftime(data$Date, data$Promo2Date, units = "days"))
    data
}

transform_data <- function(data) {
    data$DayOfYear = as.integer(format(data$Date, "%j"))
    data$Year = as.integer(format(data$Date, "%Y"))
    data$Week = as.integer(format(data$Date, "%U"))
    data$Month = as.integer(format(data$Date, "%m"))
    tmp <- paste(data$Promo2SinceYear, data$Promo2SinceWeek, rep("1", nrow(data)), 
                 sep = " ")
    data$Promo2Date <- do.call(c, 
                               lapply(tmp, function(d) {as.Date(d, format = "%Y %U %u")}))
    data$Promo2Days <- as.integer(difftime(data$Date, data$Promo2Date, units = "days"))
    data
}

save_tidy_data <- function() {
    data <- read_data()
    data <- clean_data(data)
    data <- transform_data(data)
    write.csv(data, file = "data/data_tidy.csv", row.names = F)
}

load_tidy_data <- function() {
    data <- fread("data/data_tidy.csv", head = T, sep = ',')
    data$Date = as.Date(data$Date)
    data$Open = as.factor(data$Open)
    data$Promo = as.factor(data$Promo)
    data$StateHoliday = as.factor(data$StateHoliday)
    data$SchoolHoliday = as.factor(data$SchoolHoliday)
    data$StoreType = as.factor(data$StoreType)
    data$Assortment = as.factor(data$Assortment)
    data$Promo2 = as.factor(data$Promo2)
    data$PromoInterval = as.factor(data$PromoInterval)
    data
}


