# ---- aggregate_week2month ----

# This function aggregates a weekly data to monthly monthly.
# 'weeklyDF' is a data frame of variables to aggregate.
# 'dates' is a vector with the first day of the corresponding week.
aggregate_week2month  <- function(weeklyDF, dates) {
        # Perform temporal disagregation from weekly to daily data
        library(tempdisagg)
        denton <- lapply(weeklyDF, 
                         FUN=function(x) td(ts(x) ~ 1, method="denton-cholette", conversion="average", to=7))
        
        # Assign dates to daily data
        library(lubridate)
        days <- seq(from=as.Date(dates[1]), to=as.Date(dates[length(dates)]) + 6, by=1)
        dailyDF <- as.data.frame(lapply(denton, FUN=function(x) predict(x)))
        dailyDF <- cbind(data.frame(date=days, year=year(days), month=month(days)), dailyDF)
        
        # Restrict daily data to days of complete months
        startDate <- ceiling_date(dailyDF$date[1], unit="month")
        endDate <- floor_date(dailyDF$date[nrow(dailyDF)], unit="month") - 1
        dailyDF <- dailyDF[dailyDF$date >= startDate & dailyDF$date <= endDate, ]
        
        # Aggregate data from days to months
        monthlyDF <- aggregate(x=dailyDF[, -c(1, 2, 3)], 
                               by=list(dailyDF$month, dailyDF$year), 
                               FUN=mean)
        names(monthlyDF)[1:2] <- c("month", "year")
        
        # Return monthly data
        monthlyDF
}



aggregate_week2month  <- function(weeklyDF, dates) {
  library(lubridate)
  weeklyDF <- cbind(data.frame(year=year(as.Date(dates)), month=month(as.Date(dates))), weeklyDF)
  monthlyDF <- aggregate(x=weeklyDF[, -c(1, 2)], 
                         by=list(weeklyDF$month, weeklyDF$year), 
                         FUN=mean)
  names(monthlyDF)[1:2] <- c("month", "year")
  # Return monthly data
  monthlyDF
}
