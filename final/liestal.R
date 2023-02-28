library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(forecast)
library(zoo)
library(vars)


# Functions
get_forecasted_data <- function(df, annual_is_average = T) {
  # Given a dataframe of monthly observations with the current year partially
  # filled in, forecast any NA values from months that have not yet been recorded
  # in the current year.
  
  forecasts <- c() # Store forecasts for this year
  
  for (col in colnames(df)[-c(1,ncol(df))]) { # For each month
    
    # Impute historical NA
    df[[col]][1:nrow(df)-1] <- na.aggregate(df[[col]][1:nrow(df)-1], FUN = mean) 
    
    # If month has not happened yet,
    if (is.na(df[[col]][nrow(df)])) {
      
      # Make time series with annual frequency
      temp <- ts(df[[col]][1:nrow(df)-1],frequency = 1) # Monthly data
      
      # Fit an ARIMA(p,d,q) model
      ts <- auto.arima(temp) 
      
      # Forecast the next observation 
      forecasted <- forecast(object = ts) # Forecast the observation
      forecasted <- forecasted$mean[length(forecasted$mean)]
      forecasts <- append(forecasts, forecasted)
    } else {
      forecasts <- append(forecasts, df[[col]][nrow(df)]) 
    }
  }
  
  if (annual_is_average) {
    # Ensure no 'annual' values are 'na'
    for (j in 1:(nrow(df)-1)) {
      if (is.na(df[j,ncol(df)])) {
        df[j,ncol(df)] <- sum(df[j,2:(ncol(df)-1)]) / length(2:(ncol(df)-1))
      }
    }
    df[nrow(df),2:ncol(df)] <- c(forecasts, mean(forecasts))
  } else {
    # Ensure no 'annual' values are 'na'
    for (j in 1:(nrow(df)-1)) {
      if (is.na(df[j,ncol(df)])) {
        df[j,ncol(df)] <- sum(na.omit(df[j,2:(ncol(df)-1)]))
      }
    }
    df[nrow(df),2:ncol(df)] <- c(forecasts, sum(forecasts))
  }
  return(df)
}

# Data

liestal <- read.csv("../data/liestal_cleaned.csv") 
liestal <- data.frame(liestal[,-1]) %>% dplyr::select(-c("lat", "long", "bloom_date", "location", "alt", "MONTH", "ELEVATION", "LATITUDE", "LONGITUDE", "year", "DP01", "DP10", "CDSD", "TAVG", "DT32", "DX70", "EMXT", "HDSD", "EMNT", "PRCP"))
liestal

temp <- data.frame(t(c(80,rep(NA,9))))
names(temp) <- names(liestal)
temp

liestal <- rbind(liestal, temp)

liestal <- liestal %>% get_forecasted_data()
annualWithPreds <- liestal # For consistency

# Load data from a data frame
data <- annualWithPreds
data <- cbind(data[2:ncol(data)], bloom_doy = data$bloom_doy)
data
# Convert data to a time series object
y <- ts(data, start = 1961, end = 2022, frequency = 1)
jo <- ca.jo(y, type = "trace", ecdet = "trend", K = 3, spec = "longrun")

# Estimate VECM model
vecm <- vec2var(jo, r = 2)


preds <- predict(vecm, n.ahead = 10)
predsDF <- data.frame(year = c(2023:2032), 
                      bloom_doy = preds$fcst$bloom_doy[,1])

ciDF <- data.frame(year = c(2023:2032),
                   lower = preds$fcst$bloom_doy[,2],
                   upper = preds$fcst$bloom_doy[,3],
                   ci    = preds$fcst$bloom_doy[,4])
historyDF <- data.frame(year = c(1901:2022),  
                        bloom_doy = annualWithPreds[,1])

results <- rbind(predsDF, historyDF)
results <- results[order(results$year,decreasing = F),]
historyDF
plot(results$year, results$bloom_doy, "l", ylim = c(35,140), xlab = "Year", 
     ylab = "bloom_doy", main = "VECM Forecast of Bloom DOY")
lines(ciDF$year,ciDF$lower, col = "blue")
lines(ciDF$year,ciDF$upper, col = "blue")

liestal_preds <- data.frame(year = predsDF$year, liestal = round(predsDF$bloom_doy))
write.csv(liestal_preds, "./liestal_preds.csv",row.names = F)



