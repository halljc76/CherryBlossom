# Packages
 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(forecast)
library(zoo)
library(vars)
 

## Data Cleaning and Preprocessing

 
remove_non_numeric <- function(file_path, sheet) {
  # Load the Excel file as a dataframe
  df <- read_excel(file_path, sheet)
  
  # Remove non-numeric characters from dataframe
  df_cleaned <- apply(df, 2, function(x) {
    if (is.numeric(x)) {
      return(x)
    } else {
      t <- gsub("[^0-9.]", "", x)
      t <- gsub('--', '0', x)
      return(as.numeric(t))
    }
  })
  
  # Convert cleaned dataframe to numeric type
  df_cleaned <- apply(df_cleaned, 2, as.numeric)
  
  # Return cleaned dataframe
  return(data.frame(df_cleaned))
}

plot_monthly_data <- function(df) {
  # Create an index variable for ggplot
  df$index <- seq.int(nrow(df))
  
  # Gather data for ggplot
  df_gathered <- tidyr::gather(df, key = "variable", value = "value", -index)
  
  # Plot all columns on a single ggplot
  suppressWarnings(ggplot(df_gathered, aes(x = index, y = value, color = variable)) +
                     geom_line() + facet_wrap(facets = ~variable, nrow = 3, ncol = 4))
}
 

## Data Imputation of Missing Values

 
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
 

## Reading, Cleaning Data

 
kyoto <- read.csv("../data/kyoto.csv")
kyoto_sunshine_hour <- suppressWarnings(remove_non_numeric("../data/Kyoto_data_bowen.xlsx",
                                                           sheet = "Monthly Sunshine Hour") %>%
                                          get_forecasted_data(annual_is_average = F)) %>% filter(Year >= 1961)
kyoto_max_temp <- suppressWarnings(remove_non_numeric("../data/Kyoto_data_bowen.xlsx",
                                                      sheet = " Maximum Temperature") %>%
                                     get_forecasted_data()) %>% filter(Year >= 1961)
kyoto_min_temp <- suppressWarnings(remove_non_numeric("../data/Kyoto_data_bowen.xlsx",
                                                      sheet = "Minimum Temperature") %>%
                                     get_forecasted_data()) %>% filter(Year >= 1961)
kyoto_wind_speed <- suppressWarnings(remove_non_numeric("../data/Kyoto_data_bowen.xlsx",
                                                        sheet = "Wind Speed") %>%
                                       get_forecasted_data()) %>% filter(Year >= 1961)
kyoto_monthly_solar_radiation <- suppressWarnings(remove_non_numeric("../data/Kyoto_data_bowen.xlsx",
                                                                     sheet = "Monthly mean global solar radia") %>%
                                                    get_forecasted_data()) %>% filter(Year >= 1961)
kyoto_monthly_snowfall <- suppressWarnings(remove_non_numeric("../data/Kyoto_data_bowen.xlsx",
                                                              sheet = "Monthly total of snowfall depth") %>%
                                             get_forecasted_data(annual_is_average = F)) %>% filter(Year >= 1961)
 

## Post-Processed Data with Imputed Data

# Important to note observations in this dataset for '2023' were forecasted to assist
# in the time-series forecasting process.

 
annualWithPreds <- cbind(
  sunshine = kyoto_sunshine_hour$Annual,
  minTemp = kyoto_min_temp$Annual,
  maxTemp = kyoto_max_temp$Annual,
  windSpeed = kyoto_wind_speed$Annual,
  solar = kyoto_monthly_solar_radiation$Annual,
  snow = kyoto_monthly_snowfall$Annual,
  bloom_doy = c(unname(unlist(kyoto %>% filter(year >= 1961) %>% dplyr::select(bloom_doy))), c(NA))
)
 

## VECM Code

 
# Load data from a data frame
data <- annualWithPreds[1:(nrow(annualWithPreds)-1),]

# Convert data to a time series object
y <- ts(data, start = 1961, end = 2022, frequency = 1)
jo <- ca.jo(y, type = "eigen", ecdet = "trend", K = 4, spec = "longrun")

# Estimate VECM model
vecm <- vec2var(jo, r = 2)


preds <- predict(vecm, newdata = annualWithPreds[nrow(annualWithPreds),], n.ahead = 10)
predsDF <- data.frame(year = c(2023:2032), 
                      bloom_doy = preds$fcst$bloom_doy[,1])

ciDF <- data.frame(year = c(2023:2032),
                   lower = preds$fcst$bloom_doy[,2],
                   upper = preds$fcst$bloom_doy[,3],
                   ci    = preds$fcst$bloom_doy[,4])
historyDF <- data.frame(year = c(1961:2022),  
                        bloom_doy = annualWithPreds[(1:(nrow(annualWithPreds)-1)),7])

results <- rbind(predsDF, historyDF)
results <- results[order(results$year),]
plot(results$year, results$bloom_doy, "l", ylim = c(70,120), xlab = "Year", 
     ylab = "bloom_doy", main = "VECM Forecast of Bloom DOY")
lines(ciDF$year,ciDF$lower, col = "blue")
lines(ciDF$year,ciDF$upper, col = "blue")
 

## Results

 
kyoto_preds <- data.frame(year = predsDF$year, kyoto = round(predsDF$bloom_doy))
write.csv(kyoto_preds, "./kyoto_preds.csv",row.names = F)
 


