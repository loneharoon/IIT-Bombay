library(forecast)

arima_modelling <- function(){
  power_parent <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/main_meter_kresit/"
  weather_parent <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/weather/"
  #read power data
  meter <- "hourly_complete_2016.csv"
  metername <- paste0(power_parent,meter)
  df <- fread(metername,header = TRUE) # reading timestamp and power
  df_xts <- xts(round(df$power,2),as.POSIXct(df$timestamp, tz="Asia/Kolkata",origin = "1970-01-01"))
  stopifnot(!length(unique(.indexmin(df_xts))) > 1)
  # read weather data
  weather <- "hourlyweather_complete2016.csv"
  weatherfile <- paste0(weather_parent,weather)
  df_weather <- fread(weatherfile, select = c('timestamp','TemperatureC'),header = TRUE) 
  df_weatherxts <- xts(round(df_weather$TemperatureC,2),as.POSIXct(df_weather$timestamp, tz="Asia/Kolkata",origin = "1970-01-01"))
  stopifnot(!length(unique(.indexmin(df_weatherxts))) > 1)
  
  df_mix <- cbind(power = df_xts,temperature = df_weatherxts)
  df_mix <- interpolate_weather(df_mix)
  summary(df_mix)
  
}

holidays_2016 <- c( "2016-01-26", "2016-03-24", "2016-03-25", "2016-04-08","2016-04-20",
                    "2016-07-06", "2016-08-15", "2016-09-05", "2016-09-12","2016-10-11",
                    "2016-10-12", "2016-10-31", "2016-11-01", "2016-11-14","2016-12-13"
)
holidays <- as.Date(holidays_2016)
dfs = create_featuresXX(df_mix,holidays)
print(paste0("Start:", start(dfs)," End:", end(dfs)))
start_time = "2016-01-01"
end_time = "2016-06-30"
df_clipped = clip_dataframe(dfs,start_time, end_time)
train_data <- df_clipped['2016-01-01/2016-01-31T23:59']
test_data <- df_clipped['2016-02-01/2016-02-28T23:59']
model <- create_modelXX(train_data)
pred_output <- execute_modelXX(model, train_data, test_data)

executeARIMA <- function(train_data,test_data) {
  #numcores <- detectCores() - 1 #no. of cores used for computation 
  lastday_traindata <- as.Date(last(index(train_data)),tz="Asia/Kolkata") # last day of training data
  noOfDays <-length(unique(as.Date(index(test_data),tz="Asia/Kolkata"))) # no. of predicting days 
  forecastday <- lastday_traindata+1 # day for which forecast is needed
  ist_obs <- xts(coredata(first(train_data)),index(first(train_data))-3600)
  train_data <- rbind(ist_obs,train_data)
  rm("res_obj")
  for(i in 1:noOfDays) {
    # create time series object, 48 is seasonality i.e, 48 observations contstitue one season
    #tsobject <- ts(coredata(train_data$power), frequency = 144) 
    #temperature <- coredata(train_data$temperature)
    power_dat <- ts(coredata(train_data$power), frequency = 24)
    temp_dat <- ts(coredata(train_data$temperature), frequency = 24)
    holi_dat <- ts(coredata(train_data$holiday), frequency = 24)
    arimaObject <- Arima(power_dat, order = c(1,1,0), xreg = data.frame(temperature = temp_dat, holiday = holi_dat), method = "CSS")
    
    # model arima  object, with temperature being only external variable
    #arimaObject <- auto.arima(tsobject,xreg=temperature,stepwise = TRUE,parallel=FALSE)
    # arimaObject <- auto.arima(tsobject,xreg=temperature,stepwise = FALSE,parallel=TRUE,num.cores = numcores)
    # extract actual power consumption and temperature on forecast day
    testday_data <- test_data[as.Date(index(test_data),tz="Asia/Kolkata") %in% forecastday]
    
    power_dat_test <- ts(coredata(testday_data$power), frequency = 24)
    temp_dat_test <- ts(coredata(testday_data$temperature), frequency = 24)
    holi_dat_test <- ts(coredata(testday_data$holiday), frequency = 24)
    
    #  browser()
    predresult <- predict(arimaObject,n.ahead = 24, newxreg = data.frame(temperature=temp_dat_test, holiday = holi_dat_test))
    #predict(arimaObject,n.ahead = 48,interval = "predict", level=0.95)
    if (!exists('res_obj')){
      res_obj <- xts(data.frame(actual= testday_data$power,fit = as.numeric(predresult$pred),lwr=1,upr=1),index(testday_data))
    }else{
      temp <- xts(data.frame(actual= testday_data$power,fit = as.numeric(predresult$pred),lwr=1,upr=1),index(testday_data))
      res_obj <- rbind(res_obj,temp)
    }
    
    train_data <- rbind(train_data,testday_data) # append actual data of forecast day with previous training data
    train_data <- train_data[-c(1:24),] # remove first day data from training data as extra day data is appended
    forecastday <- forecastday + 1 # index of next forecast day
    #browser()
  }
  return(res_obj)
}
metric_MAPE(res_obj)
metric_MAPE <- function(forecast_ob){
  # this metric correponds to mean absolute percent error 
  return(mean(abs(forecast_ob$power - forecast_ob$fit)/forecast_ob$power,na.rm=TRUE ))
}

create_featuresXX <- function(data, holidays) {
  dfs <- cbind(power = data$power, temperature = data$temperature)
  dfs$holiday <- ifelse(as.Date(index(dfs),tz="Asia/Kolkata") %in% holidays, 1, 0)
  return(dfs)
}

clip_dataframe <- function(df, start_time, end_time){
  dat = df[index(df) >= start_time & index(df) <= end_time,]
  return(dat)
}

interpolate_weather <- function(dframe){
  # function used to interpolate weather column of dataframe
  dframe$temperature <- na.approx(dframe$temperature)
  return(dframe)
} 

