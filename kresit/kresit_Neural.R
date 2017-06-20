
library(nnet)
neural_modelling <- function(){
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
dfs = create_features(df_mix,holidays)
print(paste0("Start:", start(dfs)," End:", end(dfs)))
start_time = "2016-01-01"
end_time = "2016-06-30"
df_clipped = clip_dataframe(dfs,start_time, end_time)
train_data <- df_clipped['2016-01-01/2016-01-31T23:59']
test_data <- df_clipped['2016-02-01/2016-06-29T23:59']
pred_output <- executeNeuralNetworks(train_data, test_data)
plot(pred_output$fit)
metric_MAPE(pred_output)

centerandscale_KreSIT <- function(df){
  # this function performs centering and scaling of data colums
  df1 <- df[,!colnames(df) %in% c("tod","dow","power")]
  df2 <- df[,c("timeofday","weekday","power")] # do not scale these variables
  if(is.zoo(df)){
    df_scaled <- xts(apply(df1,2,scale,center=TRUE,scale=TRUE),index(df))
  }else{
    df_scaled <- apply(df1,2,scale,center=TRUE,scale=TRUE)
  }
  dfs <- cbind(df_scaled,df2)
  return(dfs)
}

executeNeuralNetworks <- function(train_data,test_data) {
  
  lastday_traindata <- as.Date(last(index(train_data)),tz="Asia/Kolkata") # last day of training data
  noOfDays <-length(unique(as.Date(index(test_data),tz="Asia/Kolkata"))) # no. of predicting days 
  forecastday <- lastday_traindata + 1 # day for which forecast is needed
  ist_obs <- xts(coredata(first(train_data)),index(first(train_data))-3600)
  train_data <- rbind(ist_obs,train_data)
  suppressWarnings(rm("res_obj"))
  
  for(i in 1:noOfDays) {
    train_data_na_omit <- na.omit(train_data)
    # note in this I assign data type to factor variables. xts by defulat considers the data type of all columns
    combine_module <- function(df_formated, dtest_formated){
      mixdata <-  rbind(df_formated,dtest_formated)
      mixdata$temperature <- scale(mixdata$temperature)
      temp_df <- model.matrix(~ dow -1,mixdata)
      temp_df <- apply(temp_df,2,function(x) ifelse(x==0,-1,1))
      temp <- cbind(temp_df,tod = mixdata$tod, temperature = mixdata$temperature)
      colnames(temp) <- c(colnames(temp_df),"tod","temperature")
      return(temp)
    }
    # to be similar. so to differentiate this we reassociate the meanings
    #TRAIN MODULE
    df_formated <- as.data.frame(train_data_na_omit)
    df_formated$dow <- as.factor(df_formated$dow)
    df_formated$tod <- as.factor(df_formated$tod)
    df_formated$holiday <- as.factor(df_formated$holiday)
    # holiday variable already handled much before
    testday_data <- test_data[as.Date(index(test_data),tz="Asia/Kolkata") %in% forecastday]
    dtest_formated <- as.data.frame(testday_data)
    dtest_formated$dow <- as.factor(dtest_formated$dow)
    dtest_formated$tod <- as.factor(dtest_formated$tod)
    dtest_formated$holiday <- as.factor(dtest_formated$holiday)
    
    train_test_data <- combine_module(df_formated,dtest_formated)
    #browser()
    temp1 <- train_test_data[1:dim(df_formated)[1],]
    tempdata1 <- cbind(temp1,holiday = df_formated$holiday)
    neural_model <- nnet(x =  tempdata1, y =  df_formated$power, linout = TRUE, na.action = na.exclude, trace = FALSE, size = 15, decay = 0.01, maxit = 200)
    
    # neural_model <- nnet(x =  df_formated[,c("dow","tod","temperature","holiday")], y =  df_formated$power, linout = TRUE, na.action = na.exclude, trace = FALSE, size = 5, decay = 0.01, maxit = 500)
    
    #TEST MODULE
    
    temp2 <- tail(train_test_data,dim(dtest_formated)[1])
    tempdata2 <- cbind(temp2,holiday = dtest_formated$holiday)
    #estday_data_df <- dtest_formated[,c("dow","tod","temperature","holiday")]
    predresult <- predict(neural_model,newdata = tempdata2, class = "raw")
    #
    if (!exists('res_obj')){
      res_obj <- xts(data.frame(actual= testday_data$power,fit = as.numeric(predresult),lwr=1,upr=1),index(testday_data))
    }else{
      temp <- xts(data.frame(actual= testday_data$power,fit = as.numeric(predresult),lwr=1,upr=1),index(testday_data))
      res_obj <- rbind(res_obj,temp)
    }
    
    train_data <- rbind(train_data,testday_data) # append actual data of forecast day with previous training data
    #train_data <- train_data[-c(1:24),] # remove first day data from training data as extra day data is appended
    forecastday <- forecastday + 1 # index of next forecast day
    #browser()
  }
  return(res_obj)
}

metric_MAPE(res_obj)

metric_MAPE <- function(forecast_ob) {
  # this metric correponds to mean absolute percent error 
  return(mean(abs(forecast_ob$power - forecast_ob$fit)/forecast_ob$power,na.rm=TRUE ))
}

create_features <- function(data,holidays) {
  #data = df_mix
  dow = .indexwday(data) + 1 # one added to keep range in 1:7 instead of 0:6 for easy indexing
  tod = .indexhour(data) + .indexmin(data)/60 + .indexsec(data)/3600
  dfs <- cbind(power = data$power, dow = dow, tod = tod, temperature = data$temperature)
  dfs$holiday <- ifelse(as.Date(index(dfs),tz="Asia/Kolkata") %in% holidays, 1, -1)
  #browser()
  return(dfs)
}

clip_dataframe <- function(df, start_time, end_time) {
  #browser()
  dat = df[index(df) >= start_time & index(df) <= end_time,]
  return(dat)
}

interpolate_weather <- function(dframe) {
  # function used to interpolate weather column of dataframe
  dframe$temperature <- na.approx(dframe$temperature)
  return(dframe)
} 


