library(data.table)
library(xts)
library(fasttime) #for fastPosixct
Sys.setenv(TZ="Asia/Kolkata")
read_mix_interpolate <- function() {
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


holidays <- c( "2016-01-26", "2016-03-24", "2016-03-25", "2016-04-08","2016-04-20",
               "2016-07-06", "2016-08-15", "2016-09-05", "2016-09-12","2016-10-11",
               "2016-10-12", "2016-10-31", "2016-11-01", "2016-11-14","2016-12-13"
)
holidays <- as.Date(holidays)
dfs = create_features(df_mix,holidays)
print(paste0("Start:", start(dfs)," End:", end(dfs)))
start_time = "2016-01-01"
end_time = "2016-06-30"
df_clipped = clip_dataframe(dfs,start_time, end_time)
train_data <- df_clipped['2016-01-01/2016-01-31T23:59']
test_data <- df_clipped['2016-02-01/2016-06-29T23:59']
model <- create_model(train_data)
pred_output <- execute_model(model, train_data, test_data)

# for only glm
# colnames(pred_output) <- c("power", "dow", "tod", "temperature","fit")
plot(pred_output$power)
lines(pred_output$fit,col="red")
lines(pred_output$upr,col="blue")
metric_RMSE(pred_output)
metric_MAPE(pred_output)

SEVEN_MODELS_SEP_DAYS <- function() {
  
  create_model <- function(train_data){
    library(splines)
    cat("SEPARATE MODELS")
    day_formula <- list()
    factorize <- function(din) {
      din <- as.data.frame(din)
      din$dow <- factor(din$dow,levels = as.character(1:7))
      din$tod <- factor(din$tod,levels = as.character(1:24))
      din$holiday <- factor(din$holiday,levels = as.character(c(-1,1)))
      return(din)
    }
    for (i in 1:7){
     # day_formula[[i]] <- as.formula(power ~  ns(tod, knots = c(9,18)) + ns(temperature,df = 4) )
      day_formula[[i]] <- as.formula(power ~  tod + temperature )
    }
    day_model <- list()
    for (i in 1:7){
      #day_model[[i]] <- lm(day_formula[[i]], data = train_data[train_data$dow == i,],na.action = na.exclude )
      dat <- train_data[train_data$dow == i,]
      dat <- factorize(dat)
      day_model[[i]] <- lm(day_formula[[i]], data = dat, na.action = na.exclude )
    } 
    return (day_model)
  }
  
  execute_model <- function(model, train_data, test_data){
    factorize <- function(din) {
      din <- as.data.frame(din)
      din$dow <- factor(din$dow,levels = as.character(1:7))
      din$tod <- factor(din$tod,levels = as.character(1:24))
      din$holiday <- factor(din$holiday,levels = as.character(c(-1,1)))
      return(din)
    }
    
    n_days <- length(unique(lubridate::date(test_data)))
    
    suppressWarnings(rm("agg_sequence"))
    for(i in 1:n_days) {
      test_day <- unique(lubridate::date(test_data))[i]
      #weekday <- .indexwday(test_day) + 1
      curr_testdata <- test_data[paste0(test_day,'T00:00/',test_day,'T23:59'), ]
      model_ind <-  unique(curr_testdata$dow)
      dat <- curr_testdata ; dat <- factorize(dat)
      pred_obj <- predict(model[[model_ind]], newdata = dat, interval = "predict", level = 0.95)
      test_plus_pred <- cbind(curr_testdata,pred_obj)
      # create a sequential output prediction object for n days
      if(!exists("agg_sequence")){
        agg_sequence <- test_plus_pred
      } else {
        agg_sequence <- rbind(agg_sequence,test_plus_pred)
      }
      # retrain your model
      # step 1: add recent data to train_data
      train_data <- rbind(train_data,curr_testdata)
      # step 2: remove the oldest historical day data
      daydate = unique(lubridate::date(train_data))[1]
      train_data = train_data[!(index(train_data) %in% index(daydate)),]
      # step 3: retrain model
      model <- create_model(train_data)
    }
    return(agg_sequence)
  }
  
}

SEPARATE_MODELS_DAY_PORTION <- function( ) {
  
  create_model <- function(train_data){
    cat("SEPARATE MODELS")
    day_model <- list()
    lst <- list()
    forms <- as.formula(power ~ tod + temperature )
    for (i in 1:7){
      #browser()
      factorize <- function(din) {
        din <- as.data.frame(din)
        din$dow <- factor(din$dow,levels = as.character(1:7))
        din$tod <- factor(din$tod,levels = as.character(1:24))
        din$holiday <- factor(din$holiday,levels = as.character(c(-1,1)))
        return(din)
      }
      #browser()
      dat1 <- train_data[train_data$dow == i & train_data$tod <=9,]; dat1 <- factorize(dat1)
      dat2 <- train_data[train_data$dow == i & train_data$tod > 9 & train_data$tod <= 18,]; dat2 <- factorize(dat2)
      dat3 <- train_data[train_data$dow == i & train_data$tod > 18,]; dat3 <- factorize(dat3)
      
      day_model[[1]] <- lm(forms, data = dat1,na.action = na.exclude )
      day_model[[2]] <- lm(forms, data = dat2,na.action = na.exclude )
      day_model[[3]] <- lm(forms, data = dat3,na.action = na.exclude )
      lst[i] <- list(day_model)
    }  # browser()
    return (lst)
  }
  
  execute_model <- function(model, train_data, test_data) {
    
    n_days <- length(unique(lubridate::date(test_data)))
    suppressWarnings(rm("agg_sequence"))
    for(i in 1:n_days) {
      #browser()
      test_day <- unique(lubridate::date(test_data))[i]
      #weekday <- .indexwday(test_day) + 1
      curr_testdata <- test_data[paste0(test_day,'T00:00/',test_day,'T23:59'), ]
      model_ind <-  unique(curr_testdata$dow)
      
      factorize <- function(din) {
        din <- as.data.frame(din)
        din$dow <- factor(din$dow,levels = as.character(1:7))
        din$tod <- factor(din$tod,levels = as.character(1:24))
        din$holiday <- factor(din$holiday,levels = as.character(c(-1,1)))
        return(din)
      }
      
      curr_testdata1 = curr_testdata[curr_testdata$tod <= 9, ]; curr_testdata1 <- factorize(curr_testdata1)
      curr_testdata2 = curr_testdata[curr_testdata$tod >9 & curr_testdata$tod <= 18, ]; curr_testdata2 <- factorize(curr_testdata2)
      curr_testdata3 = curr_testdata[curr_testdata$tod > 18, ]; curr_testdata3 <- factorize(curr_testdata3)
      # browser()
      if(unique(curr_testdata$holiday) == 1) { # if day is calender holiday use nearest saturday for prediction
        pred_obj1 <- predict(model[[7]][[1]], newdata = curr_testdata1, interval = "predict", level = 0.95)
        pred_obj2 <- predict(model[[7]][[2]], newdata = curr_testdata2, interval = "predict", level = 0.95)
        pred_obj3 <- predict(model[[7]][[3]], newdata = curr_testdata3, interval = "predict", level = 0.95)
      } else {
        pred_obj1 <- predict(model[[model_ind]][[1]], newdata = curr_testdata1, interval = "predict", level = 0.95)
        pred_obj2 <- predict(model[[model_ind]][[2]], newdata = curr_testdata2, interval = "predict", level = 0.95)
        pred_obj3 <- predict(model[[model_ind]][[3]], newdata = curr_testdata3, interval = "predict", level = 0.95)  
      }
      pred_obj <- rbind(pred_obj1,pred_obj2,pred_obj3)
      
      test_plus_pred <- cbind(curr_testdata,pred_obj)
      # create a sequential output prediction object for n days
      if(!exists("agg_sequence")){
        agg_sequence <- test_plus_pred
      } else {
        agg_sequence <- rbind(agg_sequence,test_plus_pred)
      }
      # retrain your model
      # step 1: add recent data to train_data
      train_data <- rbind(train_data,curr_testdata)
      # step 2: remove the oldest historical day data
      daydate = unique(lubridate::date(train_data))[1]
      train_data = train_data[!(index(train_data) %in% index(daydate)),]
      # step 3: retrain model
      model <- create_model(train_data)
    }
    return(agg_sequence)
  }
}

create_features <- function(data,holidays) {
  #data = df_mix
  dow = .indexwday(data) + 1 # one added to keep range in 1:7 instead of 0:6 for easy indexing
  tod = .indexhour(data) + .indexmin(data)/60 + .indexsec(data)/3600
  dfs <- cbind(power = data$power, dow = factor(dow), tod = factor(tod), temperature = data$temperature)
  #browser()
  dfs$temperature <- scale(dfs$temperature)
  dfs$holiday <- ifelse(as.Date(index(dfs),tz="Asia/Kolkata") %in% holidays, 1, -1)
  #browser()
  return(dfs)
}

clip_dataframe <- function(df, start_time, end_time) {
  dat = df[index(df) >= start_time & index(df) <= end_time,]
  return(dat)
}

interpolate_weather <- function(dframe) {
  # function used to interpolate weather column of dataframe
  dframe$temperature <- na.approx(dframe$temperature)
  return(dframe)
} 

metric_RMSE <- function(forecast_ob) {
  # this metric corresponds root mean square error
  return (sqrt(mean((forecast_ob$power - forecast_ob$fit)^2,na.rm=TRUE )))
}

metric_MAPE <- function(forecast_ob) {
  # this metric correponds to mean absolute percent error 
  return(mean(abs(forecast_ob$power - forecast_ob$fit)/forecast_ob$power,na.rm=TRUE ))
}

ONE_MODEL_ALL_DAYS <- function() {
  
  create_model <- function(train_data){
    cat("SINGLE MODEL")
    df <- as.data.frame(train_data)
    summary(df)
    model_formula <- as.formula(power ~ dow + tod + temperature)
    mod <- lm(model_formula, data = df,na.action = na.exclude)
    return(mod)
  }
  
  execute_model <- function(model, train_data, test_data){
    
    n_days <- length(unique(lubridate::date(test_data)))
    suppressWarnings(rm("agg_sequence"))
    for(i in 1:n_days) {
      test_day <- unique(lubridate::date(test_data))[i]
      curr_testdata <- test_data[paste0(test_day,'T00:00/',test_day,'T23:59'), ]
      pred_obj <- predict(model, newdata = curr_testdata, interval = "predict", level = 0.95)
      test_plus_pred <- cbind(curr_testdata,pred_obj)
      # create a sequential output prediction object for n days
      if(!exists("agg_sequence")){
        agg_sequence <- test_plus_pred
      } else {
        agg_sequence <- rbind(agg_sequence,test_plus_pred)
      }
      # retrain your model
      # step 1: add recent data to train_data
      train_data <- rbind(train_data,curr_testdata)
      # step 2: remove the oldest historical day data
      daydate = unique(lubridate::date(train_data))[1]
      train_data = train_data[!(index(train_data) %in% index(daydate)),]
      # step 3: retrain model
      model <- create_model(train_data)
    }
    return(agg_sequence)
  }
}

print_predicted <- function(obj){
  # this function is used to print actual, fitted, upper and lower values in a facet form
  library(lubridate)
  obj = pred_output
  months <- unique(month(obj))
  savepath <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/results/"
  warning("Ensure file name is correct")
  for(i in 1:length(months)){
    month_idx <- months[i]
    filename <- paste0("month_",month_idx,"_2016kreSIT_mainmeter_neuralNetworks.pdf")
    ggplot_function( obj[.indexmon(obj) == month_idx-1, ],filename,savepath)
  }
  ggplot_function <- function(obj,filename,savepath) {
    timestamp = .indexhour(obj) +.indexmin(obj)*30
    df <- data.frame(timestamp = timestamp, coredata(obj)[,c('power','fit','upr','lwr')],day = .indexmday(obj) )
    colnames(df) <- c("timestamp","actual","fit","upr","lwr","day")
    df_long <- reshape2::melt(df, id.vars = c("timestamp","day"))
    
    ggplot(df_long,aes(timestamp,value/1000, group = variable, color = variable )) + geom_line() + facet_wrap(~day,ncol = 7) + labs(x = "Hour of the day", y = "power(kW") + 
      scale_colour_manual(values=c('#e66101','#5e3c99','#b2abd2','#b2abd2')) + 
      theme(legend.position = c(0.7,0.1), legend.title = element_blank()) +
      ggtitle(filename)
    #scale_colour_manual(values=c('#e66101','#fdb863','#b2abd2','#5e3c99'))
    ggsave(file = filename,width=14,height = 10,path = savepath)
  }
}

metric_SMAPE <- function(object){
  numer <- sum(abs(object$fit-object$actual))
  denom <- sum(abs(object$fit)+abs(object$actual))
  smape <- numer/denom
  return (smape)
}

metric_MASE <- function(forecast_ob){
  # https://en.wikipedia.org/wiki/Mean_absolute_scaled_error 
  numerator <- abs(forecast_ob$actual - forecast_ob$fit)
  naive_error <- 0
  multiplier <- NROW(forecast_ob)/NROW(forecast_ob-1)
  for(i in 2:NROW(forecast_ob)){
    naive_difference <- abs (coredata(forecast_ob$actual[i]) - coredata(forecast_ob$actual[i-1]))
    naive_error <- naive_error + naive_difference
  }
  metric_val <- sum(numerator)/(multiplier*naive_error)
  return(metric_val)
}

effectof_previousyear <- function(){
  # df_mix contains data and weather
  power_parent <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/main_meter_kresit/mix_kresit_mainmeter_2015-2016.csv"
  previos_data<- fread(power_parent,header=TRUE)
  previos_data_xts <- xts(data.frame(previos_data$year_2015,previos_data$year_2016),fastPOSIXct(previos_data$timestamp)-19800)
  colnames(previos_data_xts) <- c("year_2015","year_2016")
  df_mix_clip <- df_mix[paste0(start(df_mix),'/',end(previos_data_xts))] 
  dframe = cbind(df_mix_clip, previos_data_xts$year_2015)
  
  holidays <- c( "2016-01-26", "2016-03-24", "2016-03-25", "2016-04-08","2016-04-20",
                 "2016-07-06", "2016-08-15", "2016-09-05", "2016-09-12","2016-10-11",
                 "2016-10-12", "2016-10-31", "2016-11-01", "2016-11-14","2016-12-13"
  )
  holidays <- as.Date(holidays)
  
  dfs = create_features2(dframe,holidays)
  print(paste0("Start:", start(dfs)," End:", end(dfs)))
  start_time = "2016-01-01"
  end_time = "2016-06-30"
  df_clipped = clip_dataframe(dfs,start_time, end_time)
  train_data <- df_clipped['2016-01-01/2016-01-31T23:59']
  test_data <- df_clipped['2016-02-01/2016-06-29T23:59']
  model <- create_model(train_data)
  pred_output <- execute_model(model, train_data, test_data)
  plot(pred_output$power)
  lines(pred_output$fit,col="red")
  lines(pred_output$upr,col="blue")
  metric_RMSE(pred_output)
  metric_MAPE(pred_output)
  
  create_features2 <- function(data,holidays) {
    #data = df_mix
    dow = .indexwday(data) + 1 # one added to keep range in 1:7 instead of 0:6 for easy indexing
    tod = .indexhour(data) + .indexmin(data)/60 + .indexsec(data)/3600
    dfs <- cbind(power = data$power, dow = factor(dow), tod = factor(tod), temperature = data$temperature, previous = data$year_2015)
    dfs$holiday <- ifelse(as.Date(index(dfs),tz="Asia/Kolkata") %in% holidays, 1, 0)
    #browser()
    return(dfs)
  }
  
  SEPARATE_MODELS_DAY_PORTION <- function( ) {
    
    create_model <- function(train_data){
      cat("SEPARATE MODELS with previous temp")
      day_model <- list()
      lst <- list()
      forms <- as.formula(power ~ tod + temperature + year_2015 )
      for (i in 1:7){
        dat1 <- train_data[train_data$dow == i & train_data$tod<=9,]
        dat2 <- train_data[train_data$dow == i & train_data$tod > 9 & train_data$tod <= 18,]
        dat3 <- train_data[train_data$dow == i & train_data$tod > 18,]
        
        day_model[[1]] <- lm(forms, data = dat1,na.action = na.exclude )
        day_model[[2]] <- lm(forms, data = dat2,na.action = na.exclude )
        day_model[[3]] <- lm(forms, data = dat3,na.action = na.exclude )
        lst[i] <- list(day_model)
      }  # browser()
      return (lst)
    }
    
    execute_model <- function(model, train_data, test_data) {
      
      n_days <- length(unique(lubridate::date(test_data)))
      suppressWarnings(rm("agg_sequence"))
      for(i in 1:n_days) {
        #browser()
        test_day <- unique(lubridate::date(test_data))[i]
        #weekday <- .indexwday(test_day) + 1
        curr_testdata <- test_data[paste0(test_day,'T00:00/',test_day,'T23:59'), ]
        model_ind <-  unique(curr_testdata$dow)
        # browser
        curr_testdata1 = curr_testdata[curr_testdata$tod <= 9, ]
        curr_testdata2 = curr_testdata[curr_testdata$tod >9 & curr_testdata$tod <= 18, ]
        curr_testdata3 = curr_testdata[curr_testdata$tod > 18, ]
        if(unique(curr_testdata$holiday) == 1) { # if day is calender holiday use nearest saturday for prediction
          pred_obj1 <- predict(model[[7]][[1]], newdata = curr_testdata1, interval = "predict", level = 0.95)
          pred_obj2 <- predict(model[[7]][[2]], newdata = curr_testdata2, interval = "predict", level = 0.95)
          pred_obj3 <- predict(model[[7]][[3]], newdata = curr_testdata3, interval = "predict", level = 0.95)
        } else {
          pred_obj1 <- predict(model[[model_ind]][[1]], newdata = curr_testdata1, interval = "predict", level = 0.95)
          pred_obj2 <- predict(model[[model_ind]][[2]], newdata = curr_testdata2, interval = "predict", level = 0.95)
          pred_obj3 <- predict(model[[model_ind]][[3]], newdata = curr_testdata3, interval = "predict", level = 0.95)  
        }
        pred_obj <- rbind(pred_obj1,pred_obj2,pred_obj3)
        
        test_plus_pred <- cbind(curr_testdata,pred_obj)
        # create a sequential output prediction object for n days
        if(!exists("agg_sequence")){
          agg_sequence <- test_plus_pred
        } else {
          agg_sequence <- rbind(agg_sequence,test_plus_pred)
        }
        # retrain your model
        # step 1: add recent data to train_data
        train_data <- rbind(train_data,curr_testdata)
        # step 2: remove the oldest historical day data
        daydate = unique(lubridate::date(train_data))[1]
        train_data = train_data[!(index(train_data) %in% index(daydate)),]
        # step 3: retrain model
        model <- create_model(train_data)
      }
      return(agg_sequence)
    }
  }
  
}

plot_KReSIT_data <- function(){
  library(lubridate)
  readdir <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/power_k_a/"
  filename <-  "hourly_complete_2016.csv"
  dfx <- fread(paste0(readdir,filename))
  obj_xts = xts(dfx$power,fastPOSIXct(dfx$timestamp)-19800)
  months <- unique(lubridate::month(obj_xts))
  metername <- "power_k_a"
  savepath <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/results/"
  savepath <- paste0(savepath,metername,"/")
  
  warning("Ensure file name is correct")
  for(i in 1:length(months)){
    month_idx <- months[i]
    filename2 <- paste0("month_",month_idx,"_2016_",metername,".pdf")
    ggplot_function( obj_xts[.indexmon(obj_xts) == month_idx-1, ],filename2,savepath)
  }
  ggplot_function <- function(obj,filename2,savepath) {
    timestamp = .indexhour(obj) 
    df <- data.frame(timestamp = timestamp, coredata(obj),day = .indexmday(obj) )
    colnames(df) <- c("timestamp","power","day")
    df_long <- reshape2::melt(df, id.vars = c("timestamp","day"))
    ggplot(df_long,aes(timestamp,value/1000)) + geom_line() + facet_wrap(~day,ncol = 7) + labs(x = "Hour of the day", y = "power(kW)") + ggtitle(filename2)
    ggsave(file = filename2,width=14,height = 10,path = savepath)
  }
}