

rm(list=ls())
library(xts)
library(data.table)
Sys.setenv(TZ="Asia/Kolkata")
power_correlation <- function() {
power_parent <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/main_meter_kresit/"
#read power data
meter <- "hourly_complete_2015.csv"
metername <- paste0(power_parent,meter)
df1 <- fread(metername,header = TRUE) # reading timestamp and power
df_xts <- xts(round(df1$power,2),as.POSIXct(df1$timestamp, tz="Asia/Kolkata",origin = "1970-01-01"))
stopifnot(!length(unique(.indexmin(df_xts))) > 1)

meter2 <- "hourly_complete_2016.csv"
metername2 <- paste0(power_parent,meter2)
df2 <- fread(metername2,header = TRUE) # reading timestamp and power
df_xts2 <- xts(round(df2$power,2),as.POSIXct(df2$timestamp, tz="Asia/Kolkata",origin = "1970-01-01"))
stopifnot(!length(unique(.indexmin(df_xts2))) > 1)

period.apply(df_xts,INDEX = )
res = apply.monthly(df_xts, function(x){
  return (x)
})

}

weather_correlation <- function() {
weather_parent <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/weather/"
weather <- "hourlyweather_complete2015.csv"
weatherfile <- paste0(weather_parent,weather)
df_weather <- fread(weatherfile, select = c('timestamp','TemperatureC'),header = TRUE) 
df_weatherxts <- xts(round(df_weather$TemperatureC,2),as.POSIXct(df_weather$timestamp, tz="Asia/Kolkata",origin = "1970-01-01"))
stopifnot(!length(unique(.indexmin(df_weatherxts))) > 1)

weather2 <- "hourlyweather_complete2016.csv"
weatherfile2 <- paste0(weather_parent,weather2)
df_weather2 <- fread(weatherfile2, select = c('timestamp','TemperatureC'),header = TRUE) 
df_weatherxts2 <- xts(round(df_weather2$TemperatureC,2),as.POSIXct(df_weather2$timestamp, tz="Asia/Kolkata",origin = "1970-01-01"))
stopifnot(!length(unique(.indexmin(df_weatherxts2))) > 1)
}




df_mix <- cbind(power = df_xts,temperature = df_weatherxts)
df_mix <- interpolate_weather(df_mix)
summary(df_mix)