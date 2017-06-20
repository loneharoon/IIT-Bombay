# THIS SCRIPT READS POWER DATA OF YEAR 2015 AND 2016 AND THEN IT MATCHES THE 2015 AND 2016 DATA WITH LOGIC WRITTEN IN RULES 1 AND 2

power_parent <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/main_meter_kresit/"
weather_parent <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/weather/"
#read power data
meter1 <- "hourly_complete_2015.csv"
metername1 <- paste0(power_parent,meter1)
df1 <- fread(metername1,header = TRUE) # reading timestamp and power
df_xts1 <- xts(round(df1$power,2),as.POSIXct(df1$timestamp, tz="Asia/Kolkata",origin = "1970-01-01"))
stopifnot(!length(unique(.indexmin(df_xts1))) > 1)

meter2 <- "hourly_complete_2016.csv"
metername2 <- paste0(power_parent,meter2)
df2 <- fread(metername2,header = TRUE) # reading timestamp and power
df_xts2 <- xts(round(df2$power,2),as.POSIXct(df2$timestamp, tz="Asia/Kolkata",origin = "1970-01-01"))
stopifnot(!length(unique(.indexmin(df_xts2))) > 1)

df1_clip = clip_dataframe(df_xts1,"2015-01-01", "2015-07-02 23:59:59")
df1_clip = create_complete_sequence(df1_clip)
df2_clip = clip_dataframe(df_xts2,"2016-01-01", "2016-06-30 23:59:59")
df2_clip = create_complete_sequence(df2_clip)



create_complete_sequence <- function(df_seq) {
  # this function introduces NAs at missing timings
  start = start(df_seq)
  end = end(df_seq)
  seqidx = seq(from =  start, to = end, by = "hour")
  vec <- rep(NA,NROW(seqidx))
  xts_ser <- xts(vec,seqidx)
  temp = cbind(df_seq,xts_ser)
  return(temp[,1])
  #df <- xts(rowSums(temp,na.rm = TRUE),index(temp))
} 

no_days <- length(unique(lubridate::date(df2_clip)))
holidays_2016 <- c( "2016-01-26", "2016-03-24", "2016-03-25", "2016-04-08","2016-04-20",
                    "2016-07-06", "2016-08-15", "2016-09-05", "2016-09-12","2016-10-11",
                    "2016-10-12", "2016-10-31", "2016-11-01", "2016-11-14","2016-12-13"
)
holidays_2015 <- c( "2015-01-26", "2015-03-06", "2015-04-02", "2015-04-03","2015-05-01",
                    "2015-05-04", "2015-09-17","2015-09-25",
                    "2015-10-02", "2015-10-22", "2015-11-11","2015-11-12",
                    "2015-11-13", "2015-11-25", "2015-12-25"
)


holidays_2016 = as.Date(holidays_2016)
holidays_2015 = as.Date(holidays_2015)
df2_first_row = xts(coredata(df2_clip[1,]), index(df2_clip[1,]) - 3600)
df2_clip <- rbind(df2_first_row,df2_clip)
rm("mix_data")

for(i in 1:no_days) {
  
  curr_day <- unique(lubridate::date(df2_clip))[i]
  hist_day <- unique(lubridate::date(df1_clip))[i+1]
  curr_data <- df2_clip[paste0(curr_day,'T00:00/',curr_day,'T23:59'), ]
  # rule1: if current day is holiday ensure historical day is holiday as well
  if( curr_day %in% holidays_2016) {
    forge_day = as.Date(paste0(lubridate::year(hist_day),'-',lubridate::month(curr_day) ,'-', lubridate::day(curr_day)))
    cat("forge_day")
    hist_data <- df1_clip[paste0(forge_day,'T00:00/',forge_day,'T23:59'), ]
  } else {
    hist_data <- df1_clip[paste0(hist_day,'T00:00/',hist_day,'T23:59'), ]
  }
  # rule 2: if historical day is holiday and current day is not holiday then simply use data of some other
  # working day
  if(hist_day %in% holidays_2015)  {
    forge_day = as.Date(hist_day) - 7
    hist_data <- df1_clip[paste0(forge_day,'T00:00/',forge_day,'T23:59'), ]
    cat("forge_day second")
  }
  #print(paste0(curr_day,'::',hist_day))
  temp_data <- cbind(curr_data,coredata(hist_data))
  if(!exists("mix_data")) {
    mix_data <- temp_data
  } else {
    mix_data <- rbind(mix_data,temp_data)
  }
}
plot(mix_data[,1])
lines(mix_data[,2],col="red")

dfs <- data.frame(timestamp = index(mix_data),coredata(mix_data))
colnames(dfs) <- c("timestamp", "year_2016","year_2015")
write.csv(dfs,file = "mix_kresit_mainmeter_2015-2016.csv",row.names = FALSE)

sub <- mix_data["2016-01-01/2016-01-30"]
sub2 <- as.matrix(data.frame(coredata(sub)))
ccf(sub2[,1],sub2[,2])