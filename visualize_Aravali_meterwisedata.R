library(data.table)
library(xts)
parent_dir = "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Aravali_iitb/month_9/aggregate/"
write_path <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Aravali_iitb/month_9/power/"
meters <- list.files(parent_dir)
for(i in 2:length(meters)) {
meter_no = strsplit(meters[i],"[.]")[[1]][1]
meter_name = paste0(parent_dir,meter_no,".csv.gz")

data = read.csv(gzfile(meter_name),header = TRUE)
df = data.frame(timestamp = as.POSIXct(data$TS2,tz="Asia/Kolkata",origin = "1970-01-01"),power = data$W1+data$W2+data$W3)
write.csv(df,file = paste0(write_path, meter_no,".csv"),row.names = FALSE)
#power_frame  <- xts(df$power,df$timestamp)
}