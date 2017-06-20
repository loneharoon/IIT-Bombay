library(data.table)
library(xts)
library(fasttime)
# STEP 1
read_hourlyfiles <- function() {
  #this script searches for particular files(see grep statement) out of all meters and then copies them at a particular location
  extdir <- "/Volumes/WDFILES/KReSIT_data"
  subdirs <- list.dirs(extdir,recursive = FALSE)
  for( i in 1:length(subdirs)) {
    files <- list.files(subdirs[i])
    idx <- grep("hourly_complete_2016",files)
    if(length(idx >0)) {
      ff <- fread(paste0(subdirs[i],"/",files[idx]),header = TRUE)
      savname = substring(files[idx],22)
      savedir = "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/data/"
      write.csv(ff,paste0(savedir,savname),row.names = FALSE)
    }
  }
}
# STEP 2
process_files <- function() {
  # this file represents data from different meters in a single matrix format. Each column represents different meter
  readdir <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/data/"
  fls <- list.files(readdir)
  dat <- lapply(fls, function(x){
    dfz <- fread(paste0(readdir,x),header = TRUE)
    dfz_xts <- xts(dfz$power,fastPOSIXct(dfz$timestamp)-19800)
    newtimestamp <- vector()
    for (i in c(1:dim(dfz_xts)[1])) {
      if(.indexmin(dfz_xts[i,]) %in% c(30)) {
        newtimestamp[i] <-  index(dfz_xts[i,]) + 60*60*0.5
      } else{
        newtimestamp[i] <-  index(dfz_xts[i,]) + 0
      }
    }
    dfz_xts <- xts(round(coredata(dfz_xts),2),as.POSIXct(newtimestamp,origin = "1970-01-01"))
    dfz_xts <- dfz_xts["2016-01-01 00:00:00/"] # subsetting ensures only dates starting from some range are there
    return(dfz_xts)
  })
  dat2 <- do.call(cbind,dat)
  names_col <- unlist(lapply(fls, function(x) strsplit(substring(x,7),'[.]')[[1]][1]))
  colnames(dat2) <- names_col
  dat2$k_yc <- rowSums(cbind(dat2$k_yc_a,dat2$k_yc_p), na.rm = TRUE)
  dat2$k_erts <- rowSums(cbind(dat2$k_erts_a, dat2$k_erts_p, dat2$k_erts_l), na.rm = TRUE)
  dat2$k_sr <- rowSums(cbind(dat2$k_sr_a,dat2$k_sr_p),na.rm=TRUE)
  dat2$k_ch <- rowSums(cbind(dat2$k_ch_a,dat2$k_ch_p1,dat2$k_ch_p2),na.rm=TRUE)
  dat2$k_dil<- rowSums(cbind(dat2$k_dil_a, dat2$k_dil_p, dat2$k_dil_l), na.rm = TRUE)
  dat2$k_f2 <- rowSums(cbind(dat2$k_f2_a, dat2$k_f2_p, dat2$k_f2_l), na.rm = TRUE)
  dat2$k_fck <- rowSums(cbind(dat2$k_fck_a,dat2$k_fck_l),na.rm=TRUE)
  dat2$k_wc <- rowSums(cbind(dat2$k_wc_a, dat2$k_wc_p, dat2$k_wc_l), na.rm = TRUE)
  dat2$k_l <-  dat2$k_m - rowSums(cbind(dat2$k_a,dat2$k_p),na.rm=TRUE)
  dat2$k_meter <- rowSums(cbind(dat2$k_meter1, dat2$k_meter2, dat2$k_meter3), na.rm = TRUE)
  # df_form <- data.frame(timestamp = index(dat2),coredata(dat2))
  write.csv(df_form,file=paste0(readdir,"all_meters.csv"),row.names = FALSE)
}
#df_form <- data.frame(timestamp = index(dat2),coredata(dat2))
#df_melt <- reshape2::melt(df_form,id.vars="timestamp")
#ggplot(df_melt,aes(timestamp,value,group=variable)) + geom_line()

dir_temp <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/weather/"
dft <- fread(paste0(dir_temp,"hourlyweather_complete2016.csv"),header = TRUE)
dft_xts <- xts(dft$TemperatureC,fastPOSIXct(dft$timestamp)-19800)
dft_xts <- dft_xts["2016-01-01 00:00:00/"] # subsetting ensures only dates starting from some
dat3 <- cbind(dat2,dft_xts)
colnames(dat3) <- c(colnames(dat2),"temp")
df_form <- data.frame(timestamp = index(dat3),coredata(dat3))
write.csv(df_form,file=paste0(readdir,"all_meters.csv"),row.names = FALSE)
