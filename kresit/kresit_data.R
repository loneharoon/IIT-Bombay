library(data.table)
library(xts)
library(fasttime) #for fastPosixct
Sys.setenv(TZ="Asia/Kolkata")
library(ggplot2)

data_cleaning <- function(){
  # steps: 1:= adds header to each file within each subfolder
  parent <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_oct/"
  filp <- 29
  files <- list.files(paste0(parent,filp),pattern = "*.csv")
  filename <- paste0(parent,filp,"/",files[1])
  df <- fread(filename)
  df$V4 <- as.POSIXct(df$V4,tz="Asia/Kolkata",origin = "1970-01-01")
  #df[,1:6,with=FALSE]
  labels_kresit <- c('srl','TS1','SQNo','TS2','VA','W','VAR','PF','VLL','VLN','A','F','VA1','W1','VAR1','PF1','V12','V1','A1','VA2','W2','VAR2','PF2','V23','V2','A2','VA3','W3','VAR3','PF3','V31','V3','A3','FwdVAh','FwdWh','FwdVARhR','FwdVARhC')
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/")
  filesp = list.files(parent, pattern=".csv")
}

insert_header <- function( ) {
  parent <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/seil_sep/"
  dirs = list.files(parent)
  for (i in 1:length(dirs)){
    filesl <- list.files(paste0(parent,dirs[i]),pattern = ".csv")
    lapply(filesl, function(x){
      df = read.csv(paste0(parent,dirs[i],"/",x))
      colnames(df) <- labels_kresit
      meterno <- strsplit(x,"[_]")[[1]][1]
      write.csv(df,file=paste0(parent,dirs[i],"/",meterno,".csv"),row.names = FALSE)
      file.remove(paste0(parent,dirs[i],"/",x))
      return(0)
    }) 
  }
}

understand_data_redundancy <- function(){
  parent_dir = "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/sept/"
  file = "24/1.csv"
  df1 <- fread(paste0(parent_dir,file),header = TRUE)
  df1$TS2 <- as.POSIXct(df1$TS2,tz="Asia/Kolkata",origin = "1970-01-01")
  df1[,1:6,with=FALSE]
  dim(df1)
}

combine_main_meterdata <- function(){
  # METER NO 1 IS  A MAIN METER OF THE BUILDING
  # BETTER use function combine_remaining_meterdata to combine all the meters data
  dirs = c(1:31)
  parent <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/sept/"
  for (i in 1:length(dirs)){
    if(i==1)
      temp = fread(paste0(parent,dirs[i],"/1.csv"),header = TRUE)
    else
      temp = rbind(temp,fread(paste0(parent,dirs[i],"/1.csv"),header = TRUE))
  }
  write.csv(temp,file=paste0(parent,"aggregate/","1.csv"),row.names = FALSE)
}

combine_remaining_meterdata <- function(){
  # this function combines data of different days into one file
  # Also this function creates another csv which contain only power and timestamp
  parent <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_oct/"
  writedir <- "/Volumes/WDFILES/Kresit_monthwise/"
  
  meters = c(1:39) # are indidual meters
  for (i in 1:length(meters)){
    
    days <- c(1:31) # days in month
    ff <- lapply(days, function(x) {
      if(file.exists(paste0(parent,x,"/",meters[i],".csv"))){
        cat(paste0(i,":",x,";"))
        df <- read.csv(paste0(parent,x,"/",meters[i],".csv"))
        return(df)
      }
    })
    agg_df <- do.call(rbind,ff) # merge files 
    unique(agg_df$srl) # ensure that files are from a single meter only
    warning("ensure everything is ok")
    agg2 <- agg_df[agg_df$srl==meters[i],] # REMEMBER THIS funda fails in SEIL meters take care and for seil
    #meters perform manually
    unique(agg2$srl)
    fwrite(agg2,paste0(writedir,"oct/",meters[i],".csv"),row.names = FALSE)
    timestamp <- as.POSIXct(agg2$TS2,origin = "1970-01-01")
    df_xts <- xts(round(agg2$W,2),timestamp)
    sd <- resample_data(df_xts,60)
    newtimestamp <- vector()
    for (j in c(1:dim(sd)[1])) {
      if(.indexmin(sd[j,]) %in% c(30)) {
        newtimestamp[j] <-  index(sd[j,]) + 60*60*0.5
      } else{
        newtimestamp[j] <-  index(sd[j,]) + 0
      }
    }
    sd2 <- xts(coredata(sd),as.POSIXct(newtimestamp,origin = "1970-01-01"))
    sd2 <- data.frame( as.character(index(sd2)), round(coredata(sd2),2) )
    
    colnames(sd2) <- c("timestamp","power")
    fwrite(sd2,paste0(writedir,"oct/",meters[i],"_power.csv"),row.names = FALSE)
    
  }
  
}

process_files <- function() {
  # this file represents data from different meters in a single matrix format. Each column represents different meter. This script is a copy used to process data downloaded from mongodb. Output is same but some steps are differnt on basis of differnt data pre processing steps
  readdir <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/sept_oct_agg/"
  fls <- list.files(readdir)
  library(gtools) # required to sort names http://stackoverflow.com/a/15944950/3317829
  fls <- mixedsort(fls)
  dat <- lapply(fls, function(x){
    dfz <- fread(paste0(readdir,x),header = TRUE)
    dfz_xts <- xts(dfz$power,fastPOSIXct(dfz$timestamp)-19800)
    dfz_xts <- dfz_xts["2016-09-15 00:00:00/"] # subsetting ensures only dates starting from some range are there
    dfz_xts <- na.omit(dfz_xts) # removes NA readings, these create problems as of now
    return(dfz_xts)
  })
  
  dat2 <- do.call(cbind,dat)
  warning("Make ensure that columns are labelled correctly")
  # Names copied from the excel sheet in order
  meter_names <- c("power_k_m", "power_k_a", "power_k_p", "power_k_wc_l", "power_k_wc_a", 
                   "power_k_wc_p", "power_k_f2_l", "power_k_f2_a", "power_k_f2_p", 
                   "power_k_yc_p", "power_k_yc_a", "power_k_sr_a", "power_k_sr_p", 
                   "power_k_erts_p", "power_k_erts_l", "power_k_erts_a", "power_k_fck_a", 
                   "power_k_fck_l", "power_k_off_l", "power_k_off_a", "power_k_cr_a1", 
                   "power_k_cr_a2", "power_k_ch_p1", "power_k_ch_p2", "power_k_ch_l", 
                   "power_k_dil_l", "power_k_dil_a", "power_k_dil_p", "power_k_clsrm_ac1", 
                   "power_k_clsrm_ac2", "power_k_clsrm_ac3", "power_k_ch_a", "power_k_lab_od1", 
                   "power_k_lab_od2", "power_k_lab_od3", "power_k_meter1", "power_k_meter2", 
                   "power_k_meter3")
  names_col <- unlist(lapply(meter_names, function(x) strsplit(substring(x,7),'[.]')[[1]][1]))
  stopifnot(length(colnames(dat2)) == length(meter_names))
  colnames(dat2) <- names_col
  dat2$k_l <-  dat2$k_m - rowSums(cbind(dat2$k_a,dat2$k_p),na.rm=TRUE)
  dat2$k_wc <- rowSums(cbind(dat2$k_wc_a, dat2$k_wc_p, dat2$k_wc_l), na.rm = TRUE)
  dat2$k_f2 <- rowSums(cbind(dat2$k_f2_a, dat2$k_f2_p, dat2$k_f2_l), na.rm = TRUE)
  dat2$k_yc <- rowSums(cbind(dat2$k_yc_a,dat2$k_yc_p), na.rm = TRUE)
  dat2$k_sr <- rowSums(cbind(dat2$k_sr_a,dat2$k_sr_p),na.rm=TRUE)
  dat2$k_erts <- rowSums(cbind(dat2$k_erts_a, dat2$k_erts_p, dat2$k_erts_l), na.rm = TRUE)
  dat2$k_fck <- rowSums(cbind(dat2$k_fck_a,dat2$k_fck_l),na.rm=TRUE)
  dat2$k_off <- rowSums(cbind(dat2$k_off_a,dat2$k_off_l),na.rm=TRUE)
  dat2$k_cr <- rowSums(cbind(dat2$k_cr_a1,dat2$k_cr_a2),na.rm=TRUE)
  dat2$k_ch <- rowSums(cbind(dat2$k_ch_l,dat2$k_ch_p1,dat2$k_ch_p2,dat2$k_ch_a),na.rm=TRUE)
  dat2$k_dil <- rowSums(cbind(dat2$k_dil_l,dat2$k_dil_a,dat2$k_dil_p),na.rm=TRUE)
  dat2$k_clsrm <- rowSums(cbind(dat2$k_clsrm_ac1,dat2$k_clrsrm_ac2,dat2$k_clsrm_ac3),na.rm=TRUE)
  dat2$k_lab_od <- rowSums(cbind(dat2$k_lab_od1,dat2$k_lab_od2,dat2$k_lab_od3),na.rm=TRUE)
  dat2$k_meter <- rowSums(cbind(dat2$k_meter1, dat2$k_meter2, dat2$k_meter3), na.rm = TRUE)
  
  # df_form <- data.frame(timestamp = index(dat2),coredata(dat2))
  write.csv(df_form,file=paste0(readdir,"all_meters_sept_oct.csv"),row.names = FALSE)
}

plot_aggregate <- function(){
  
  parent <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/sept/aggregate/"
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/sept/")
  fil <- "1.csv"
  temp = fread(paste0(parent,fil),header = TRUE)
  temp$TS2 <- as.POSIXct(temp$TS2,tz="Asia/Kolkata",origin = "1970-01-01")
  df_xts <- xts(temp$W,temp$TS2)
  sampled_df_xts <- resample_data(df_xts,60)
  full_df <- insert_NA_missing(sampled_df_xts)
  plot_title <- "Aggregate_KRESIT_sept_month_2016"
  savename <- paste0(plot_title,".pdf")
  plot_facetgrid_kresit(full_df,plot_title,savename)
  power_df <- data.frame(timestamp = index(df_xts), power = coredata(df_xts))
  write.csv(power_df,paste0(parent,"1_power.csv"),row.names = FALSE)
}

resample_data<- function(xts_datap,xminutes){
  #downsampled data
  ds_data <- period.apply(xts_datap,INDEX = endpoints(index(xts_datap)-3600*0.5, on = "minutes", k = xminutes ), FUN= mean) # subtracting half hour to align hours
  # align data to nearest time boundary
  align_data <- align.time(ds_data,xminutes*60-3600*0.5) # aligning to x minutes
  # return(ds_data)
  rm(ds_data)
  return(align_data)
}

insert_NA_missing <-  function(temp) {
  # function used to insert NA values on missed values
  time_index <-  seq(from=start(temp),to=end(temp),by="hour")
  temp_xts <- xts(rep(-3,length(time_index)),time_index) # -3 to show missing values
  temp2 <- cbind(temp,temp_xts)[,1] # saving only required column 
  return(temp2)
}

plot_facetgrid_kresit <- function(day_data,plot_title,savename){
  # a plotting function to plot data in grid format
  day_data <- cbind(day_data, day = .indexmday(day_data), hour = .indexhour(day_data), daylab = factor(strftime(index(day_data),"%a")))
  colnames(day_data) <- c('power','day',"hour","daylab")
  df <- data.frame(timestamp=index(day_data),coredata(day_data))
  ggplot(df,aes(x = hour, y = power/1000, group = day)) + geom_line() + facet_wrap(~day,ncol=7) + ggtitle(plot_title) + theme(axis.text= element_text(color="black") ) + labs(x = "Hour of the day", y = "Power (kW)" )
  ggsave(file=savename,width=16,height = 8)
}

combine_different_months <- function() {
  #this script combines hourly files of different months and creates aggregate to each corresponding meter
  parent1 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/sept/"
  parent2 <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/oct/"
  writedir <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/sept_oct_agg/"
  library(gtools)
  fls <-   mixedsort(list.files(parent, pattern = "*_power.csv"))
  for(h in 2:length(fls)){
    df1 <- fread(paste0(parent1,fls[h]))
    df2 <- fread(paste0(parent2,fls[h]))
    df1_xts <- xts(round(df1$power,2),fastPOSIXct(df1$timestamp)-19800)
    df2_xts <- xts(round(df2$power,2),fastPOSIXct(df2$timestamp)-19800)
    dfs <- rbind(df1_xts,df2_xts)
    sd2 <- data.frame( as.character(index(dfs)), coredata(dfs) )
    colnames(sd2) <- c("timestamp","power")
    fwrite(sd2,paste0(writedir,fls[h]),row.names = FALSE)
  }
}

plot_summation <- function() {
  # this function is designed to take individual meter data and then
  # to plot their summation.
  month = "oct"
  parent <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/"
  dfiles <- list.files(paste0(parent,month,"/hourly/"),pattern = "*_power.csv")
  dfiles <- dfiles[2:length(dfiles)] # removing first first that represents aggregate
  df_combine = list()
  
  for (i in 1:length(dfiles)) {
    df <- fread(paste0(parent,month,"/hourly/",dfiles[i]),header = TRUE)
    df_combine[[i]] <- xts(df$power,fastPOSIXct(df$timestamp)-19800) # subtracting5:30 hours
    # df_combine[[i]] <- resample_data(df_xts,60) # convert to hourly format
  }
  
  for (i in 1:length(df_combine)){
    if (i==1)
      netpower = df_combine[[i]]
    else
      netpower = netpower + df_combine[[i]]
  }
  #filep = paste0(parent,month,"/hourly/",dfiles[i])
  title_name = paste0("Summation_Hourly_","oct","_2016")
  file_savename = paste0(parent,"oct","/results/","summation.pdf")
  #df <- fread(filep,header = TRUE)
  #df_xts <- xts(df$power,fastPOSIXct(df$timestamp)-19800) # subtracting5:30 hours
  df_xts_full<- insert_NA_missing(netpower)
  plot_facetgrid_kresit(df_xts_full,title_name,file_savename) # function defined in
}

plot_individual_meters <- function (){
  # this function plots data of each individual meter
  month = "oct"
  parent <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/"
  #setwd("/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/sept/results/")
  dfiles <- list.files(paste0(parent,month,"/hourly/"),pattern = "*_power.csv")
  
  for (i in 1:length(dfiles) ){
    filep = paste0(parent,month,"/hourly/",dfiles[i])
    title_name = paste0(strsplit(dfiles[i],"[.]")[[1]][1],"_Hourly_",month,"_2016")
    file_savename = paste0(parent,month,"/results/",strsplit(dfiles[i],"[_]")[[1]][1],".pdf")
    df <- fread(filep,header = TRUE)
    df_xts <- xts(df$power,fastPOSIXct(df$timestamp)-19800) # subtracting5:30 hours
    df_xts_full<- insert_NA_missing(df_xts)
    plot_facetgrid_kresit(df_xts_full,title_name,file_savename) # function defined in plot_aravali_data.R
  }
}

save_hourly <- function(){
  # function to convert default data into hourly representation
  month = "oct"
  parent <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/"
  dfiles <- list.files(paste0(parent,month,"/"),pattern = "*_power.csv")
  #dfiles <- dfiles[2:length(dfiles)] # removing first first that represents aggregate
  
  lapply(dfiles, function(x){
    df <- fread(paste0(parent,month,"/",x),header = TRUE)
    #df_xts <- xts(df$power, as.POSIXct(df$timestamp,tz="Asia/Kolkata",origin = "1970-01-01"))
    df_xts <- xts(df$power,fastPOSIXct(df$timestamp)-19800) # subtracting5:30 hours
    sampled_df_xts <- resample_data(df_xts,60) # convert to hourly format
    write.csv(data.frame(timestamp=index(sampled_df_xts),power=coredata(sampled_df_xts)),file=paste0(parent,month,"/hourly/",x))
  })
}