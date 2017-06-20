library(data.table)
library(xts)
library(ggplot2)
library(scales)


search_variation_uisng_grid_search_consistencyscore() <- function() {
# this function is used to find conssitency score with steps as homes - > then different historical days - > and then different sampling rates
  #energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/onlyservyed_flats.csv"
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/minutely/all_withflatid_Aravalimeters_june_nov2016_minutelevel.csv"
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/grid_plots/")
  df <- fread(energydata)
  Sys.setenv(TZ="GMT")
  df_xts <-  xts(df[,2:dim(df)[2]],fasttime::fastPOSIXct(df$timestamp,tz="GMT"))
  dfs <- df_xts["2016-06-09 00:00:00/2016-06-22 23:59:59"]
  keep2 <- !colnames(dfs) %in% c("F208","F221")
  dfs_keep <- dfs[,keep2]
  for (j in 1:dim(dfs_keep)[2]) { # FLAT WISE
      df_xts <- dfs_keep[,j]
     times <- c(1,10,20,30,40,50,60)
     # times <- c(1)
  for(time in 1:length(times)) { # sampling times
     sample_time = times[time] 
   day_data <- interpolate_missing_readings(df_xts,paste0(1, " min")) # keep it one always
   daywise_data <- resample_daywise_minutely_jugaad(day_data,sample_time)
   sapply(daywise_data,length)
    count = 1
    gp_data <- list()
    for (day_size in 6:13) {  # GROUP DAYS
    gp_data[[count]] <- do.call(cbind,daywise_data[1:day_size])
    count <- count + 1
    }
    plots <- list()
    print(j)
    for(i in 1:length(gp_data)) { # ONE GROUP AT A TIME
      plots[[i]] <- compute_relevancescore_vertical_portion(gp_data[[i]])
    }
    l <- gridExtra::marrangeGrob(plots,ncol=3,nrow = 3)
    fname <- names(df_xts)
    ggsave(paste0(fname,"_",sample_time,".pdf"),l)
  }
  }
}

compute_relevancescore_vertical_portion  <- function(dat) {
  # this version performs interpolation to fill missing readings
  #dat <- interpolate_missing_readings(dat,"1 hour")
 #browser()
   #daydat <- split.xts(dat,f="days",k=1)
   #daymat <- sapply(daydat,function(x) coredata(x))
  daymat <- dat
  colnames(daymat) <- paste0('D',1:dim(daymat)[2])
  ########## vertical logic########
  daymat_xts <- daymat[.indexhour(daymat)>=9 & .indexhour(daymat)<14]
  
  rowMedian <- function(x, na.rm = FALSE)
  {
    apply(x, 1, median, na.rm = na.rm) 
  }
  # stat dataframe with mean and standard devation
  stat <- xts(data.frame(rowmean = rowMeans(daymat_xts,na.rm = TRUE)),index(daymat_xts))
  # stat <- xts(data.frame(rowmean = rowMedian(daymat_xts,na.rm = TRUE)),index(daydat[[1]]))
  stat <- cbind(stat,xts(data.frame(rowsd=apply(as.matrix(coredata(daymat_xts)),1,sd,na.rm=TRUE)),index(daymat_xts)))
  
  status <- vector()
  for( i in 1:dim(daymat_xts)[2]) {
    status[i] <- all((daymat_xts[,i] <= (stat$rowmean + 2*stat$rowsd)) & ( daymat_xts[,i] >= (stat$rowmean - 2*stat$rowsd) ))
  } 
  # percentage of times this apartment has been consistent in the past
  score <- sum(status,na.rm = TRUE)/length(status)
  
  df_melt <- reshape2::melt(data.frame(timestamp=index(daymat_xts),coredata(daymat_xts)),id.vars="timestamp")
  df_melt$value <- df_melt$value/1000
  g <- ggplot(df_melt,aes(timestamp,value, group=variable)) + geom_line(show.legend = FALSE)  + theme_grey(base_size = 10) + stat_summary(data = df_melt,aes(timestamp,value),fun.y = median,fun.ymin = function(x)
  { s = median(x)-2*sd(x)
  if(s < 0)
  { s = 0}
  return(s)
  },fun.ymax = function(x) median(x) + 2*sd(x), geom="line", inherit.aes = FALSE, color="red",size=1) 
  g <- g + theme_grey() + labs(x = "Time (hour)", y = " Power (kW)") + scale_x_datetime(breaks = date_breaks("1 hour"),date_labels = "%H",timezone = "GMT") + theme(axis.text = element_text(color="black",size=9),plot.title = element_text(hjust=0.5))
  #  g <- g + annotate("text", x = df_melt$timestamp[6] , y = max(df_melt$value), label = paste0("Consistency Score = ",round(score,1),"%"),fontface=2) 
  g <- g + ggtitle(paste0("Score = ",round(score,1)))
  # g
  # SECOND VERSION
  # g <- ggplot(df_melt,aes(timestamp,value, group=variable)) + geom_line() +geom_line(data=fortify(stat),aes(Index,rowmean),size=2,col="red",inherit.aes = FALSE)
  #  g
  
  return(g)
}

resample_daywise_minutely_jugaad <- function(xts_datap,xminutes) {
  # function used to downsample data such that all days contain same no. of readings
  day_dat <- split.xts(xts_datap,"days",k=1)
  chng_daydat <- lapply(day_dat,function(x){
    index(x) <- index(day_dat[[1]])
    return(x)
  })
  daylist <- lapply(chng_daydat, function(x) {
    ds_data <- period.apply(x,INDEX = endpoints(index(x), on = "minutes", k = xminutes ), FUN= mean)
    align_data <- align.time(ds_data,xminutes*60) # aligning to x seconds
    return(align_data)
  })
  return(daylist)
}

interpolate_missing_readings <- function(org_xts,samp_duration) {
  timerange <- seq(start(org_xts),end(org_xts), by = samp_duration) # assuming original object is hourly sampled
  #browser()
  temp <- xts(rep(NA,length(timerange)),timerange)
  complete_xts <-  merge(org_xts,temp)[,1]
  complete_xts <- na.approx(complete_xts)
  return(complete_xts)
}



plot_consistencyscore() <- function() {
  # this function is used to find conssitency score with steps as homes - > then different historical days - > and then different sampling rates
  #energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/onlyservyed_flats.csv"
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/minutely/all_withflatid_Aravalimeters_june_nov2016_minutelevel.csv"
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/iitb_report/revisiting-selection-residential/plots/")
  df <- fread(energydata)
  Sys.setenv(TZ="GMT")
  df_xts <-  xts(df[,2:dim(df)[2]],fasttime::fastPOSIXct(df$timestamp,tz="GMT"))
  dfs <- df_xts["2016-06-09 00:00:00/2016-06-22 23:59:59"]
  
  keep2 <- colnames(dfs) %in% c("F207","F212","F225","F252","F258")
  dfs_keep <- dfs[,keep2]
  for (j in 1:dim(dfs_keep)[2]) { # FLAT WISE
    df_xts <- dfs_keep[,j]
      sample_time = 10 #minutes
      day_data <- interpolate_missing_readings(df_xts,paste0(1, " min")) # keep it one always
      daywise_data <- resample_daywise_minutely_jugaad(day_data,sample_time)
      sapply(daywise_data,length)
      gp_data <- do.call(cbind,daywise_data[1:7])
      plot <- compute_relevancescore_vertical_portion(gp_data)
      fname <- names(df_xts)
      ggsave(paste0(fname,"_",sample_time,".pdf"),plot,width = 6, height = 6,units="cm" )
  }
}