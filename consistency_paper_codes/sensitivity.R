sensitivity_analysis_sigma_main <- function(){
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/minutely/all_withflatid_Aravalimeters_june_nov2016_minutelevel.csv"
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/iitb_report/revisiting-selection-residential/plots/")
  df <- fread(energydata)
  Sys.setenv(TZ="GMT")
  df_xts <-  xts(df[,2:dim(df)[2]],fasttime::fastPOSIXct(df$timestamp,tz="GMT"))
  dfs <- df_xts["2016-06-09 00:00:00/2016-06-22 23:59:59"]
  keep2 <- colnames(dfs) %in% c("F252","F207","F212","F225")
  dfs_keep <- dfs[,keep2]
  score <-list()
  for(i in 1:dim(dfs_keep)[2]) { #home wise
    df_xts <- dfs_keep[,i]
    sample_time = 10 #minutes
    day_data <- interpolate_missing_readings(df_xts,paste0(1, " min")) # keep it one always
    daywise_data <- resample_daywise_minutely_jugaad(day_data,sample_time)
    gp_data <- do.call(cbind,daywise_data[1:7]) # sample no. of days
    score[[i]] <- sensitivity_analysis_sigma(gp_data)
  }
  df <- as.data.frame(sapply(score,function(x) return(x$score)))
  colnames(df) <- c("C1","C2","C3","C4")
  df$sd <- c(0.5,1,1.5,2,2.5,3.0)
  df_melt <- reshape2::melt(df,id.vars=c("sd"))
  
  
  g <- ggplot(df_melt,aes(sd,value,color=variable)) + geom_line(size=0.6) + geom_point(aes(shape=variable))
  g <- g +  labs(x='Standard Deviation',y = "Consistency Score")  + theme_grey(base_size = 9) 
  g <- g + theme(axis.text = element_text(color="Black",size=9),legend.position = "top",legend.title=element_blank(),legend.background = element_rect(fill = alpha('white',0.3)),legend.text = element_text(size = 9)) + scale_x_continuous(breaks = pretty_breaks(n=6))
  g
  # ggsave("sigma_sens2.pdf", width = 8, height = 6,units="cm")
}

sensitivity_analysis_sigma  <- function(dat) {
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
  score <- vector()
  stds <- c(0.5,1,1.5,2,2.5,3.0)
  for(j in 1:length(stds)){
    status <- vector()
    for (i in 1:dim(daymat_xts)[2]) {
      status[i] <- all((daymat_xts[,i] <= (stat$rowmean + stds[j]*stat$rowsd)) & ( daymat_xts[,i] >= (stat$rowmean - stds[j]*stat$rowsd) ))
    } 
    score[j] <- round(sum(status,na.rm = TRUE)/length(status),1)
  }
  #browser()
  res_frame <- data.frame(std=stds,score=score)
  return(res_frame)
  #return(g)
}


historical_days_effect_main <- function(){
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/minutely/all_withflatid_Aravalimeters_june_nov2016_minutelevel.csv"
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/iitb_report/revisiting-selection-residential/plots/")
  df <- fread(energydata)
  Sys.setenv(TZ="GMT")
  df_xts <-  xts(df[,2:dim(df)[2]],fasttime::fastPOSIXct(df$timestamp,tz="GMT"))
  dfs <- df_xts["2016-06-09 00:00:00/2016-06-22 23:59:59"]
  keep2 <- colnames(dfs) %in% c("F252","F207","F212","F225")
  dfs_keep <- dfs[,keep2]
  score <-list()
  
  hist_days <- c(6:12)
  hist_scores = list()
  for (j in 1:length(hist_days)) {
   days = hist_days[j]
   score = vector("numeric")
  for(i in 1:dim(dfs_keep)[2]) { #home wise
    df_xts <- dfs_keep[,i]
    sample_time = 10 #minutes
    day_data <- interpolate_missing_readings(df_xts,paste0(1, " min")) # keep it one always
    daywise_data <- resample_daywise_minutely_jugaad(day_data,sample_time)
    gp_data <- do.call(cbind,daywise_data[1:days]) # sample no. of days
    score[i] <- compute_score_sigma_constant(gp_data)
  }
  hist_scores[[j]] <- score
  }
  agg_score <- as.data.frame(do.call(rbind,hist_scores))
  colnames(agg_score) <- c("C1","C2","C3","C4")
  agg_score$days <- c(6:12)

  df_melt <- reshape2::melt(agg_score,id.vars=c("days"))

  g <- ggplot(df_melt,aes(days,value,color=variable)) + geom_line(size=0.6) + geom_point(aes(shape=variable))
  g <- g +  labs(x=' Historical Days (#)',y = "Consistency Score")  + theme_grey(base_size = 9) 
  g <- g + theme(axis.text = element_text(color="Black",size=9),legend.position = "top",legend.title=element_blank(),legend.background = element_rect(fill = alpha('white',0.3)),legend.text = element_text(size = 9)) + scale_x_continuous(breaks = pretty_breaks(n=6))
  g
  # ggsave("hist_days2.pdf", width = 8, height = 6,units="cm")
  
}


sampling_effect <- function(){
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/minutely/all_withflatid_Aravalimeters_june_nov2016_minutelevel.csv"
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/iitb_report/revisiting-selection-residential/plots/")
  df <- fread(energydata)
  Sys.setenv(TZ="GMT")
  df_xts <-  xts(df[,2:dim(df)[2]],fasttime::fastPOSIXct(df$timestamp,tz="GMT"))
  dfs <- df_xts["2016-06-09 00:00:00/2016-06-22 23:59:59"]
  keep2 <- colnames(dfs) %in% c("F252","F207","F212","F225")
  dfs_keep <- dfs[,keep2]
  samp_scores <-list()
  times <- c(1,10,20,30,40,50,60)
  for(j in 1:length(times)) { # sampling times
    sample_time = times[j]
    hist_days <- 7
    score = vector("numeric")
    for(i in 1:dim(dfs_keep)[2]) { #home wise
       df_xts <- dfs_keep[,i]
      day_data <- interpolate_missing_readings(df_xts,paste0(1, " min")) # keep it one always
      daywise_data <- resample_daywise_minutely_jugaad(day_data,sample_time)
      gp_data <- do.call(cbind,daywise_data[1:hist_days]) # sample no. of days
      score[i] <- compute_score_sigma_constant(gp_data)
    }
    samp_scores[[j]] <- score
  }
  
  agg_score <- as.data.frame(do.call(rbind,samp_scores))
  colnames(agg_score) <- c("C1","C2","C3","C4")
  agg_score$sampling <-  c(1,10,20,30,40,50,60)
  
  df_melt <- reshape2::melt(agg_score,id.vars=c("sampling"))
  #ggplot(df_melt,aes(sd,value,color=variable)) + geom_line()
  
  g <- ggplot(df_melt,aes(sampling,value,color=variable)) + geom_line(size=0.6) + geom_point(aes(shape=variable))
  g <- g +  labs(x=' Sample Interval (minutes)',y = "Consistency Score")  + theme_grey(base_size = 9) 
  g <- g + theme(axis.text = element_text(color="Black",size=9),legend.position = "top",legend.title=element_blank(),legend.background = element_rect(fill = alpha('white',0.3)),legend.text = element_text(size = 9)) + scale_x_continuous(breaks = pretty_breaks(n=6))
  g
  # ggsave("sample_interval2.pdf", width = 8, height = 6,units="cm")
  
}


compute_score_sigma_constant  <- function(dat) {
  #browser()
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
  score <- round(sum(status,na.rm = TRUE)/length(status),1)
  
  return(score)
}
