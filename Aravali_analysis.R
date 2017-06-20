# this script is used to analyse the Aravali data
library(data.table)
library(xts)
library(fasttime) #for fastPosixct
Sys.setenv(TZ="Asia/Kolkata")
library(ggplot2)
library(scales)
library(plotly)
library(cluster)
library(gridExtra)
library(fpc)

setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/iitb_report/figures/")

resample_data <- function(xts_datap,xminutes) {
  #downsampled data
  ds_data <- period.apply(xts_datap,INDEX = endpoints(index(xts_datap)-3600*0.5, on = "minutes", k = xminutes ), FUN= mean) # subtracting half hour to align hours
  # align data to nearest time boundary
  align_data <- align.time(ds_data,xminutes*60-3600*0.5) # aligning to x minutes
  # return(ds_data)
  rm(ds_data)
  return(align_data)
}

resample_data_minutely <- function(xts_datap,xminutes) {
  #downsampled data
  ds_data <- period.apply(xts_datap,INDEX = endpoints(index(xts_datap)-3600*0.5, on = "minutes", k = xminutes ), FUN= mean) # subtracting half hour to align hours
  # align data to nearest time boundary
 # browser()
  align_data <- align.time(ds_data,xminutes*60) # aligning to x seconds
  rm(ds_data)
  return(align_data)
}

resample_data_daywise <- function(xts_datap,xdays) {
  datas <- split.xts(xts_datap,"days",k=xdays)
  daydata <- lapply(datas,function(x){
    xts(mean(x),lubridate::date(x[1])) # 
  })
  ds_data <- do.call(rbind,daydata)
  return(ds_data)
}

create_minutely <- function(){
  library(gtools)
  month = "year2016_data/default"
  parent <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/"
  dfiles <- mixedsort(list.files(paste0(parent,month,"/"),pattern = "*.csv"))
  #dfiles <- dfiles[2:length(dfiles)] # removing first first that represents aggregate
  
  lapply(dfiles, function(x){
    df <- fread(paste0(parent,month,"/",x),header = TRUE)
    #df_xts <- xts(df$power, as.POSIXct(df$timestamp,tz="Asia/Kolkata",origin = "1970-01-01"))
    df_xts <- xts(df$power,fastPOSIXct(df$timestamp)-19800) # subtracting5:30 hours
    #remvove duplicates
    duplicate_rno <- which(duplicated(index(df_xts)))
    df_xts <- df_xts[-duplicate_rno,]
    sampled_df_xts <- resample_data_minutely(df_xts,1) # convert to hourly format
    stopifnot(length(unique(lubridate::second(sampled_df_xts)))==1) # ensures series end at 0 seconds
    #duplicate_rno <- which(duplicated(index(sampled_df_xts))) # ensure no duplicates are there
    #sampled_df_xts <- sampled_df_xts[-duplicate_rno,]
    write.csv(data.frame(timestamp=index(sampled_df_xts),power=round(coredata(sampled_df_xts),3)),file=paste0(parent,"year2016_data","/minutely/",x),row.names = FALSE)
  })
}

create_15minutely <- function(){
  library(gtools)
  month = "year2016_data/default"
  parent <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/"
  dfiles <- mixedsort(list.files(paste0(parent,month,"/"),pattern = "*.csv"))
  #dfiles <- dfiles[2:length(dfiles)] # removing first first that represents aggregate
  
  lapply(dfiles, function(x){
    df <- fread(paste0(parent,month,"/",x),header = TRUE)
    #df_xts <- xts(df$power, as.POSIXct(df$timestamp,tz="Asia/Kolkata",origin = "1970-01-01"))
    df_xts <- xts(df$power,fastPOSIXct(df$timestamp)-19800) # subtracting5:30 hours
    #remvove duplicates
    duplicate_rno <- which(duplicated(index(df_xts)))
    df_xts <- df_xts[-duplicate_rno,]
    sampled_df_xts <- resample_data_minutely(df_xts,15) # convert to hourly format
    stopifnot(length(unique(lubridate::second(sampled_df_xts)))==1) # ensures series end at 0 seconds
    #duplicate_rno <- which(duplicated(index(sampled_df_xts))) # ensure no duplicates are there
    #sampled_df_xts <- sampled_df_xts[-duplicate_rno,]
    write.csv(data.frame(timestamp=index(sampled_df_xts),power=round(coredata(sampled_df_xts),3)),file=paste0(parent,"year2016_data","/15minutes/",x),row.names = FALSE)
  })
}

create_hourly <- function(){
  library(gtools)
  # function to convert default data into hourly representation
  month = "year2016_data/default"
  parent <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/"
  dfiles <- mixedsort(list.files(paste0(parent,month,"/"),pattern = "*.csv"))
  #dfiles <- dfiles[2:length(dfiles)] # removing first first that represents aggregate
  
  lapply(dfiles, function(x){
    df <- fread(paste0(parent,month,"/",x),header = TRUE)
    #df_xts <- xts(df$power, as.POSIXct(df$timestamp,tz="Asia/Kolkata",origin = "1970-01-01"))
    df_xts <- xts(df$power,fastPOSIXct(df$timestamp)-19800) # subtracting5:30 hours
    #remvove duplicates
    duplicate_rno <- which(duplicated(index(df_xts)))
    df_xts <- df_xts[-duplicate_rno,]
    sampled_df_xts <- resample_data(df_xts,60) # convert to hourly format
    write.csv(data.frame(timestamp=index(sampled_df_xts),power=round(coredata(sampled_df_xts),2)),file=paste0(parent,"year2016_data","/hourly/",x),row.names = FALSE)
  })
}

create_daily_data<- function() {
  library(gtools)
  # function to convert default data into hourly representation
  month = "year2016_data/default"
  parent <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/"
  dfiles <- mixedsort(list.files(paste0(parent,month,"/"),pattern = "*.csv"))
  #dfiles <- dfiles[2:length(dfiles)] # removing first first that represents aggregate
  
  lapply(dfiles, function(x) {
    df <- fread(paste0(parent,month,"/",x),header = TRUE)
    #df_xts <- xts(df$power, as.POSIXct(df$timestamp,tz="Asia/Kolkata",origin = "1970-01-01"))
    df_xts <- xts(df$power,fastPOSIXct(df$timestamp)-19800) # subtracting5:30 hours
    #remvove duplicates
    duplicate_rno <- which(duplicated(index(df_xts)))
    df_xts <- df_xts[-duplicate_rno,]
    sampled_df_xts <- resample_data_daywise(df_xts,1) # convert to daywise format
    write.csv(data.frame(timestamp=index(sampled_df_xts),power=round(coredata(sampled_df_xts),2)),file=paste0(parent,"year2016_data","/daily/",x),row.names = FALSE)
  })
}

create_matrix_of_N_meters_minutewise <- function() {
  # this file represents data from different meters in a single matrix format. Each column represents different meter.
  readdir <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/minutely/"
  fls <- list.files(readdir)
  library(gtools) # required to sort names http://stackoverflow.com/a/15944950/3317829
  fls <- mixedsort(fls)
  dat <- lapply(fls, function(x){
    dfz <- fread(paste0(readdir,x),header = TRUE)
    dfz_xts <- xts(dfz$power,fastPOSIXct(dfz$timestamp)-19800)
    dfz_xts <- dfz_xts["2016-06-01/"] # subsetting ensures only dates starting from some range 
    return(dfz_xts)
  })
  
  dat2 <- do.call(cbind,dat)
  warning("Make ensure that columns are labelled correctly")
  names_col <- unlist(lapply(fls, function(x) strsplit(x,'[.]')[[1]][1]))
  stopifnot(length(colnames(dat2)) == length(names_col))
  colnames(dat2) <- names_col
  df_form <- data.frame(timestamp = index(dat2),coredata(dat2))
  write.csv(df_form,file=paste0(readdir,"all_Aravalimeters_june_nov2016_minutelevel.csv"),row.names = FALSE)
}

create_matrix_of_N_meters_15minutewise <- function() {
  # this file represents data from different meters in a single matrix format. Each column represents different meter.
  readdir <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/15minutes/"
  fls <- list.files(readdir)
  library(gtools) # required to sort names http://stackoverflow.com/a/15944950/3317829
  fls <- mixedsort(fls)
  dat <- lapply(fls, function(x){
    dfz <- fread(paste0(readdir,x),header = TRUE)
    dfz_xts <- xts(dfz$power,fastPOSIXct(dfz$timestamp)-19800)
    dfz_xts <- dfz_xts["2016-06-01/"] # subsetting ensures only dates starting from some range 
    return(dfz_xts)
  })
  
  dat2 <- do.call(cbind,dat)
  warning("Make ensure that columns are labelled correctly")
  names_col <- unlist(lapply(fls, function(x) strsplit(x,'[.]')[[1]][1]))
  stopifnot(length(colnames(dat2)) == length(names_col))
  colnames(dat2) <- names_col
  df_form <- data.frame(timestamp = index(dat2),coredata(dat2))
  write.csv(df_form,file=paste0(readdir,"all_Aravalimeters_june_nov2016_15minutelevel.csv"),row.names = FALSE)
}
create_matrix_of_N_meters <- function() {
  # this file represents data from different meters in a single matrix format. Each column represents different meter.
  readdir <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/"
  fls <- list.files(readdir)
  library(gtools) # required to sort names http://stackoverflow.com/a/15944950/3317829
  fls <- mixedsort(fls)
  dat <- lapply(fls, function(x){
    dfz <- fread(paste0(readdir,x),header = TRUE)
    dfz_xts <- xts(dfz$power,fastPOSIXct(dfz$timestamp)-19800)
    dfz_xts <- dfz_xts["2016-06-01 00:00:00/"] # subsetting ensures only dates starting from some range are there
    newtimestamp <- vector()
    #this part handles observations mapped to 30 minutes, and ulitimately pushes them to one hour boundary
    for (j in c(1:dim(dfz_xts)[1])) {
      if(.indexmin(dfz_xts[j,]) %in% c(30)) {
        newtimestamp[j] <-  index(dfz_xts[j,]) + 60*60*0.5
      } else{
        newtimestamp[j] <-  index(dfz_xts[j,]) + 0
      }
    }
    dfz_xts <- xts(coredata(dfz_xts),as.POSIXct(newtimestamp,origin = "1970-01-01"))
    # fill missing readings with NA
    timerang <- seq(start(dfz_xts),end(dfz_xts), by = "hour")
    temp <- xts(rep(NA,length(timerang)),timerang)
    dfz_xts <- merge(dfz_xts,temp)[,1]
    return(dfz_xts)
  })
  
  dat2 <- do.call(cbind,dat)
  warning("Make ensure that columns are labelled correctly")
  names_col <- unlist(lapply(fls, function(x) strsplit(x,'[.]')[[1]][1]))
  stopifnot(length(colnames(dat2)) == length(names_col))
  colnames(dat2) <- names_col
  df_form <- data.frame(timestamp = index(dat2),coredata(dat2))
  write.csv(df_form,file=paste0(readdir,"all_Aravalimeters_june_nov2016.csv"),row.names = FALSE)
}

create_matrix_of_N_meters_aggreage_daywise <- function() {
  # this file represents data from different meters in a single matrix format. Each column represents different meter.
  readdir <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/daily/"
  fls <- list.files(readdir)
  library(gtools) # required to sort names http://stackoverflow.com/a/15944950/3317829
  fls <- mixedsort(fls)
  dat <- lapply(fls, function(x){
    dfz <- fread(paste0(readdir,x),header = TRUE)
    dfz_xts <- xts(dfz$power,fastPOSIXct(dfz$timestamp)-19800)
    dfz_xts <- dfz_xts["2016-06-01/"] # subsetting ensures only dates starting from some range 
    return(dfz_xts)
  })
  
  dat2 <- do.call(cbind,dat)
  warning("Make ensure that columns are labelled correctly")
  names_col <- unlist(lapply(fls, function(x) strsplit(x,'[.]')[[1]][1]))
  stopifnot(length(colnames(dat2)) == length(names_col))
  colnames(dat2) <- names_col
  df_form <- data.frame(timestamp = index(dat2),coredata(dat2))
  write.csv(df_form,file=paste0(readdir,"all_Aravalimeters_june_nov2016_daylevel.csv"),row.names = FALSE)
}

aggregate_consumption <- function() {
  # this function reads matrices created and calculates sum (net building consumpion)
  readdir <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/minutely/"
  readfile <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/minutely/all_Aravalimeters_june_nov2016_minutelevel.csv"
  df <- fread(readfile)
  dfs <-  xts(df[,2:dim(df)[2]],fastPOSIXct(df$timestamp)-19800)
  remove_meters <- c("X3","X10","X11") # meters containing bogus data
  dfs <- dfs[,!colnames(dfs)%in%remove_meters]
  agg <- apply(coredata(dfs),1,function(x){ # returns sum of NA's as NA otherwise not possible
    ifelse(all(is.na(x)), NA, sum(x,na.rm = TRUE))
  })
  agg_consump <- data.frame(power = agg, timestamp = index(dfs))
  write.csv(agg_consump,file=paste0(readdir,"aggregate_building2016.csv"),row.names = FALSE)
  
}

clustering <- function() {
  
  readfile <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/all_Aravalimeters_june_nov2016.csv"
  df <- fread(readfile)
  dfs <-  xts(df[,2:dim(df)[2]],fastPOSIXct(df$timestamp)-19800)
  remove_meters <- c("X3","X10","X11") # meters containing bogus data
  dfs <- dfs[,!colnames(dfs)%in%remove_meters]
  net_building_consumption(dfs)
  four_day_aggregate_consumption_plot(dfs)
  #range of dates for which we need to do clustering
  subset1 <- dfs["2016-09-19 00:00:00/2016-09-22 23:59:59"]
  
  
  suppressWarnings(rm("returnob"))
  for (i in 1:dim(subset1)[2]) {
    # 1: compute mean across all days and REMEMBER time is set to first day of series
    # 2: Combine all meters in matrix form
    sub <- split.xts(subset1[,i], f = "days", k = 1)
    sub_mean <- rowMeans(sapply(sub, function(y) return(coredata(y))),na.rm = TRUE)
    sub_xts <- xts(round(sub_mean,2),index(sub[[1]]))
    
    if(!exists("returnob"))
      returnob <- sub_xts
    else
      returnob<- cbind(returnob,sub_xts)
  }
  colnames(returnob) <- colnames(subset1)
  view_data(returnob)
  set.seed(123)
  clusob <- kmeans(t(returnob),6)# forced 6 clusters
  labels <- as.factor(clusob$cluster)
  view_clustering_result(returnob,labels)
  view_distriubtion_bands(returnob,labels)
  # create profiles
  view_profiles_in_FACETS_decreasingorder(returnob,labels)
  view_energy_contribution(returnob,labels)
  
}

keep_surveyed_flats_only <- function() {
  # this function creates a matrix of only surveyed flats. other flats are dropped
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/15minutes/all_withflatid_Aravalimeters_june_nov2016.csv"
  df <- fread(energydata)
  dfs <-  xts(df[,2:dim(df)[2]],fastPOSIXct(df$timestamp)-19800)
  
  surfile <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/surveydata.csv"
  sfile <- read.csv(surfile)
  colnames(sfile)
  sframe <- sfile[,c(8,1,3,4,5)]
  sframe$flatId <- paste0('F',sframe$flatId)
  #sframe$total <- rowSums(sframe[,2:5])
  #sframe <- sframe[sframe$flatId!="F236",]#currently, there is duplicate of it
  table(duplicated(sframe$flatId)) 
  
  keeplist <- sframe$flatId
  dfs_new <- dfs[,keeplist] # keep only those flats which are surveyed
  removelist <- c("F217","F239","F11") # remove meters whose behaviour is choatic
  final_homes <- dfs_new[,!colnames(dfs_new)%in% removelist]
  
  write.csv(data.frame(timestamp=index(final_homes),coredata(final_homes)),"/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/15minutes/onlyservyed_flats.csv",row.names = FALSE)
}

fit_lineto_clusters <- function(df,labels) {
  # this function takes data and creates clusters and also fits a line
  df_form <- data.frame(timestamp = index(df),coredata(df))
  df_long <- reshape2::melt(df_form,id.vars = "timestamp")
  df_long$cluslabel <- rep(labels, each = dim(df)[1])
  
  g <- ggplot(df_long, aes(timestamp, value/1000, group = variable))+facet_wrap(~cluslabel)+ geom_line(alpha=0.15)
  g <- g + scale_y_continuous(limits = c(0,2.2))
  g <- g + labs(y ="Power (kW)",x="Hour of the day") + theme_grey(base_size = 10)
  g <- g+ stat_summary(data = df_long,aes(timestamp,value/1000,group=cluslabel),
                       fun.y = mean,fun.ymin = function(x) mean(x)-sd(x),
                       fun.ymax = function(x) mean(x)+sd(x),
                       geom="smooth",inherit.aes = FALSE,color="red")
  g <- g + scale_x_datetime(breaks = date_breaks("3 hour"), labels = date_format("%H",tz="Asia/Kolkata")) # use scales package
  g <- g + theme(axis.text.x = element_text(angle = 0,hjust = 1),axis.text=element_text(color="black"))
  return(g)
  # ggplotly(g)
}

clustering_for_flats_withSurvey <- function() {
  # this shows clustering for the flats for which we have survey data
  readfile <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/onlyservyed_flats.csv"

  df <- fread(readfile)
  dfs <-  xts(df[,2:dim(df)[2]],fastPOSIXct(df$timestamp)-19800)
  #net_building_conusmption(dfs)
  #four_day_aggregate_consumption_plot(dfs)
  #range of dates for which we need to do clustering
  subset1 <- dfs["2016-09-01 00:00:00/2016-09-07 23:59:59"]
 # subset1 <- dfs
  drop_homes <- c("F209","F239","F216")
  subset1 <- subset1[,!colnames(subset1) %in% drop_homes]
  cat(paste0("Dropped ",drop_homes))
  
  surfile <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/surveydata.csv"
  sfile <- read.csv(surfile)
  colnames(sfile)
  sframe <- sfile[,c(8,1,3,4,5)]
  sframe$flatId <- paste0('F',sframe$flatId)
  sframe$total <- rowSums(sframe[,2:5])
  table(duplicated(sframe$flatId))
  # Keep only those homes for which we have survey data
  subset2 <- subset1[,colnames(subset1) %in% sframe$flatId]
  
  suppressWarnings(rm("returnob"))
  for (i in 1:dim(subset2)[2]) {
    # 1: compute mean across all days and REMEMBER time is set to first day of series
    # 2: Combine all meters in matrix form
    #browser()
    sub <- split.xts(subset2[,i], f = "days", k = 1)
    sub_mean <- rowMeans(sapply(sub, function(y) return(coredata(y))),na.rm = TRUE)
    sub_xts <- xts(round(sub_mean,2),index(sub[[1]]))
    
    if(!exists("returnob"))
      returnob <- sub_xts
    else
      returnob<- cbind(returnob,sub_xts)
  }
  colnames(returnob) <- colnames(subset2)
  #view_data(returnob)
  set.seed(123)
  no_of_clusters <- pamk(t(returnob))$nc
  distmat <- dist(t(returnob))
  clusob <- pam(distmat,no_of_clusters)
  
  #clusob <- kmeans(t(returnob),6)# forced 6 clusters
  labels <- as.factor(clusob$cluster)
  # add cluster no. to each row in sframe
  sframe$cluster <- clusob$cluster[sframe$flatId]
  fit_lineto_clusters(returnob,labels)
  # ggsave("fitline_june13-19.pdf",width=4,height = 3)
  # ggsave("fitline_aug12-18.pdf",width=4,height = 3)
  # ggsave("fitline_sep1-7.pdf",width=4,height = 3)
  view_clustering_result(returnob,labels)
  view_distriubtion_bands(returnob,labels)
  # create profiles
  view_profiles_in_FACETS_decreasingorder(returnob,labels)
  view_energy_contribution(returnob,labels,sframe)
  
}

clustering_occupancy_data <- function() {
  library(fpc)
  surfile <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/surveydata.csv"
  sfile <- read.csv(surfile)
  colnames(sfile)
  sframe <- sfile[,c(8,1,3,4,5)]
  sframe$flatId <- paste0('F',sframe$flatId)
  sframe$total <- rowSums(sframe[,2:5])
  #sframe <- sframe[sframe$flatId!="F236",]#currently, there is duplicate of it
  table(duplicated(sframe$flatId))
  
  no_clusters <- pamk(t(x),krange = 2:8)
  clusob <- pam(t(x), no_clusters$nc)# forced 6 clusters
  
  
}

view_clusters_along_days <- function(){
  # this function takes cluster day wise data individually and also
  # plots statistics table
  
  surfile <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/surveydata.csv"
  sfile <- read.csv(surfile)
  colnames(sfile)
  sframe <- sfile[,c(8,1,3,4,5)]
  sframe$flatId <- paste0('F',sframe$flatId)
  sframe$total <- rowSums(sframe[,2:5])
  #sframe <- sframe[sframe$flatId!="F236",]#currently, there is duplicate of it
  table(duplicated(sframe$flatId))
  
  readfile <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/oct/hourly/all_withflatid_Aravalimeters_Sept2016.csv"
  df <- fread(readfile)
  dfs <-  xts(df[,2:dim(df)[2]],fastPOSIXct(df$timestamp)-19800)
  
  drop_homes <- c("F209","F239","F216")
  dfs <- dfs[,!colnames(dfs) %in% drop_homes]
  cat(paste0("Dropped ",drop_homes))
  
  #range of dates for which we need to do clustering
  subset1 <- dfs["2016-09-01 00:00:00/2016-09-22 23:59:59"]
  # Keep only those homes for which we have survey data
  subset2 <- subset1[,colnames(subset1) %in% sframe$flatId]
  
  daydat <- split.xts(subset2,f="days",k=1)
  
  plots <- lapply(daydat, function(x) {
    if(all(is.na(x))) # for days where entire day data is missing
      return(0)
    sframe <- sfile[,c(8,1,3,4,5)]
    sframe$flatId <- paste0('F',sframe$flatId)
    sframe$total <- rowSums(sframe[,2:5])
    set.seed(123)
    temp <- apply(x,1,function(x) all(is.na(x)))
    x <- x[!temp,]
    cat("HE:")
    clusob <- pam(t(x), 6)# forced 6 clusters
    cat("X;")
    labels <- as.factor(clusob$cluster)
    # add cluster no. to each row in sframe
    sframe$cluster <- clusob$cluster[sframe$flatId]
    res <- view_energy_contribution(x, labels, sframe)
    rm(sframe)
    return(res)
  })
  
  pdf("dd.pdf")
  lapply(plots,function(x) plot(x))
  dev.off()
}

correlation_analysis <- function(relation_mat) {
  #this function finds the times a group of homes remain in the same cluster over several days
  cormaplot <- cor(relation_mat)
  # p.mat <- cor.mtest(relation_mat)
  #corrplot(cormaplot,type="upper",order = "hclust",
  #         p.mat=p.mat,sig.level = 0.1,insig = "blank" )
  #http://blog.minitab.com/blog/adventures-in-statistics/understanding-hypothesis-tests:-significance-levels-alpha-and-p-values-in-statistics
  pos <- which(cormaplot > 0.5,arr.ind = TRUE,useNames=FALSE)
  dfx <- data.frame(F1= rownames(cormaplot)[pos[,1]],F2= colnames(cormaplot)[pos[,2]])
  dfx$value <- cormaplot[pos]
  dfx <- dfx[dfx$F1!=dfx$F2,] # removing diagonal elements
  dfx[dfx$value > 0.90 & dfx$value <0.95,]
  return(dfx)
}

net_building_consumption <- function(dfs) {
  # this plots the aggregate consumption of the building
  # 
  agg <- apply(coredata(dfs),1,function(x){ # returns sum of NA's as NA otherwise not possible
    ifelse(all(is.na(x)), NA, sum(x,na.rm = TRUE))
  })
  
  agg_consump <- data.frame(power = agg, timestamp = index(dfs))
 # agg_consump <- data.frame(power = rowSums(dfs,na.rm = TRUE),timestamp = index(dfs))
  xts(agg_consump$power,fastPOSIXct(agg_consump$timestamp)-19800)
  g <- ggplot(agg_consump, aes(timestamp, power/1000))
  g <- g + geom_line() + labs(y ="power (kW)",x="Timestamp")
  g <- g + scale_x_datetime(breaks = date_breaks("1 day"))
  # labels = date_format("%d %H",tz="Asia/Kolkata")) # use scales package
  g <- g + theme(axis.text.x = element_text(angle = 90,hjust = 1))
  ggplotly(g)
}

four_day_aggregate_consumption_plot <- function(dfs) {
  agg_consump <- data.frame(power = rowSums(dfs,na.rm = FALSE),timestamp = index(dfs))
  limdata <- xts(agg_consump$power,fastPOSIXct(agg_consump$timestamp)-19800)
  daydata <- split.xts(limdata,f="days",k=1)
  suppressWarnings(rm("newframe"))
  for(i in 1:4){
    newdata <- coredata(daydata[[i]])
    if(!exists("newframe")){
      newframe <- newdata
    }else {
      newframe <- cbind(newframe,newdata)
    }
  }
  dat<-xts(newframe,index(daydata[[1]]))#forcing index of first day
  dat_df <- data.frame(timestamp=index(dat),coredata(dat))
  df_long <- reshape2::melt(dat_df,id.vars = "timestamp")
  g <- ggplot(df_long,aes(x=timestamp,y=value/1000,col=variable)) + geom_line() 
  g <- g + labs(x="Hour of the day",y = "Power (kW)") +ggtitle("Aggregate consumption on 4 different days [1-4 sept] of Sept month 2016") +scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%H:%M",tz="Asia/Kolkata")) # use scales package
  g
  ggplotly(g)
}

view_data <- function(df) {
  # this function takes xts dataframe as input and uses interactively plotly to plot data
  library(plotly)
  df_form <- data.frame(timestamp = index(df),coredata(df))
  df_long <- reshape2::melt(df_form,id.vars = "timestamp")
  colourCount = length(unique(df_long$variable))
  #getPalette = colorRampPalette(brewer.pal(9, "Set1"))(colourCount) # brewer.pal(8, "Dark2") or brewer.pal(9, "Set1")
  scheme <- iwanthue() # for distint colors
  getPalette = scheme$hex(colourCount)
  g <- ggplot(df_long, aes(timestamp, value/1000, col = variable, group = variable))
  g <- g + geom_line() + scale_colour_manual(values = getPalette) + labs(y ="power (kW)")
  #g <- g + scale_x_datetime(breaks = date_breaks("1 hour"), labels = date_format("%d %H:%M",tz="CDT")) # use scales package
  g <- g + theme(axis.text.x = element_text(angle = 90,hjust = 1))
  ggplotly(g)
}

view_clustering_result <- function(df,labels) {
  # this function simply plots days data differentiated acc. to cluster colour
  library(plotly)
  df_form <- data.frame(timestamp = index(df),coredata(df))
  df_long <- reshape2::melt(df_form,id.vars = "timestamp")
  df_long$cluslabel <- rep(labels, each = dim(df)[1])
  g <- ggplot(df_long, aes(timestamp, value/1000, col = cluslabel, group = variable))
  g <- g + geom_line()  + labs(y ="power (kW)",x="Hour of day")
  g <- g + scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%H",tz="Asia/Kolkata")) # use scales package
  g <- g + theme(axis.text.x = element_text(angle = 0,hjust = 1))
  ggplotly(g)
}

view_profiles_in_FACETS_decreasingorder <- function (df,labels) {
  # this function plots the representative/generalized clusters in facet form. We plot profiles in decreasing order
  #rm(list=ls())
  #browser()
  df_form <- data.frame(timestamp = index(df),coredata(df))
  df_long <- reshape2::melt(df_form,id.vars = "timestamp")
  df_long$cluslabel <- rep(labels, each = dim(df)[1])
  unique_labels <- gtools::mixedsort(unique(labels))
  suppressWarnings(rm("return_df"))
  for (h in 1:length(unique_labels)) { 
    tt <- df_long[df_long$cluslabel == unique_labels[h],]
    tt <- subset(tt, select = - cluslabel)
    temp <- reshape2::dcast(tt, timestamp ~ variable)
    temp_xts <- xts(data.frame(temp[, 2:dim(temp)[2]]), fastPOSIXct(temp[,1]) - 19800)
    if(!exists("return_df")) {
      return_df <- xts(data.frame(power = rowMeans(temp_xts, na.rm = TRUE), cluster =  as.numeric(unique_labels[h])),index(temp_xts))
    } else {
      tempob <- xts(data.frame(power = rowMeans(temp_xts, na.rm = TRUE), cluster =  as.numeric(unique_labels[h])),index(temp_xts))
      return_df <- rbind(return_df,tempob)
    } 
  }
  #browser()
  df_frame <- data.frame(timestamp = index(return_df),coredata(return_df) )
  # Arrange cluster labels accroding to decreasing order with respect to cluster population size
  clusterorder <- order(table(labels), decreasing = TRUE)
  df_frame$cluster <- factor(df_frame$cluster,levels = clusterorder)
  elem_count <- sort(table(labels),decreasing = TRUE)
  g <- ggplot(df_frame, aes(timestamp, power)) + facet_wrap(~ cluster,nrow=2)
  g <- g + geom_line()  + labs(y ="Power (Watts)",x="Hour of day")
  g <- g + scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%H",tz="Asia/Kolkata")) # use scales package
  g <- g + annotate("text",x = df_frame$timestamp[11] ,y = max(df_frame$power)-100, label = elem_count,color="red",size=6)
  ggplotly(g)
}

view_distriubtion_bands <- function(df,labels) {
  # this function takes data and creates bands
    df_form <- data.frame(timestamp = index(df),coredata(df))
  df_long <- reshape2::melt(df_form,id.vars = "timestamp")
  df_long$cluslabel <- rep(labels, each = dim(df)[1])
  g <- ggplot(df_long, aes(timestamp, value/1000, col = cluslabel, group = variable))
  #g <- g + geom_line()  + labs(y ="power (kW)",x="Hour of day")
  g <- g  + labs(y ="power (kW)",x="Hour of day")
  g <- g+ stat_summary(data = df_long,aes(timestamp,value/1000,group=cluslabel,col=cluslabel),
                       fun.y = mean,fun.ymin = function(x) mean(x)-sd(x),
                       fun.ymax = function(x) mean(x)+sd(x),
                       geom="smooth",inherit.aes = FALSE)
  g <- g + scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%H",tz="Asia/Kolkata")) # use scales package
  g <- g + theme(axis.text.x = element_text(angle = 0,hjust = 1))
  g
  # ggplotly(g)
}

meter_renaming <- function() {
  # meter 19 removed since it is not in data
  # 253, 208,217 NOT WORKING
  meters <- c("X1", "X3", "X4", "X5", "X6", "X8", "X10", "X11", "X16", "X17", "X18","X21", "X22", "X23", "X24", "X25", "X26", "X27", "X28", "X29", "X30", "X31", "X32", "X34", "X35", "X36", "X37", "X200", "X207", "X208", "X209", "X211", "X212", "X214", "X218","X217","X219", "X 221", "X223", "X224", "X225", "X226", "X227", "X228", "X229", "X231", "X233", "X234", "X235", "X236", "X237", "X238", "X240", "X241", "X242", "X243","X244", "X245", "X246", "X247")
  flatid <- c(216,3,210,220,222,232,239,11,249,248,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,250,207,208,209,211,212,214,217,218,219,221,223,224,225,226,227,228,229,231,233,234,235,236,237,238,240,241,242,243,244,245,246,247)
  newflatid <- paste0('F',flatid)
  newnames <- plyr::mapvalues(meters,from = meters, to = newflatid)
  #newflatid <- sort(newflatid)
  #dput(paste0(newflatid,"=",newflatid))
  
  
  
  # read file with orig meter nos and then assign new names
  readfile <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/15minutes/all_Aravalimeters_june_nov2016_daylevel.csv"
  df <- fread(readfile)
  colnames(df) <- c("timestamp",newnames)
  # write.csv(df, "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/15minutes/all_withflatid_Aravalimeters_june_nov2016_daylevel.csv",row.names = FALSE)
  
}

format_survey_file <- function(){
  # TAKES LONG TIME TO SAVE/FORMAT TABLE
  # BETTER USE EXCEL FORMT
  surfile <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/surveydata.csv"
  sfile <- read.csv(surfile)
  #colnames(sfile)
  sframe <- sfile[,c(8,1,3,4,5,6,10,2)]
  sframe$flatId <- paste0('F',sframe$flatId)
  sframe$total <- rowSums(sframe[,2:5])
  sframe <- sframe[order(sframe[,1]),]
  # pdf("full_survey.pdf")
  #gridExtra::grid.table(sframe,rows=NULL)
  #dev.off() 
  
  tbl <- tableGrob(sframe,theme=ttheme_minimal(base_size = 8),rows=NULL)
}

compute_clusteringsimilarity_acrossdays <- function(df,gp_mems) {
  # this function calcualtes the percetange a group of objects got same clusters
  #Find details at http://stackoverflow.com/a/40713894/3317829
  library(tidyverse)
  df <- df[,gp_mems]
  x <- map(2:length(df), ~combn(names(df), .x, simplify = FALSE))    # get combinations
  y <-  x %>% flatten()    # eliminate nesting
  z <-  y %>% set_names(map_chr(., paste0, collapse = ''))    # add useful names
  # subset df with combination, see if each row has only one unique value
  h <- z %>% map(~apply(df[.x], 1, function(x){n_distinct(x) == 1})) 
  hh <- h  %>% map_dbl(~sum(.x) / length(.x))    # calculate TRUE proportion
  return(hh)
} 

family_size_clustering <- function() {
  library(factoextra)
  #  In this we cluster homes entirely on basis of population distribution
  surfile <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/surveydata.csv"
  sfile <- read.csv(surfile)
  colnames(sfile)
  sframe <- sfile[,c(8,1,3,4,5)]
  sframe$flatId <- paste0('F',sframe$flatId)
  sframe$total <- rowSums(sframe[,2:5])
  #sframe <- sframe[sframe$flatId!="F236",]#currently, there is duplicate of it
  table(duplicated(sframe$flatId))
  
  df_sframe <- sframe[,2:5]
  # mixing 2 age groups together
  df_sframe <- data.frame(zeroto20 = sframe$zeroto20, twentyto60 = sframe$twentyto40 + sframe$fortyto60,above60 = sframe$above60)
  row.names(df_sframe) <- sframe[,1]
  no_clusters <- pamk(df_sframe,krange = 2:3)
  clusob <- pam(df_sframe, no_clusters$nc)
  fviz_cluster(clusob, data = df)+theme_minimal()
  df_sframe$clus <- clusob$clustering
  df_sframe[with(df_sframe,order(clus)),]
  table(df_sframe$clus)
  
  # clustering acco. to rules mentioned by sir
  elderhomes <- sframe[sframe$above60 >0,]$flatId 
  noelders <- sframe[sframe$above60==0,]
  childrens <- noelders[noelders$zeroto20>=2,]$flatId
  remaining <- noelders[noelders$zeroto20<2,]$flatId
  
  elderhomes <- c("F222","F244","F241","F239","F235","F234","F221","F217","F219","F218")
  childrens <- c("F256","F252","F238","F240","F230","F229","F267","F264","F261")
  remaining <- c("F262","F263","F257","F253","F248","F250","F247","F237","F236","F232","F228","F227","F220","F215","F255","F260","F259","F254","F251","F246","F226","F243","F207","F208","F209","F212")
  
}

removedays_with_onlyNAs <- function() {
  # this function creates a matrix of only surveyed flats. other flats are dropped
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/onlyservyed_flats.csv"
  df <- fread(energydata)
  dfs <-  xts(df[,2:dim(df)[2]],fastPOSIXct(df$timestamp)-19800)
  # identify days with all Nas
  daydat <-  split.xts(dfs, f="days",k=1)
  
  suppressWarnings(rm("newob"))
  for(i in 1:length(daydat)){
    if(all(is.na(daydat[[i]]))){
      next
    }
    if(!exists("newob")){
      newob <- daydat[[i]]
    }else {
      newob<- rbind(newob,daydat[[i]])
    }
  }
  #write.csv(data.frame(timestamp=index(newob),coredata(newob)),"/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/onlyservyed_flats_without_NAdays.csv",row.names = FALSE)
}

view_energy_contribution <- function(df,labels, sframe) {
  #this function shows consumption of each cluster and a table with statistics
  df_form <- data.frame(timestamp = index(df),coredata(df))
  df_long <- reshape2::melt(df_form,id.vars = "timestamp")
  df_long$cluslabel <- rep(labels, each = dim(df)[1])
  
  # Let us  construct a table which will show the %age of energy consumed by a cluster against 
  # the aggregate
  agg_consump <- xts(rowSums(df,na.rm = FALSE),index(df))
  clusters <- gtools::mixedsort(unique(df_long$cluslabel))
  for(i in 1:length(clusters)) {
    dat <- df_long[df_long$cluslabel==as.factor(clusters[i]),]
    temp <- reshape2::dcast(dat,timestamp~variable)
    temp_data <- subset(temp,select = -timestamp)
    #temp_xts <- xts(temp_data,temp$timestamp)
    temp_xts <- xts(rowSums(temp_data,na.rm = FALSE),temp$timestamp)
    agg_consump <- cbind(agg_consump,temp_xts)
  }
  colnames(agg_consump) <- c("Aggregate",c(1:length(clusters)))
  #percentage energy consumed by each home against the total consumption of building
  per_contribuion <- apply(agg_consump[,2:dim(agg_consump)[2]],2,function(x) mean(x/agg_consump[,1])*100)
  tab_stat <- data.frame(table(labels),round(per_contribuion,2)) 
  colnames(tab_stat) <- c("Cluster#","Apartments","Energy_Consumed(%)")
  
  # Add no. of people to each residing in each cluster
  sframe$cluster <- as.factor(sframe$cluster)
  clnames <- gtools::mixedsort(unique(labels))
  tab_stat$people <- sapply(clnames,function(x) {
    colSums(sframe[sframe$cluster == x,]["total"],na.rm = TRUE)
  })
  
  library(gridExtra)
  tbl <- tableGrob(tab_stat,rows = NULL,theme=ttheme_default(base_size = 8,padding = unit(c(3, 3), "mm")))
  
  g <- ggplot(df_long, aes(timestamp, value/1000, col = cluslabel, group = variable))
  g <- g + labs(y ="power (kW)",x="Hour of day")
  g <- g + stat_summary(data = df_long,aes(timestamp,value/1000,group = cluslabel,col = cluslabel),
                        fun.y = sum, geom="line",inherit.aes = FALSE)
  g <- g + stat_summary(data = df_long,aes(timestamp,value/1000),
                        fun.y = sum, geom="line",inherit.aes = FALSE)
  g <- g + scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%H",tz="Asia/Kolkata")) # use scales package
  g <- g + theme(axis.text.x = element_text(angle = 0,hjust = 1)) + ggtitle(as.character(as.Date(df_long$timestamp[10])))
  # g <- g + guides(col=guide_legend(nrow=2,byrow = TRUE))+theme(axis.text = element_text(color="Black"),legend.position = c(0.20,0.80),legend.title=element_blank(),legend.background=element_rect(fill=alpha('white',0.3)),legend.text=element_text(size = 8))
  #g <- g + annotation_custom(tableGrob(tab_stat))
  return(arrangeGrob(g,tbl,as.table=FALSE,nrow=2,heights = c(4,4),widths=c(8,3)))
  
}