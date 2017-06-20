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
setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/iitb_report/figures/gridplots/")


aggregate_dayplots <- function() {
  # plots with daily data
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/daily/all_Aravalimeters_june_nov2016_daylevel.csv"
  df <- fread(energydata)
  dfs <-  xts(df[,2:dim(df)[2]],fastPOSIXct(df$timestamp)-19800)
  drop_homes <- c("X3","X10","X11")
  submat <- dfs[,!colnames(dfs) %in% drop_homes]
  net_building_consumption(submat)
 agg <- apply(coredata(submat),1,function(x){ # returns sum of NA's as NA otherwise not possible
   ifelse(all(is.na(x)), NA, sum(x,na.rm = TRUE))
 })
 agg_consump <- data.frame(power = agg, timestamp = index(submat))
 agg_consump <- xts(agg_consump$power,fastPOSIXct(agg_consump$timestamp)-19800)
 
 weatherdat <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/weather/dailyweather_complete2016.csv"
 df_masum <- fread(weatherdat)
 df_masum_xts <- xts(df_masum[,2:dim(df_masum)[2]],fastPOSIXct(df_masum$timestamp)-19800)
 df_masum_xts <- df_masum_xts[paste0(start(agg_consump),"/")]
 
 df_mix <- cbind(agg_consump,df_masum_xts)
 colnames(df_mix) <- c("Power","Temperature","Humidity")
 df_fin <- df_mix[!is.na(df_mix$Power),]
 call_python <- function(){
   # in this section, I call python code to plot my plots.
   # KEEP CARE WHILE SAVING PLOT
   library(rPython)
   tt <- df_fin
   tt <- data.frame(timestamp=index(tt),coredata(tt))
   tt$timestamp <- as.character(tt$timestamp)
   python.load("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/IIT-Bombay/python_functions.py",get.exception = TRUE)
  # savepath = "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/iitb_report/figures/"
   #savepath = paste0(savepath,"aravali_daywise.pdf")
   python.call("doubleyaxis",tt,savepath)
   
   #  # PLOTTED VIA PLOTLY
   df_fin <- subset(df_fin,select = -Humidity)
   df_fin <- data.frame(timestamp = index(df_fin),coredata(df_fin))
   df_fin$Power <- df_fin$Power/1000
   df_long <- reshape2::melt(df_fin,id.vars="timestamp")
   g <- ggplot(df_long,aes(timestamp,value, group=variable, col=variable)) + geom_line()
   #g <- labs(x="Days from June to Nov",y ="power")
   ggplotly(g)
 }
 
 #write.csv(data.frame(timestamp=index(df_fin),coredata(df_fin)),file="/Volumes/MacintoshHD2/Users/haroonr/Dropbox/pythoncodes/aravali.csv",row.names = FALSE)
 # STORED AND PLOTTED VIA PYTHON
 df_fin <- subset(df_fin,select = -humidity)
 df_fin <- data.frame(timestamp = index(df_fin),coredata(df_fin))
 df_fin$power <- df_fin$power/1000
 df_long <- reshape2::melt(df_fin,id.vars="timestamp")
 g <- ggplot(df_long,aes(timestamp,value, group=variable, col=variable)) + geom_line()
 g <- labs(x="Days from June to Nov",y =)
}

aggregate_hourplots <- function() {
  # plots with hourly data
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/all_Aravalimeters_june_nov2016.csv"
  df <- fread(energydata)
  dfs <-  xts(df[,2:dim(df)[2]],fastPOSIXct(df$timestamp)-19800)
  drop_homes <- c("X3","X10","X11")
  subdata <- dfs[,!colnames(dfs) %in% drop_homes]
  #submat = subdata
  submat <- subdata['2016-11-14/2016-11-20']
  net_building_consumption(submat)
  agg <- apply(coredata(submat),1,function(x){ # returns sum of NA's as NA otherwise not possible
    ifelse(all(is.na(x)), NA, sum(x,na.rm = TRUE))
  })
  agg_consump <- data.frame(power = agg, timestamp = index(submat))
  agg_consump <- xts(agg_consump$power,fastPOSIXct(agg_consump$timestamp)-19800)
  plot(agg_consump)
  # write.csv(data.frame(timestamp=index(agg_consump),power=coredata(agg_consump)),"/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/aggregate_building2016.csv",row.names = FALSE)
  
  weatherdat <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/weather/hourlyweather_complete2016.csv"
  df_masum <- fread(weatherdat)
  df_masum_xts <- xts(df_masum[,2:dim(df_masum)[2]],fastPOSIXct(df_masum$timestamp)-19800)
  df_masum_xts <- df_masum_xts[paste0(start(agg_consump),"/")]
  
  df_mix <- cbind(agg_consump,df_masum_xts)
  colnames(df_mix) <- c("Power","Temperature","Humidity")
  df_fin <- df_mix[!is.na(df_mix$Power),]
  
  df_fin <- na.approx(df_fin)
  
  call_python <- function(){
    # in this section, I call python code to plot my plots.
    # TAKE CARE WHILE SAVING PLOT
  library(rPython)
  tt <- df_fin
  tt <- data.frame(timestamp=index(tt),coredata(tt))
  tt$timestamp <- as.character(tt$timestamp)
  python.load("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/R_codesDirectory/R_Codes/IIT-Bombay/python_functions.py",get.exception = TRUE)
  # savepath = "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/iitb_report/figures/"
  # savepath = paste0(savepath,"aravali_june.pdf")

  # savepath = "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/iitb_report/figures/"
  # savepath = paste0(savepath,"aravali_august.pdf")
  
  # savepath = "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/iitb_report/figures/"
  # savepath = paste0(savepath,"aravali_sept.pdf")
  
  # savepath = "/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/iitb_report/figures/"
  # savepath = paste0(savepath,"aravali_november.pdf")
  python.call("doubleyaxis",tt,savepath)
  
  }
 # write.csv(data.frame(timestamp=index(df_fin),coredata(df_fin)),file="/Volumes/MacintoshHD2/Users/haroonr/Dropbox/pythoncodes/hourly_aravali.csv",row.names = FALSE)
 #  # PLOTTED VIA PLOTLY
  df_fin <- subset(df_fin,select = -Humidity)
  df_fin <- data.frame(timestamp = index(df_fin),coredata(df_fin))
  df_fin$Power <- df_fin$Power/1000
  df_long <- reshape2::melt(df_fin,id.vars="timestamp")
  g <- ggplot(df_long,aes(timestamp,value, group=variable, col=variable)) + geom_line()
  #g <- labs(x="Days from June to Nov",y ="power")
  ggplotly(g)
  
}

aggregate_minuteplots <- function() {
  #this plots minutely data plots for several days
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/iitb_report/figures/")
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/minutely/aggregate_building2016.csv"
  df <- fread(energydata)
  dfs <-  xts(df$power,fastPOSIXct(df$timestamp)-19800)
  submat <- dfs['2016-08-13']
  
  df_fin <- data.frame(timestamp = index(submat),power = coredata(submat))
  
  g <- ggplot(df_fin,aes(timestamp,power/1000)) + geom_line() 
  g <- g + scale_y_continuous(limits=c(8,45)) 
  g <- g + theme(axis.text = element_text(colour = "black")) + theme_grey(base_size = 12)
  g <- g + labs(x="Time of the Day",y ="Power (kW)")
  g
 # ggsave("aravali_aug13.pdf",height = 4, width = 6, units="in")
  #ggplotly(g)
  
}

plot_heatmaps <- function() {
  # this function is used to plot the heat maps.
  # Solution for missing value representation is at http://stackoverflow.com/a/40865664/3317829
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/all_Aravalimeters_june_nov2016.csv"
  df <- fread(energydata)
  dfs <-  xts(df[,2:dim(df)[2]],fastPOSIXct(df$timestamp)-19800)
  drop_homes <- c("X3","X10","X11")
  subdata <- dfs[,!colnames(dfs) %in% drop_homes]
  #submat<- subdata['2016-11-14/2016-11-20']
  submat <- subdata
  #net_building_consumption(submat)
  agg <- apply(coredata(submat),1,function(x){ # returns sum of NA's as NA otherwise not possible
    ifelse(all(is.na(x)), NA, sum(x,na.rm = TRUE))
  })
  agg_consump <- data.frame(power = agg, timestamp = index(submat))
  agg_consump <- xts(agg_consump$power,fastPOSIXct(agg_consump$timestamp)-19800)
  plot(agg_consump)
# agg_consump <- agg_consump[!is.na(agg_consump)]
  dframe <- data.frame(power=coredata(agg_consump),timestamp=index(agg_consump))
  dframe$power <- dframe$power/1000
  dframe$hour <- lubridate::hour(dframe$timestamp)
  dframe$day <- lubridate::date(dframe$timestamp)
 g <- ggplot(dframe,aes(day,hour)) + geom_tile(aes(fill=power))
 g
 
} 

percentage_calculation <- function() {
  # please read points 1,2 and 3 
  #1: this loop calcalates the percentage of flats belonging to 3 different gps
  labelsy <- clusob$cluster
  rm("clusper")
  for(i in 1:length(unique(labelsy))) {
    c1= (sum(cluslab1 %in% names(labelsy[labelsy==i]))/length(labelsy[labelsy==i])) *100
    c2= (sum(cluslab2 %in% names(labelsy[labelsy==i]))/length(labelsy[labelsy==i])) *100
    c3= (sum(cluslab3 %in% names(labelsy[labelsy==i]))/length(labelsy[labelsy==i])) *100
    if(!exists("clusper")){
      clusper <- c(c1,c2,c3)
    } else {
      clusper  <- cbind(clusper, c(c1,c2,c3))
    }
  }
  clusper
  
  #2: this loop calcalates the percentage of a cluster flats belonging to different clusters obtained 
  labelsy <- clusob$cluster
  rm("cluspercent")
  type1 <- cluslab1[cluslab1 %in% names(labelsy)]
  type2 <- cluslab2[cluslab2 %in% names(labelsy)]
  type3 <- cluslab3[cluslab3 %in% names(labelsy)]
  for(i in 1:length(unique(labelsy))) {
    r1= (sum(type1 %in% names(labelsy[labelsy==i])))/length(type1) *100
    r2= (sum(type2 %in% names(labelsy[labelsy==i])))/length(type2) *100
    r3= (sum(type3 %in% names(labelsy[labelsy==i])))/length(type3) *100
    if(!exists("cluspercent")){
      cluspercent <- c(r1,r2,r3)
    } else {
      cluspercent  <- rbind(cluspercent, c(r1,r2,r3))
    }
  }
  row.names(cluspercent) <- paste0('Dataclus',1:dim(cluspercent)[1])
  colnames(cluspercent) <-  paste0('Familyclus',1:dim(cluspercent)[2])
  cluspercent
  
  #3: Same as above one, but without percentages
  labelsy <- clusob$cluster
  rm("cluspercent")
  type1 <- cluslab1[cluslab1 %in% names(labelsy)]
  type2 <- cluslab2[cluslab2 %in% names(labelsy)]
  type3 <- cluslab3[cluslab3 %in% names(labelsy)]
  for(i in 1:length(unique(labelsy))) {
    r1= (sum(type1 %in% names(labelsy[labelsy==i])))
    r2= (sum(type2 %in% names(labelsy[labelsy==i])))
    r3= (sum(type3 %in% names(labelsy[labelsy==i])))
    if(!exists("cluspercent")){
      cluspercent <- c(r1,r2,r3)
    } else {
      cluspercent  <- rbind(cluspercent, c(r1,r2,r3))
    }
  }
  
  row.names(cluspercent) <- paste0('Dataclus',1:dim(cluspercent)[1])
  colnames(cluspercent) <-  paste0('Familyclus',1:dim(cluspercent)[2])
  cluspercent
}

make_parallel_reltionship_status <- function(df) {
  # this is a paralelzzible function. in which i try to find all the combinations of 40 apartments and then to find which group mainted relationship for large no. of days 
  internfunc <- function(z) {
    library(tidyverse)
    #h <- z %>% map(~apply(df[.x], 1, function(x){n_distinct(x) == 1})) 
    #hh <- h  %>% map_dbl(~sum(.x) / length(.x))    # calculate TRUE proportion
    #return(hh)
    k <- lapply(z,function(x) {
      apply(df[x],1,function(y) {n_distinct(y) ==1} )
    })
    ret <- k  %>% map_dbl(~sum(.x) / length(.x))
    return(ret)
  }
  
  library(parallel)
  no_cores = detectCores()-1# No. of cores in your system
  cl = makeCluster(no_cores) # Make cluster
  library(tidyverse)
  x <- map(2:length(df), ~combn(names(df), 3, simplify = FALSE))    # get combinations
  y <-  x %>% flatten()    # eliminate nesting
  z <-  y %>% set_names(map_chr(., paste0, collapse = ''))    # add useful names
  clusterExport(cl,'df') # pass parameters on-fly to the threads
  #start_time = Sys.time() # start time of parallel computation
  parallel_result = parLapply(cl,z,internfunc)
  #total_time = Sys.time()-start_time # total time taken for computation
  #cat ('Total_parallel_time_taken',total_time)
  stopCluster(cl)
  return(parallel_result)
}

calculate_percentages <- function(subset2,sframe,simscore) {
  # used by the function view_clustering_relationship_along_days
  daydat <- split.xts(subset2,f="days",k=1)
  
  plots <- lapply(daydat, function(x) {
    if(all(is.na(x)))  # for days where entire day data is missing
      return(NULL)
    sframe <- sfile[,c(8,1,3,4,5)]
    sframe$flatId <- paste0('F',sframe$flatId)
    sframe$total <- rowSums(sframe[,2:5])
    set.seed(123)
    # remove rows with all NA
    temp <- apply(x,1,function(x) all(is.na(x)))
    x <- x[!temp,]
    # fill NAs if any
    x <- na.approx(x)
    # customized scaling:  http://stackoverflow.com/a/15364319/3317829 
    x <- apply(x,2,function(y) (y-mean(y))/sd(y)^as.logical(sd(y)))
    # no_clusters <- pamk(t(x),krange = 2:8)
    no_clusters <- 3
    clusob <- pam(t(x), no_clusters)
    labels <- as.factor(clusob$cluster)
    rm(sframe)
    return(clusob) 
  })
  
  plots <- plots[!sapply(plots,is.null)] # remove NULL ELEMENTS
  relation_mat <- t(sapply(plots,function(x) x$clustering)) # create matrix form
  #res <- correlation_analysis(relation_mat)
  relation_mat <- relation_mat[,order(colnames(relation_mat))]
  clus_no <- pamk(t(relation_mat),krange = 2:10)
  clus <- pam(t(relation_mat),clus_no$nc)
  #clusplot(t(relation_mat),clus$clustering,labels = 2)
  
  total_clusters <- length(unique(clus$clustering))# no. of clusters
  gp_mems <- lapply(1:total_clusters,function(x) names(clus$clustering)[which(clus$clustering %in% x)] )
  
  res <- vector()
  max_disimilar_apts <- vector()
  count <- 1
  for(i in 1:length(gp_mems)){
    if(length(gp_mems[[i]])>1){
      temp <- compute_clusteringsimilarity_acrossdays(as.data.frame(relation_mat),gp_mems[[i]])
      res[count] <- list(temp)
      count <- count +1
    } else {
      # Apartements which do not cluster with any other cluster
      max_disimilar_apts <- c(max_disimilar_apts, gp_mems[[i]])
    }
  }
  result <- unlist(res)
  dfh <- data.frame(Group = names(result), score = result, stringsAsFactors = FALSE)
  row.names(dfh) <- NULL
  dfh$size <- sapply(strsplit(dfh$Group,"['F']"),length)-1 #strsplit results in one extra member
  
  dfh_similar <- dfh[dfh$score >= simscore,] # 100 percent similar
  max_gp_size <- max(dfh_similar$size)
  dfh_similar[dfh_similar$size == max_gp_size,]$Group #largest gp maintaing similarity
  max_disimilar_apts  # apartments which do not cluster with anyone
  
  t <- dfh_similar[dfh_similar$size == max_gp_size,]$Group
  for(i in 1:length(t)) {
    mt <- regmatches(t[i], gregexpr(".{4}", t[i]))[[1]]
    print(sframe[sframe$flatId %in% mt,])
  }
  return(list(group=dfh_similar[dfh_similar$size == max_gp_size,]$Group, single = max_disimilar_apts,pop=t))
  
  redundant_code <- function() {
    #CODE BLOCK TO FIND THE RELATION SHIP STATUS ALONG DAYS
    relation_size <- 4
    system.time(results <- compute_clusteringsimilarity_acrossdays(as.data.frame(relation_mat),relation_size))
    make_parallel_reltionship_status(as.data.frame(relation_mat))
    saveob <- list(data = subset2, no_clusters = no_clusters, labels = relation_mat, relation_size = relation_size,    relation_result = results)
    rdspath <-"/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/rds_files/"
    rdspath <- paste0(rdspath,"june13-19-rel-4.rds")
    # saveRDS(saveob,rdspath)
  }
}

view_clustering_relationship_along_days <- function(){
  # this function takes cluster day wise data individually and keeps track how cluster members
  # change their relationship along different days
  surfile <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/surveydata.csv"
  sfile <- read.csv(surfile)
  colnames(sfile)
  sframe <- sfile[,c(8,1,3,4,5)]
  sframe$flatId <- paste0('F',sframe$flatId)
  sframe$total <- rowSums(sframe[,2:5])
  #sframe <- sframe[sframe$flatId!="F236",]#currently, there is duplicate of it
  table(duplicated(sframe$flatId))
  
  readfile <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/onlyservyed_flats.csv"
  df <- fread(readfile)
  dfs <-  xts(df[,2:dim(df)[2]],fastPOSIXct(df$timestamp)-19800)
  
  drop_homes <- c("F209","F239","F216")
  dfs <- dfs[,!colnames(dfs) %in% drop_homes]
  cat(paste0("Dropped ",drop_homes))
  #range of dates for which we need to do clustering
  #
  subset1 <- dfs["2016-08-12 00:00:00/2016-08-18 23:59:59"]
  # Keep only those homes for which we have survey data
  subset2 <- subset1[,colnames(subset1) %in% sframe$flatId]
  message("PLZ. TUNE MONTHS")
  # subset2 <-subset(subset2,select = -F221)
  ## MONTH SPECIFIC ANALYSIS###
  #1 JUNE 13: 19 REMOVE F221 CONTAINS NA'S
  #   subset2 <-subset(subset2,select = -F221)
  simscore <- 0.80 # % similarity you want ot check
  res <- calculate_percentages(subset2,sframe,simscore) 
  
}

cluster_samehome_acrossdays <- function() {
   #In this we take a single apartemtent, and cluster on different days and see it results into how many clusters
  library("factoextra")
  library(dtw)
  readfile <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/onlyservyed_flats.csv"
  df <- fread(readfile)
  dfs <-  xts(df[,2:dim(df)[2]],fastPOSIXct(df$timestamp)-19800)
  drop_homes <- c("F209","F239","F216")
  dfs <- dfs[,!colnames(dfs) %in% drop_homes]
  cat(paste0("Dropped ",drop_homes))
  #range of dates for which we need to do clustering
  subset1 <- dfs["2016-06-13 00:00:00/2016-06-19 23:59:59"]
  daydat <- split.xts(subset1,f="days",k=1)
  
  daydat_singhome <- split.xts(subset1[,4],f="days",k=1)
  column_names <- paste0('D',sapply(daydat_singhome,function(x) unique(lubridate::day(index(x)))))
  samehome <- sapply(daydat_singhome,coredata)
  colnames(samehome) <- column_names
  table(is.na(samehome))
  samehome <- na.approx(samehome)
  #distmat <- dist(t(samehome),method = 'DTW')
  #no_clusters <- pamk(distmat,krange = 1:(dim(samehome)[2]-1),diss=TRUE)
  #clusob <- pam(distmat, no_clusters$nc,diss=TRUE)# forced 6 clusters
  no_clusters <- pamk(t(samehome),krange = 1:(dim(samehome)[2]-1))
  clusob <- pam(t(samehome), no_clusters$nc)# forced 6 clusters
  
  labels <- as.factor(clusob$cluster)
  df <- scale(samehome)
  fviz_cluster(clusob, data = df)+theme_minimal()
  
  samehome <- as.data.frame(samehome)
  samehome_long <- reshape2::melt(samehome)
  samehome_long$ind <- rep(1:(dim(samehome)[1]),dim(samehome)[2])
  g <- ggplot(samehome_long,aes(ind,value,col=variable, group=variable)) + geom_line()
  ggplotly(g)
}

plot_annotated_facets <- function(df,labels,type,title) {
  # this function plots annoted facets clusters, each cluster shows #people,#apartments and energy percentage
  #  Function used by compare_cluster_using_population_distribution
  df_form <- data.frame(timestamp = index(df),coredata(df))
  df_long <- reshape2::melt(df_form,id.vars = "timestamp")
  df_long$cluslabel <- rep(labels, each = dim(df)[1])
  agg_consump <- xts(rowSums(df,na.rm = FALSE),index(df))
  clusters <- gtools::mixedsort(unique(df_long$cluslabel))
  st_features = list(meanf=vector(),sdf= vector())
  
  for(i in 1:length(clusters)) {
    #browser()
    dat <- df_long[df_long$cluslabel==as.factor(clusters[i]),]
    temp <- reshape2::dcast(dat,timestamp~variable)
    temp_data <- subset(temp,select = -timestamp)
    #temp_xts <- xts(temp_data,temp$timestamp)
    temp_xts <- xts(rowSums(temp_data,na.rm = FALSE),temp$timestamp)
    agg_consump <- cbind(agg_consump,temp_xts)
    st_features$meanf[i] <- mean(as.matrix(temp_data),na.rm = TRUE)
    st_features$sdf[i] <- sd(as.matrix(temp_data),na.rm = TRUE)
  }
  colnames(agg_consump) <- c("Aggregate",c(1:length(clusters)))
  #percentage energy consumed by each cluster against the total consumption of building
  per_contribuion <- apply(agg_consump[,2:dim(agg_consump)[2]],2,function(x) mean(x/agg_consump[,1])*100)
  tab_stat <- data.frame(table(labels),round(per_contribuion,2),round(st_features$meanf,2),round(st_features$sdf,2)) 
  colnames(tab_stat) <- c("Cluster#","Apartments","Energy_Consumed(%)","Mean","SD")
  
  # Add no. of people to each residing in each cluster
  if(type =="dataclus"){
    sframe$cluster <- as.factor(sframe$cluster)
    clnames <- gtools::mixedsort(unique(labels))
    tab_stat$people <- sapply(clnames,function(x) {
      colSums(sframe[sframe$cluster == x,]["total"],na.rm = TRUE)
    })
  } else if(type =="popclus"){
    sframe$popcluster <- as.factor(sframe$popcluster)
    clnames <- gtools::mixedsort(unique(labels))
    tab_stat$people <- sapply(clnames,function(x) {
      colSums(sframe[sframe$popcluster == x,]["total"],na.rm = TRUE)
    })  
  }else { # for dataclus criteria 2
    sframe$popcluster_accosir <- as.factor(sframe$popcluster_accosir)
    clnames <- gtools::mixedsort(unique(labels))
    tab_stat$people <- sapply(clnames,function(x) {
      colSums(sframe[sframe$popcluster_accosir == x,]["total"],na.rm = TRUE)
    }) 
  }
  # this will add member names column to the table 
  member <- lapply(clusters, function(x) {
    mems = as.list(na.omit(sframe[sframe$cluster==x,]$flatId))
    mems <- do.call(paste,mems)
  })
  
  g <- ggplot(df_long, aes(timestamp, value/1000, group = variable))+facet_wrap(~cluslabel)+ geom_line(alpha=0.15)
  g <- g + scale_y_continuous(limits = c(0,2.3))
  g <- g + labs(y ="Power (kW)",x="Hour of the day") + theme_grey(base_size = 10)
  g <- g + stat_summary(data = df_long,aes(timestamp,value/1000,group=cluslabel),
                        fun.y = mean,fun.ymin = function(x) mean(x)-sd(x),
                        fun.ymax = function(x) mean(x)+sd(x),
                        geom="smooth",inherit.aes = FALSE,color="red")
  g <- g + scale_x_datetime(breaks = date_breaks("3 hour"), labels = date_format("%H",tz="Asia/Kolkata")) # use scales package
  g <- g + theme(axis.text.x = element_text(angle = 0,hjust = 1),axis.text=element_text(color="black"))
  g <- g + annotate("text",x = df_long$timestamp[11] ,y = 2.1, label = paste0(paste0("#apt: ",tab_stat$Apartments), paste0(", #ppl: ",tab_stat$people),paste0("\nenergy: ",tab_stat$`Energy_Consumed(%)`,"%"), paste0("\nSD: ",tab_stat$SD)) ,color="red",size=4)
  g <- g + ggtitle(title)
  return(g)
  
}

compare_cluster_using_population_distribution <- function() {
  # in this function I compare the clustering results drawn from consumption data only and the clusters obtained using popultion distribution
  surfile <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/surveydata.csv"
  sfile <- read.csv(surfile)
  colnames(sfile)
  sframe <- sfile[,c(8,1,3,4,5)]
  sframe$flatId <- paste0('F',sframe$flatId)
  sframe$total <- rowSums(sframe[,2:5])
  #sframe <- sframe[sframe$flatId!="F236",]#currently, there is duplicate of it
  table(duplicated(sframe$flatId))
  
  readfile <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/onlyservyed_flats.csv"
  df <- fread(readfile)
  dfs <-  xts(df[,2:dim(df)[2]],fastPOSIXct(df$timestamp)-19800)
  
  drop_homes <- c("F209","F239","F216")
  dfs <- dfs[,!colnames(dfs) %in% drop_homes]
  cat(paste0("Dropped ",drop_homes))
  # these labels were obtained using function family_size_clustering
  cluslab1 <- c("F222","F244","F241","F239","F235","F234","F221","F217","F219","F218")
  cluslab2 <- c("F262","F263","F257","F253","F248","F250","F247","F237","F236","F232","F228","F227","F220","F215","F255","F260", "F259","F254","F251","F246","F226","F243","F207","F208","F209","F212")
  cluslab3 <- c("F256","F252","F238","F240","F230","F229","F267","F264","F261")
  elderhomes <- c("F222","F244","F241","F239","F235","F234","F221","F217","F219","F218")
  childrens <- c("F256","F252","F238","F240","F230","F229","F267","F264","F261")
  remaining <- c("F262","F263","F257","F253","F248","F250","F247","F237","F236","F232","F228","F227","F220","F215","F255","F260","F259","F254","F251","F246","F226","F243","F207","F208","F209","F212")
  #range of dates for which we need to do clustering
  subsetx <- dfs["2016-11-1 00:00:00/2016-11-6 23:59:59"]
  suppressWarnings(rm("returnob"))
  for (i in 1:dim(subsetx)[2]) {
    # 1: compute mean across all days and REMEMBER time is set to first day of series
    # 2: Combine all meters in matrix form
    #browser()
    sub <- split.xts(subsetx[,i], f = "days", k = 1)
    sub_mean <- rowMeans(sapply(sub, function(y) return(coredata(y))),na.rm = TRUE)
    sub_xts <- xts(round(sub_mean,2),index(sub[[1]]))
    
    if(!exists("returnob"))
      returnob <- sub_xts
    else
      returnob<- cbind(returnob,sub_xts)
  }
  colnames(returnob) <- colnames(subsetx)
  
  flat_label <- rbind(data.frame(flat=cluslab1,clus= 1),data.frame(flat=cluslab2,clus=2),data.frame(flat=cluslab3,clus=3))
  flat_label_acco_sir <- rbind(data.frame(flat=elderhomes,clus= 1),data.frame(flat=childrens,clus=2),data.frame(flat=remaining,clus=3))
  set.seed(123)
  # customized scaling:  http://stackoverflow.com/a/15364319/3317829 
  scaled_ob <- apply(returnob,2,function(y) (y-mean(y))/sd(y)^as.logical(sd(y)))
 # no_of_clusters <- pamk(t(scaled_ob))$nc
  no_of_clusters <- 2
  message("clusters forced to2")
  distmat <- dist(t(scaled_ob))
  
  #no_of_clusters <- pamk(t(returnob))$nc
  #distmat <- dist(t(returnob))
  clusob <- pam(distmat,no_of_clusters)
  #percentage_calculation() ## calculate %age of famaly clusters in data clusters
  #clusob <- kmeans(t(returnob),6)# forced 6 clusters
  labels <- as.factor(clusob$cluster)
  # add cluster no. to each row in sframe
  sframe$cluster <- clusob$cluster[sframe$flatId]
  visualize_clusterwise_familydistribution()
  fit_lineto_clusters(returnob,labels)
  fit_lineto_clusters_accordingto_apartment_distribution(returnob,flat_label)
  plot_annotated_facets(returnob,labels,type="dataclus",title="Using consumption clusters")
  labels2 <- setNames(flat_label$clus,flat_label$flat)
  tt <- labels2[names(labels2) %in% colnames(returnob)] # only flat labels for which we have data
  sframe$popcluster <- tt[sframe$flatId] # add  pop cluster labels
  labels2 <- tt[colnames(returnob)] # arrange labels in same order as of col names of data
  plot_annotated_facets(returnob,labels2,type="popclus",title="Using Family clusters")
  
  labels3 <- setNames(flat_label_acco_sir$clus,flat_label_acco_sir$flat)
  tt2 <- labels3[names(labels3) %in% colnames(returnob)] # only flat labels for which we have data
  sframe$popcluster_accosir <- tt2[sframe$flatId] # add  pop cluster labels
  labels3 <- tt2[colnames(returnob)] # arrange labels in same order as of col names of data
  plot_annotated_facets(returnob,labels3,type="popcluscri2",title="Using Family clusters criteria 2")
  
}

compute_similarity_relationships_weekelymonthscombine <- function() {
  #This relation_mat contains clustering results of 4 months. By months I mean weeks of 4 months. and the weeks chosen are as on calender dates
  relation_mat <- readRDS("/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/clustering_ob_4weeks4monts.rds")
  clus_no <- pamk(t(relation_mat),krange = 2:10)
  clus <- pam(t(relation_mat),clus_no$nc)
  #clusplot(t(relation_mat),clus$clustering,labels = 2)
  
  total_clusters <- length(unique(clus$clustering))# no. of clusters
  gp_mems <- lapply(1:total_clusters,function(x) names(clus$clustering)[which(clus$clustering %in% x)])
  
  res <- vector()
  max_disimilar_apts <- vector()
  count <- 1
  for(i in 1:length(gp_mems)){
    if(length(gp_mems[[i]])>1){
      temp <- compute_clusteringsimilarity_acrossdays(as.data.frame(relation_mat),gp_mems[[i]])
      res[count] <- list(temp)
      count <- count +1
    } else {
      # Apartements which do not cluster with any other cluster
      max_disimilar_apts <- c(max_disimilar_apts, gp_mems[[i]])
    }
  }
  result <- unlist(res)
  dfh <- data.frame(Group = names(result), score = result, stringsAsFactors = FALSE)
  row.names(dfh) <- NULL
  dfh$size <- sapply(strsplit(dfh$Group,"['F']"),length)-1 #strsplit results in one extra member
  
  dfh_similar <- dfh[dfh$score >= simscore,] # 100 percent similar
  max_gp_size <- max(dfh_similar$size)
  dfh_similar[dfh_similar$size == max_gp_size,]$Group #largest gp maintaing similarity
  max_disimilar_apts  # apartments which do not cluster with anyone
  t <- dfh_similar[dfh_similar$size == max_gp_size,]$Group
  for(i in 1:length(t)) {
    mt <- regmatches(t[i], gregexpr(".{4}", t[i]))[[1]]
    print(sframe[sframe$flatId %in% mt,])
  }
}

PARK_Omaid_paper_temperature_power_relationship <- function() {
  # paper name : Computing Electricity Consumption Profiles from Household Smart Meter Data
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/onlyservyed_flats.csv"
  df <- fread(energydata)
  dfs <-  xts(df[,2:dim(df)[2]],fastPOSIXct(df$timestamp)-19800)
  weatherdat <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/weather/hourlyweather_complete2016.csv"
  df_masum <- fread(weatherdat)
  df_masum_xts <- xts(df_masum[,2:dim(df_masum)[2]],fastPOSIXct(df_masum$timestamp)-19800)
  
  fnames <- colnames(dfs)
  plotlist <- list()
  
  for(i in 1:length(fnames)) {
    flatname <- 'F208'
    #flatname <- fnames[i]
    homedata <- dfs[,flatname]
    submat <- homedata['2016-06-02/2016-11-20 23:59:59']
    
    df_masum_xts <- df_masum_xts[paste0(start(submat),"/",end(submat))]
    agg_data <- cbind(submat,df_masum_xts$TemperatureC)
    agg_data$hour <- lubridate::hour(agg_data)
    fname <- flatname
    colnames(agg_data) <- c("Power","temp","hour")
    plotlist[[i]] <- ggplot(agg_data,aes(temp,Power/1000)) +facet_wrap(~hour) +geom_point(size=0.8)+
      ggtitle(paste0(flatname,"-all days"))
    
  }
  pdf("allflats_june-Nov16.pdf",width=12)
  lapply(plotlist,print)
  dev.off()
  
}

compare_cluster_using_population_distribution_REMOVED_OUTLIERS <- function() {
  # function simiilar to compare_cluster_using_population_distribution() except that outiers are removed beforehand
  # in this function I compare the clustering results drawn from consumption data only and the clusters obtained using popultion distribution
  surfile <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/surveydata.csv"
  sfile <- read.csv(surfile)
  colnames(sfile)
  sframe <- sfile[,c(8,1,3,4,5)]
  sframe$flatId <- paste0('F',sframe$flatId)
  sframe$total <- rowSums(sframe[,2:5])
  #sframe <- sframe[sframe$flatId!="F236",]#currently, there is duplicate of it
  table(duplicated(sframe$flatId))
  
  readfile <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/onlyservyed_flats.csv"
  df <- fread(readfile)
  dfs <-  xts(df[,2:dim(df)[2]],fastPOSIXct(df$timestamp)-19800)
  
  drop_homes <- c("F209","F239","F216")
  dfs <- dfs[,!colnames(dfs) %in% drop_homes]
  cat(paste0("Dropped ",drop_homes))
  # these labels were obtained using function family_size_clustering
  cluslab1 <- c("F222","F244","F241","F239","F235","F234","F221","F217","F219","F218")
  cluslab2 <- c("F262","F263","F257","F253","F248","F250","F247","F237","F236","F232","F228","F227","F220","F215","F255","F260", "F259","F254","F251","F246","F226","F243","F207","F208","F209","F212")
  cluslab3 <- c("F256","F252","F238","F240","F230","F229","F267","F264","F261")
  #range of dates for which we need to do clustering
  subsetx <- dfs["2016-11-1 00:00:00/2016-11-6 23:59:59"]
  suppressWarnings(rm("returnob"))
  for (i in 1:dim(subsetx)[2]) {
    # 1: compute mean across all days and REMEMBER time is set to first day of series
    # 2: Combine all meters in matrix form
    #browser()
    sub <- split.xts(subsetx[,i], f = "days", k = 1)
    # sub_mean <- rowMeans(sapply(sub, function(y) return(coredata(y))),na.rm = TRUE)
    # REMOVING OUTLIERS
    # browser()
    sub2 <- sapply(sub, function(y) return(coredata(y)))
    message("Dropping outliers")
    sub_mean <- apply(sub2,1,function(y) { 
      lower = quantile(y,0.10,na.rm = TRUE)
      upper = quantile(y,0.90,na.rm = TRUE)
      z <- y[y >= lower & y <= upper]
      z <- mean(z,na.rm = TRUE)
    })
    sub_xts <- xts(round(sub_mean,2),index(sub[[1]]))
    
    if(!exists("returnob"))
      returnob <- sub_xts
    else
      returnob<- cbind(returnob,sub_xts)
  }
  colnames(returnob) <- colnames(subsetx)
  
  flat_label <- rbind(data.frame(flat=cluslab1,clus= 1),data.frame(flat=cluslab2,clus=2),data.frame(flat=cluslab3,clus=3))
  
  set.seed(123)
  # customized scaling:  http://stackoverflow.com/a/15364319/3317829 
  scaled_ob <- apply(returnob,2,function(y) (y-mean(y))/sd(y)^as.logical(sd(y)))
  # no_of_clusters <- pamk(t(scaled_ob))$nc
  no_of_clusters <- 2
  message("clusters forced to2")
  distmat <- dist(t(scaled_ob))
  
  #no_of_clusters <- pamk(t(returnob))$nc
  #distmat <- dist(t(returnob))
  clusob <- pam(distmat,no_of_clusters)
  #percentage_calculation() ## calculate %age of famaly clusters in data clusters
  #clusob <- kmeans(t(returnob),6)# forced 6 clusters
  labels <- as.factor(clusob$cluster)
  # add cluster no. to each row in sframe
  sframe$cluster <- clusob$cluster[sframe$flatId]
  visualize_clusterwise_familydistribution()
  fit_lineto_clusters(returnob,labels)
  fit_lineto_clusters_accordingto_apartment_distribution(returnob,flat_label)
  plot_annotated_facets(returnob,labels,type="dataclus",title="Using consumption clusters")
  labels2 <- setNames(flat_label$clus,flat_label$flat)
  tt <- labels2[names(labels2) %in% colnames(returnob)] # only flat labels for which we have data
  sframe$popcluster <- tt[sframe$flatId] # add  pop cluster labels
  labels2 <- tt[colnames(returnob)] # arrange labels in same order as of col names of data
  plot_annotated_facets(returnob,labels2,type="popclus",title="Using Family clusters")
  
}

plot_per_apartment_per_month <- function() {
  # this function plots apartment wise data on month basis separtely.
  # tempfile output is saved in figures folder of iitb_report
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/onlyservyed_flats.csv"
  df <- fread(energydata)
  dfs <-  xts(df[,2:dim(df)[2]],fastPOSIXct(df$timestamp)-19800)
  subob <- list()
  subob[[1]] <- dfs['2016-06-02/2016-06-30 23:59:59']
  subob[[2]] <- dfs['2016-07-01/2016-07-30 23:59:59']
  subob[[3]] <- dfs['2016-08-01/2016-08-30 23:59:59']
  subob[[4]] <- dfs['2016-09-01/2016-09-30 23:59:59']
  subob[[5]] <- dfs['2016-10-01/2016-10-30 23:59:59']
  subob[[6]] <- dfs['2016-11-01/2016-11-23 23:59:59']
  subob[[7]] <- dfs['2016-06-01/2016-11-23 23:59:59'] # all months
  
  pdf("tempfile.pdf",width=14,height = 10)
  for(i in 1:length(subob)){
    
    subob_df <- data.frame(timestamp=index(subob[[i]]),coredata(subob[[i]]))
    tempdf <- reshape2::melt(subob_df,id.vars="timestamp")
    colnames(tempdf) <- c("time","var","val")
    tempdf$day <- lubridate::date(tempdf$time)
    tempdf$hour <- lubridate::hour(tempdf$time)
    g <- ggplot(tempdf,aes(hour,val/1000)) + facet_wrap(~var) + geom_line(aes(group=day)) +
      labs(y="power (kW)", x = "Hour of a day")
    
    g <- g + stat_summary(data = tempdf,aes(hour,val/1000,group=var),
                          fun.y = mean,
                          geom="line",inherit.aes = FALSE,color="red")
    plot(g)
    
  }
  dev.off()
}

plot_per_apartment_per_month_15minutesdata <- function() {
  # this function plots apartment wise data on month basis separtely.
  # tempfile output is saved in figures folder of iitb_report
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/15minutes/onlyservyed_flats.csv"
  df <- fread(energydata)
  dfs <-  xts(df[,2:dim(df)[2]],fastPOSIXct(df$timestamp)-19800)
  subob <- list()
  subob[[1]] <- dfs['2016-06-02/2016-06-30 23:59:59']
  subob[[2]] <- dfs['2016-07-01/2016-07-30 23:59:59']
  subob[[3]] <- dfs['2016-08-01/2016-08-30 23:59:59']
  subob[[4]] <- dfs['2016-09-01/2016-09-30 23:59:59']
  subob[[5]] <- dfs['2016-10-01/2016-10-30 23:59:59']
  subob[[6]] <- dfs['2016-11-01/2016-11-23 23:59:59']
  subob[[7]] <- dfs['2016-06-01/2016-11-23 23:59:59'] # all months
  
  pdf("tempfile_15minute.pdf",width=14,height = 10)
  for(i in 1:length(subob)){
    
    subob_df <- data.frame(timestamp=index(subob[[i]]),coredata(subob[[i]]))
    tempdf <- reshape2::melt(subob_df,id.vars="timestamp")
    colnames(tempdf) <- c("time","var","val")
    tempdf$day <- lubridate::date(tempdf$time)
    #tempdf$hour <- lubridate:hour(tempdf$time)
    tempdf$hour <- lubridate::hour(tempdf$time)*60+lubridate::minute(tempdf$time)
    g <- ggplot(tempdf,aes(hour,val/1000)) + facet_wrap(~var) + geom_line(aes(group=day)) +
      labs(y="power (kW)", x = "Minutes of a day")
    
    g <- g + stat_summary(data = tempdf,aes(hour,val/1000,group=var),
                          fun.y = mean,
                          geom="line",inherit.aes = FALSE,color="red")
    plot(g)
    
  }
  dev.off()
}

cross_validatedqestion_21December <- function() {
  # this function plots apartment wise data on month basis separtely.
  # tempfile output is saved in figures folder of iitb_report
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/onlyservyed_flats.csv"
  df <- fread(energydata)
  dfs <-  xts(df[,2:dim(df)[2]],fastPOSIXct(df$timestamp)-19800)
  # subob <- list()
  subob <- dfs['2016-06-02/2016-06-30 23:59:59']
  sel <- c("F238",'F262','F267','F264')
  subob <- subob[,sel]
  
  pdf("tempfile.pdf",width=14,height = 10)
  
  
  subob_df <- data.frame(timestamp=index(subob),coredata(subob))
  tempdf <- reshape2::melt(subob_df,id.vars="timestamp")
  colnames(tempdf) <- c("time","var","val")
  tempdf$day <- lubridate::date(tempdf$time)
  tempdf$hour <- lubridate::hour(tempdf$time)
  g <- ggplot(tempdf,aes(hour,val/1000)) + facet_wrap(~var) + geom_line(aes(group=day)) +
    labs(y=" Value", x = "Hour of a day")
  
  plot(g)
  
  
  dev.off()
}

peak_detection_approaches_crossvalidation <- function() {
  # APPROACH 1
  argmax <- function(x, y, w=1, ...) {
    require(zoo)
    n <- length(y)
    y.smooth <- loess(y ~ x, ...)$fitted
    y.max <- rollapply(zoo(y.smooth), 2*w+1, max, align="center")
    delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
    i.max <- which(delta == 0) + w
    list(x=x[i.max], i=i.max, y.hat=y.smooth)
  }
  
  test <- function(w, span) {
    peaks <- argmax(x, y, w=w, span=span)
    
    plot(x, y, cex=0.75, col="Gray", main=paste("w = ", w, ", span = ", span, sep=""))
    lines(x, peaks$y.hat,  lwd=2) #$
    y.min <- min(y)
    sapply(peaks$i, function(i) lines(c(x[i],x[i]), c(y.min, peaks$y.hat[i]),
                                      col="Red", lty=2))
    points(x[peaks$i], peaks$y.hat[peaks$i], col="Red", pch=19, cex=1.25)
  }
  x <- 1:1000 / 100 - 5
  y <- exp(abs(x)/20) * sin(2 * x + (x/5)^2) + cos(10*x) / 5 + rnorm(length(x), sd=0.05)
  par(mfrow=c(3,1))
  test(2, 0.05)
  test(30,0.05)
  test(2,0.2)
  # APPROACH 2
  ThresholdingAlgo <- function(y,lag,threshold,influence) {
    signals <- rep(0,length(y))
    filteredY <- y[0:lag]
    avgFilter <- NULL
    stdFilter <- NULL
    avgFilter[lag] <- mean(y[0:lag])
    stdFilter[lag] <- sd(y[0:lag])
    for (i in (lag+1):length(y)){
      if (abs(y[i]-avgFilter[i-1]) > threshold*stdFilter[i-1]) {
        if (y[i] > avgFilter[i-1]) {
          signals[i] <- 1;
        } else {
          signals[i] <- -1;
        }
        filteredY[i] <- influence*y[i]+(1-influence)*filteredY[i-1]
        avgFilter[i] <- mean(filteredY[(i-lag):i])
        stdFilter[i] <- sd(filteredY[(i-lag):i])
      } else {
        signals[i] <- 0
        filteredY[i] <- y[i]
        avgFilter[i] <- mean(filteredY[(i-lag):i])
        stdFilter[i] <- sd(filteredY[(i-lag):i])
      }
    }
    return(list("signals"=signals,"avgFilter"=avgFilter,"stdFilter"=stdFilter))
  }
  
  y <- c(1,1,1.1,1,0.9,1,1,1.1,1,0.9,1,1.1,1,1,0.9,1,1,1.1,1,1,1,1,1.1,0.9,1,1.1,1,1,0.9,
         1,1.1,1,1,1.1,1,0.8,0.9,1,1.2,0.9,1,1,1.1,1.2,1,1.5,1,3,2,5,3,2,1,1,1,0.9,1,1,3,
         2.6,4,3,3.2,2,1,1,0.8,4,4,2,2.5,1,1,1)
  
  # Run algo with lag = 30, threshold = 5, influence = 0
  result <- ThresholdingAlgo(y,30,5,0)
  par(mfrow = c(2,1),oma = c(2,2,0,0) + 0.1,mar = c(0,0,2,1) + 0.2)
  plot(1:length(y),y,type="l",ylab="",xlab="") 
  lines(1:length(y),result$avgFilter,type="l",col="cyan",lwd=2)
  lines(1:length(y),result$avgFilter+threshold*result$stdFilter,type="l",col="green",lwd=2)
  lines(1:length(y),result$avgFilter-threshold*result$stdFilter,type="l",col="green",lwd=2)
  plot(result$signals,type="S",col="red",ylab="",xlab="",ylim=c(-1.5,1.5),lwd=2)
}

consistency_percentage_apartments <- function() {
  
  # this function is used to compute the percentage of historical days, which followed consistent pattern in consumption
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/onlyservyed_flats.csv"
  df <- fread(energydata)
  dfs <-  xts(df[,2:dim(df)[2]],fastPOSIXct(df$timestamp)-19800)
  # subob <- list()
  subob <- dfs['2016-06-02/2016-06-30 23:59:59']
  sel <- c("F238",'F262','F267','F264')
  #subob <- subob[,sel]
  
  # ONE FLAT ONLY
  dat <-  subob[,"F267"]
  daydat <- split.xts(dat,f="days",k=1)
  daymat <- sapply(daydat,function(x) coredata(x))
  colnames(daymat) <- paste0('D',1:dim(daymat)[2])
  flag <- apply(daymat,2,function(x) any(is.na(x)))
  daymat <- daymat[,!flag]
  daymat_xts <- xts(daymat, index(daydat[[1]]))
  # stat dataframe with mean and standard devation
  stat <- xts(data.frame(rowmean = rowMeans(daymat_xts,na.rm = TRUE)),index(daydat[[1]]))
  stat <- cbind(stat,xts(data.frame(rowsd=apply(as.matrix(coredata(daymat_xts)),1,sd,na.rm=TRUE)),index(daydat[[1]])))
  
  status <- vector()
  for( i in 1:dim(daymat_xts)[2]) {
    status[i] <- all((daymat_xts[,i] <= (stat$rowmean + 2*stat$rowsd)) & ( daymat_xts[,i] >= (stat$rowmean - 2*stat$rowsd) ))
  } 
  # percentage of times this apartment has been consistent in the past
  sum(status,na.rm = TRUE)/length(status)*100
  
  df_melt <- reshape2::melt(data.frame(timestamp=index(daymat_xts),coredata(daymat_xts)),id.vars="timestamp")
  
  
  
  g <- ggplot(df_melt,aes(timestamp,value, group=variable,col=variable)) + geom_line() + stat_summary(data = df_melt,aes(timestamp,value),fun.y = mean,fun.ymin = function(x) mean(x)-sd(x),fun.ymax = function(x) mean(x)+sd(x),geom="smooth",inherit.aes = FALSE,color="red")
  g
  
  g <- ggplot(df_melt,aes(timestamp,value, group=variable)) + geom_line() +geom_line(data=fortify(stat),aes(Index,rowmean),size=2,col="red",inherit.aes = FALSE)
  g
  
  
  extracode <- function() {
    ## Facet wrap of 4 flats 
    subob_df <- data.frame(timestamp=index(subob),coredata(subob))
    tempdf <- reshape2::melt(subob_df,id.vars="timestamp")
    colnames(tempdf) <- c("time","var","val")
    tempdf$day <- lubridate::date(tempdf$time)
    tempdf$hour <- lubridate::hour(tempdf$time)
    g <- ggplot(tempdf,aes(hour,val/1000)) + facet_wrap(~var) + geom_line(aes(group=day)) +
      labs(y=" Value", x = "Hour of a day")
    g
    
    # CORRELATION APPROACH
    dat <-  subob[,"F238"]
    daydat <- split.xts(dat,f="days",k=1)
    daymat <- sapply(daydat,function(x) coredata(x))
    colnames(daymat) <- paste0('D',1:dim(daymat)[2])
    cormat <- cor(daymat[,8:12])
    cormat[lower.tri(cormat)]
    temp <- as.data.frame(daymat[,8:12])
    temp$ind <- c(1:24)
    temp_melt <- reshape2::melt(temp,id.vars="ind")
    g <- ggplot(temp_melt,aes(ind,value, group=variable,color=variable)) + geom_line()
    g
  }
}

consistency_percentage_apartments <- function() {
  
  # this function is used to compute the percentage of historical days, which followed consistent pattern in consumption
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/onlyservyed_flats.csv"
  df <- fread(energydata)
  dfs <-  xts(df[,2:dim(df)[2]],fastPOSIXct(df$timestamp)-19800)
  # subob <- list()
  #subob <- dfs['2016-06-02/2016-06-30 23:59:59']
  #sel <- c("F238",'F262','F267','F264')
  #subob <- subob[,sel]
  
  # ONE FLAT ONLY
  dat_flat <-  dfs[,"F267"]
  dat <- dat_flat['2016-06-02/2016-06-12 23:59:59']
  res <- compute_relevancescore(dat)
  res$score
  plot(res$profile)
  
}

compute_relevancescore  <- function(dat) {
  daydat <- split.xts(dat,f="days",k=1)
  daymat <- sapply(daydat,function(x) coredata(x))
  colnames(daymat) <- paste0('D',1:dim(daymat)[2])
  flag <- apply(daymat,2,function(x) any(is.na(x)))
  daymat <- daymat[,!flag]
  daymat_xts <- xts(daymat, index(daydat[[1]]))
  # stat dataframe with mean and standard devation
  stat <- xts(data.frame(rowmean = rowMeans(daymat_xts,na.rm = TRUE)),index(daydat[[1]]))
  stat <- cbind(stat,xts(data.frame(rowsd=apply(as.matrix(coredata(daymat_xts)),1,sd,na.rm=TRUE)),index(daydat[[1]])))
  
  status <- vector()
  for( i in 1:dim(daymat_xts)[2]) {
    status[i] <- all((daymat_xts[,i] <= (stat$rowmean + 2*stat$rowsd)) & ( daymat_xts[,i] >= (stat$rowmean - 2*stat$rowsd) ))
  } 
  # percentage of times this apartment has been consistent in the past
  score <- sum(status,na.rm = TRUE)/length(status)*100
  
  df_melt <- reshape2::melt(data.frame(timestamp=index(daymat_xts),coredata(daymat_xts)),id.vars="timestamp")
  df_melt$value <- df_melt$value/1000
  g <- ggplot(df_melt,aes(timestamp,value, group=variable,col=variable)) + geom_line(show.legend = FALSE) + stat_summary(data = df_melt,aes(timestamp,value),fun.y = mean,fun.ymin = function(x) mean(x)-2*sd(x),fun.ymax = function(x) mean(x)+2*sd(x),geom="smooth",inherit.aes = FALSE,color="red") 
  g <- g + theme_grey() + labs(x = "Hour of Day", y = " Power (kW)") + scale_x_datetime(breaks = date_breaks("3 hour"),date_labels = "%H",timezone = "Asia/Kolkata") + theme(axis.text = element_text(color="black"),plot.title = element_text(hjust=0.5)) 
  #  g <- g + annotate("text", x = df_melt$timestamp[6] , y = max(df_melt$value), label = paste0("Consistency Score = ",round(score,1),"%"),fontface=2) 
  g <- g +ggtitle(paste0("Consistency Score = ",round(score,1),"%"))
  plot(g)
  # SECOND VERSION
  g <- ggplot(df_melt,aes(timestamp,value, group=variable)) + geom_line() +geom_line(data=fortify(stat),aes(Index,rowmean),size=2,col="red",inherit.aes = FALSE)
  g
  
  return(list(score=score,profile=stat$rowmean))
  extracode <- function() {
    ## Facet wrap of 4 flats 
    subob_df <- data.frame(timestamp=index(subob),coredata(subob))
    tempdf <- reshape2::melt(subob_df,id.vars="timestamp")
    colnames(tempdf) <- c("time","var","val")
    tempdf$day <- lubridate::date(tempdf$time)
    tempdf$hour <- lubridate::hour(tempdf$time)
    g <- ggplot(tempdf,aes(hour,val/1000)) + facet_wrap(~var) + geom_line(aes(group=day)) +
      labs(y=" Value", x = "Hour of a day")
    g
    
    # CORRELATION APPROACH
    dat <-  subob[,"F238"]
    daydat <- split.xts(dat,f="days",k=1)
    daymat <- sapply(daydat,function(x) coredata(x))
    colnames(daymat) <- paste0('D',1:dim(daymat)[2])
    cormat <- cor(daymat[,8:12])
    cormat[lower.tri(cormat)]
    temp <- as.data.frame(daymat[,8:12])
    temp$ind <- c(1:24)
    temp_melt <- reshape2::melt(temp,id.vars="ind")
    g <- ggplot(temp_melt,aes(ind,value, group=variable,color=variable)) + geom_line()
    g
  }
  
}

compute_relevancescore_outputggplotonly  <- function(dat,obs_per_day) {
  daydat <- split.xts(dat,f="days",k=1)
  daylen <- sapply(daydat,length)
  keep <- daylen >= obs_per_day
  daydat <-  daydat[keep]
  daymat <- sapply(daydat,function(x) coredata(x))
  colnames(daymat) <- paste0('D',1:dim(daymat)[2])
  print(dim(daymat))
 # browser()
  flag <- apply(daymat,2,function(x) any(is.na(x)))
  print (paste0("removing",sum(flag)))
  daymat <- daymat[,!flag]
  daymat_xts <- xts(daymat, index(daydat[[1]]))
  
  rowMedian <- function(x, na.rm = FALSE)
  {
    apply(x, 1, median, na.rm = na.rm) 
  }
  # stat dataframe with mean and standard devation
  stat <- xts(data.frame(rowmean = rowMeans(daymat_xts,na.rm = TRUE)),index(daydat[[1]]))
  # stat <- xts(data.frame(rowmean = rowMedian(daymat_xts,na.rm = TRUE)),index(daydat[[1]]))
  stat <- cbind(stat,xts(data.frame(rowsd=apply(as.matrix(coredata(daymat_xts)),1,sd,na.rm=TRUE)),index(daydat[[1]])))
  
  status <- vector()
  for( i in 1:dim(daymat_xts)[2]) {
    status[i] <- all((daymat_xts[,i] <= (stat$rowmean + 2*stat$rowsd)) & ( daymat_xts[,i] >= (stat$rowmean - 2*stat$rowsd) ))
  } 
  # percentage of times this apartment has been consistent in the past
  score <- sum(status,na.rm = TRUE)/length(status)
  
  df_melt <- reshape2::melt(data.frame(timestamp=index(daymat_xts),coredata(daymat_xts)),id.vars="timestamp")
  df_melt$value <- df_melt$value/1000
  g <- ggplot(df_melt,aes(timestamp,value, group=variable)) + geom_line(show.legend = FALSE) + stat_summary(data = df_melt,aes(timestamp,value),fun.y = median,fun.ymin = function(x)
  { s = median(x)-2*sd(x)
  if(s < 0)
  { s = 0}
  return(s)
  },fun.ymax = function(x) median(x) + 2*sd(x), geom="line", inherit.aes = FALSE, color="red",size=1) 
  g <- g + theme_grey() + labs(x = "Hour of Day", y = " Power (kW)") + scale_x_datetime(breaks = date_breaks("3 hour"),date_labels = "%H",timezone = "Asia/Kolkata") + theme(axis.text = element_text(color="black"),plot.title = element_text(hjust=0.5)) 
  #  g <- g + annotate("text", x = df_melt$timestamp[6] , y = max(df_melt$value), label = paste0("Consistency Score = ",round(score,1),"%"),fontface=2) 
  g <- g +ggtitle(paste0("Score = ",round(score,1)))
  # g
  # SECOND VERSION
  # g <- ggplot(df_melt,aes(timestamp,value, group=variable)) + geom_line() +geom_line(data=fortify(stat),aes(Index,rowmean),size=2,col="red",inherit.aes = FALSE)
  #  g
  
  return(g)
  
  
}

compute_relevancescore_outputggplotonly_ver2  <- function(dat,obs_per_day) {
  # this version performs interpolation to fill missing readings
  dat <- interpolate_missing_readings(dat,"1 hour")
  daydat <- split.xts(dat,f="days",k=1)
  daylen <- sapply(daydat,length)
  keep <- daylen >= obs_per_day
  daydat <-  daydat[keep]
  daymat <- sapply(daydat,function(x) coredata(x))
  colnames(daymat) <- paste0('D',1:dim(daymat)[2])
  print(dim(daymat))
   #browser()
  flag <- apply(daymat,2,function(x) any(is.na(x)))
  print (paste0("removing",sum(flag)))
  daymat <- daymat[,!flag]
  daymat_xts <- xts(daymat, index(daydat[[1]]))
  
  rowMedian <- function(x, na.rm = FALSE)
  {
    apply(x, 1, median, na.rm = na.rm) 
  }
  # stat dataframe with mean and standard devation
  stat <- xts(data.frame(rowmean = rowMeans(daymat_xts,na.rm = TRUE)),index(daydat[[1]]))
  # stat <- xts(data.frame(rowmean = rowMedian(daymat_xts,na.rm = TRUE)),index(daydat[[1]]))
  stat <- cbind(stat,xts(data.frame(rowsd=apply(as.matrix(coredata(daymat_xts)),1,sd,na.rm=TRUE)),index(daydat[[1]])))
  
  status <- vector()
  for( i in 1:dim(daymat_xts)[2]) {
    status[i] <- all((daymat_xts[,i] <= (stat$rowmean + 2*stat$rowsd)) & ( daymat_xts[,i] >= (stat$rowmean - 2*stat$rowsd) ))
  } 
  # percentage of times this apartment has been consistent in the past
  score <- sum(status,na.rm = TRUE)/length(status)
  
  df_melt <- reshape2::melt(data.frame(timestamp=index(daymat_xts),coredata(daymat_xts)),id.vars="timestamp")
  df_melt$value <- df_melt$value/1000
  g <- ggplot(df_melt,aes(timestamp,value, group=variable)) + geom_line(show.legend = FALSE) + stat_summary(data = df_melt,aes(timestamp,value),fun.y = median,fun.ymin = function(x)
  { s = median(x)-2*sd(x)
  if(s < 0)
  { s = 0}
  return(s)
  },fun.ymax = function(x) median(x) + 2*sd(x), geom="line", inherit.aes = FALSE, color="red",size=1) 
  g <- g + theme_grey() + labs(x = "Hour of Day", y = " Power (kW)") + scale_x_datetime(breaks = date_breaks("3 hour"),date_labels = "%H",timezone = "Asia/Kolkata") + theme(axis.text = element_text(color="black"),plot.title = element_text(hjust=0.5)) 
  #  g <- g + annotate("text", x = df_melt$timestamp[6] , y = max(df_melt$value), label = paste0("Consistency Score = ",round(score,1),"%"),fontface=2) 
  g <- g +ggtitle(paste0("Score = ",round(score,1)))
  # g
  # SECOND VERSION
  # g <- ggplot(df_melt,aes(timestamp,value, group=variable)) + geom_line() +geom_line(data=fortify(stat),aes(Index,rowmean),size=2,col="red",inherit.aes = FALSE)
  #  g
  
  return(g)
  
  
}



argmax <- function(x, y, w = 1, ...) {
  require(zoo)
  n <- length(y)
  y.smooth <- loess(y ~ x, ...)$fitted
  y.max <- rollapply(zoo(y.smooth), 2*w+1, max, align="center")
  delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
  i.max <- which(delta == 0) + w
  list(x=x[i.max], i=i.max, y.hat=y.smooth)
}

compute_and_showPeaks <- function(profiledat,w,span) {
  # Original Link: http://stats.stackexchange.com/a/36326/60072
  pf_df <- fortify(profiledat)
  pf_df$hour <- lubridate::hour(pf_df$Index)
  peaks <- argmax(pf_df$hour+1, pf_df$rowmean/1000, w=w, span=span)
  dframe <- data.frame(x=pf_df$hour,y=peaks$y.hat)
  # dframe2 <- data.frame(x=pf_df$hour,y=pf_df$rowmean)
  anno_frame <- data.frame(x2=peaks$i-1,y2=peaks$y.hat[peaks$i])
  anno_frame$x1 <- anno_frame$x2
  anno_frame$y1 <- min(peaks$y.hat)
  
  h <- ggplot(dframe,aes(x,y)) + geom_line() +
    geom_segment(data=anno_frame,aes(x=x1,y=y1,xend=x2,yend=y2),col="red",linetype=2) + geom_point(data = anno_frame,aes(x=x2,y=y2),col="red",size = 2)
  h <- h + theme_grey() + labs(x = "Hour of Day", y = " Power (kW)") + theme(axis.text = element_text(color="black"))
  plot(h)
  peak_loc = data.frame(hour= anno_frame$x1, peak_value= anno_frame$y2)
  return(peak_loc)
  
}

peak_res <- compute_and_showPeaks(profiledat,w=2,span=0.1)

show_gridformat_consistencyscore() <- function() {
  # this function takes matrix data of homes and then one by one homes, it calculates consistency score on
  # basis of last 10 days
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/onlyservyed_flats.csv"
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/iitb_report/figures/gridplots_ver4/")
  
  df <- fread(energydata)
  df_xts <-  xts(df[,2:dim(df)[2]],fasttime::fastPOSIXct(df$timestamp)-19800)
  dfs <- df_xts["2016-06-09/2016-06-22 23:59:59"]
  keep <- !apply(dfs,2,function(x) any(is.na(as.vector(x))))
  dfs_keep <- dfs[,keep]
  # ONE FLAT ONLY
  # VERSION 1
  for (j in 1:dim(dfs_keep)[2]) {
    #dat_flat <-  dfs[,"F267"]
    dat_flat <-  dfs_keep[,j]
    plot <- compute_relevancescore_outputggplotonly_ver2(dat_flat,"1 hour")
    fname <- names(dat_flat)
    ggsave(paste0(fname,".pdf"),plot)
  }
  
  # VERSION 2
   keep2 <- !colnames(dfs) %in% c("F208","F221")
   dfs_keep <- dfs[,keep2]
   for (j in 1:dim(dfs_keep)[2]) {
  #dat_flat <-  dfs[,"F267"]
      dat_flat <-  dfs_keep[,j]
      gp_data <- split.xts(dat_flat,f="days",k=10)
      plots <- list()
     for(i in 1:length(gp_data)) {
       plots[[i]] <- compute_relevancescore_outputggplotonly_ver2(gp_data[[i]],"1 hour")
     }

     l <- gridExtra::marrangeGrob(plots,ncol=2,nrow = 2)
     fname <- names(dat_flat)
     ggsave(paste0(fname,".pdf"),l)
  }
}

show_gridformat_selecteddays_consistencyscore() <- function() {
  # this function takes matrix data of homes and then one by one homes, it calculates consistency score on basis of last 10 days
  # Figure 2 in consitency paper is plotted using this only
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/iitb_report/figures/gridplots/")
  
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/onlyservyed_flats.csv"
  df <- fread(energydata)
  dfs <-  xts(df[,2:dim(df)[2]],fasttime::fastPOSIXct(df$timestamp)-19800)
  
  # ONE FLAT ONLY
  selhomes <- c("F228","F235","F240","F234","F257","F237","F247","F256")
  pos <- list(c(7,1,6,15),c(7,15,8),c(6,7,8,12),6,13,c(8,7),16,3)
  pos_up <- lapply(pos, function(x) x=x+1) # since first entry resulted in some problems
  rm("mixhomes") # mixhomes basically mixes data from different homes. Each list element represents data of 10 consecutive 10 days
  for (j in 1:length(selhomes)) {
    dat_flat <-  dfs[,selhomes[j]]
    #dat_flat <- dfs[,j]
    gp_data <- split.xts(dat_flat,f="days",k=10)
    if(!exists("mixhomes")){
      mixhomes <-  gp_data[pos_up[[j]]]
    } else {
      mixhomes <- c(mixhomes,gp_data[pos_up[[j]]])
    }
  }
  plots <- list()
  for(i in 1:(length(mixhomes))) {
    plots[[i]] <- compute_relevancescore_outputggplotonly(mixhomes[[i]])
  }
  l <- marrangeGrob(plots,ncol=3,nrow = 3)
  fname <- "fewhomes"
  #ggsave(paste0(fname,".pdf"),l,width=14,height = 12)
}

show_gridformat_selecteddays_consistencyscore_verticalsplitting() <- function() {
  # this function takes matrix data of homes and then one by one homes, it calculates consistency score on basis of last 10 days. This function differs from show_gridformat_selecteddays_consistencyscore() becuase it plots on those plots were it is evident that vertical splitting will be more benefiticial
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/iitb_report/predictibility/figures/")
  
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/onlyservyed_flats.csv"
  df <- fread(energydata)
  dfs <-  xts(df[,2:dim(df)[2]],fasttime::fastPOSIXct(df$timestamp)-19800)
  
  # ONE FLAT ONLY
  selhomes <- c("F235","F247","F250")
  pos <- list(c(5,13,15),c(5),c(6))
  pos_up <- lapply(pos, function(x) x=x+1) # since first entry resulted in some problems
  rm("mixhomes")
  for (j in 1:length(selhomes)) {
    dat_flat <-  dfs[,selhomes[j]]
    #dat_flat <- dfs[,j]
    gp_data <- split.xts(dat_flat,f="days",k=10)
    if(!exists("mixhomes")){
      mixhomes <-  gp_data[pos_up[[j]]]
    } else {
      mixhomes <- c(mixhomes,gp_data[pos_up[[j]]])
    }
  }
  plots <- list()
  for(i in 1:(length(mixhomes))) {
    plots[[i]] <- compute_relevancescore_outputggplotonly(mixhomes[[i]])
  }
  #l <- marrangeGrob(plots,ncol=3,nrow = 3)
  plot_list <- c(1, 2, 4, 5)
  
  for(h in 1:length(plot_list)) {
    plots[[plot_list[h]]]
    ggsave(paste0("vertplot", plot_list[h],".pdf"), width=3, height = 3)
  }
  
}

show_gridformat_selecteddays_peaks() <- function() {
  # this function takes input data from function show_gridformat_selecteddays_consistencyscore()
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/iitb_report/figures/gridplots/")
  # NOTE: get mixhomes variable from function show_gridformat_selecteddays_consistencyscore()
  
  plots <- list()
  for(i in 1:(length(mixhomes))) {
    daydat <- split.xts(mixhomes[[i]],f="days",k=1)
    datmat <- sapply(daydat,function(x) coredata(x))
    profile <- xts(apply(datmat,1,mean,na.rm=TRUE),index(daydat[[1]]))
    
    plots[[i]] <- compute_and_showPeaks(profile,w=1,span=0.1)
  }
  l <- marrangeGrob(plots,ncol=3,nrow = 3)
  fname <- "fewhomes_peaks"
  #ggsave(paste0(fname,".pdf"),l,width=14,height = 12)
}

compute_relevancescore_outputggplotonly_withIQR  <- function(dat) {
  # using INTER QUARTILE CONCEPTS
  dat <- mixhomes[[5]]
  daydat <- split.xts(dat,f="days",k=1)
  daymat <- sapply(daydat,function(x) coredata(x))
  colnames(daymat) <- paste0('D',1:dim(daymat)[2])
  flag <- apply(daymat,2,function(x) any(is.na(x)))
  daymat <- daymat[,!flag]
  daymat_xts <- xts(daymat, index(daydat[[1]]))
  
  rowMedian <- function(x, na.rm = FALSE)
  {
    apply(x, 1, median, na.rm = na.rm) 
  }
  iqr <- apply(daymat_xts,1,IQR,na.rm=TRUE)
  first_iqr <- apply(daymat_xts,1, quantile,1/4,na.rm=TRUE)
  third_iqr <- apply(daymat_xts,1,quantile,3/4,na.rm=TRUE)
  stat <- as.data.frame(cbind(iqr,first_iqr,third_iqr))
  stat$lower <-  stat$first_iqr - 1.5 * stat$iqr
  stat$upperr <- stat$third_iqr + 1.5 * stat$iqr
  #stat <- xts(data.frame(rowmean = rowMeans(daymat_xts,na.rm = TRUE)),index(daydat[[1]]))
  # stat dataframe with mean and standard devation
  #stat <- xts(data.frame(rowmean = rowMeans(daymat_xts,na.rm = TRUE)),index(daydat[[1]]))
  # stat <- xts(data.frame(rowmean = rowMedian(daymat_xts,na.rm = TRUE)),index(daydat[[1]]))
  #stat <- cbind(stat,xts(data.frame(rowsd=apply(as.matrix(coredata(daymat_xts)),1,sd,na.rm=TRUE)),index(daydat[[1]])))
  
  status <- vector()
  for( i in 1:dim(daymat_xts)[2]) {
    status[i] <- all((daymat_xts[,i] <= (stat$upper)) & ( daymat_xts[,i] >= (stat$lower) ))
  } 
  # percentage of times this apartment has been consistent in the past
  score <- sum(status,na.rm = TRUE)/length(status)
  
  df_melt <- reshape2::melt(data.frame(timestamp=index(daymat_xts),coredata(daymat_xts)),id.vars="timestamp")
  df_melt$value <- df_melt$value/1000
  g <- ggplot(df_melt,aes(timestamp,value, group=variable)) + geom_line(show.legend = FALSE) + stat_summary(data = df_melt,aes(timestamp,value),fun.y = median,fun.ymin = function(x)
  { s = median(x)-2*sd(x)
  if(s < 0)
  { s = 0}
  return(s)
  },fun.ymax = function(x) median(x) + 2*sd(x), geom="line", inherit.aes = FALSE, color="red",size=1) 
  g <- g + theme_grey() + labs(x = "Hour of Day", y = " Power (kW)") + scale_x_datetime(breaks = date_breaks("3 hour"),date_labels = "%H",timezone = "Asia/Kolkata") + theme(axis.text = element_text(color="black"),plot.title = element_text(hjust=0.5)) 
  #  g <- g + annotate("text", x = df_melt$timestamp[6] , y = max(df_melt$value), label = paste0("Consistency Score = ",round(score,1),"%"),fontface=2) 
  g <- g +ggtitle(paste0("Score = ",round(score,1)))
  g
  # SECOND VERSION
  # g <- ggplot(df_melt,aes(timestamp,value, group=variable)) + geom_line() +geom_line(data=fortify(stat),aes(Index,rowmean),size=2,col="red",inherit.aes = FALSE)
  #  g
  
  return(g)
  
  
}

normality_test <- function() {
  dat <- dat_flat['2016-06-02/2016-06-12 23:59:59']
  for (i in 0:23) {
    dy <- dat[lubridate::hour(dat) == i]
    qqnorm(coredata(scale(dy)))
    print(shapiro.test(coredata(dy)))
  }
}

AravaliSurvey_functions <- function() {
  # all functions related to CHI extended abstract paper
compute_bill_fromdata_AravaliSurvey <- function() {
  
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/onlyservyed_flats.csv"
  df <- fread(energydata)
  dfs <-  xts(df[,2:dim(df)[2]],fastPOSIXct(df$timestamp)-19800)
  
  bill_array <- vector()
  for( h in 1:dim(dfs)[2])
  {
    dat_flat <-  dfs[,h]
    monthwise <- split.xts(dat_flat,f="months",k=1)
    #meanenergy <- sapply(monthwise,mean,na.rm=TRUE)
    sumenergy <- sapply(monthwise,sum,na.rm=TRUE)
    #avg_monthunits <- (meanenergy*24*30)/1000
    sum_monthunits <- sumenergy/1000
    unit <- mean(sum_monthunits[2:length(sum_monthunits)]) # excluding june month
    
    if(unit <= 30)
    { amt <- unit * 0.6
    } else if(unit <= 100){
      amt <- 18 + (unit-30)*0.9
    } else if(unit <= 150){
      amt <- 18 + 63 + (unit-100)*1.1
    } else if(unit <= 200){
      amt <- 18 + 63 + 55 + (unit-150)*1.3
    } else if(unit <= 300){
      amt <- 18 + 63 + 55 + 65 + (unit-200)*1.3
    }else{
      amt <- 18 + 63 + 55 + 65 + 130 + (unit-300)*1.35
    }
    x = (unit) * (0.1283 - 0.4405 + 0.0904)
    y = amt * 5.81
    z =  40
    billamt <- round(x+y+z)
    #pos <- length(bill_array)+1
    bill_array[h] <- billamt
  }
  #bill_frame <- data.frame(flat = colnames(dfs),actual= bill_array)
  names(bill_array) <- colnames(dfs) 
  
  surfile <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/surveydata.csv"
  sfile <- read.csv(surfile)
  sframe <- sfile[,c(1,3,4,5,8,10)]
  sframe$totalpop <- sframe$zeroto20+ sframe$twentyto40+sframe$fortyto60+sframe$above60
  sframe$flatId <- paste0('F',actual_bill=sframe$flatId)
  
  labnames <- c("lessthan500","500to1000","1001to1500","1501to2000","Above2000","dontknow")
  sframe$avgBill <- factor(sframe$avgBill,levels=labnames,labels=labnames)
  sframe$reportedBillrange <- as.numeric(sframe$avgBill)
  sframe$actualBill <- bill_array[sframe$flatId]
  
  actual_bill_range <- vector()
  for(k in 1:length(sframe$actualBill))
  { val <- sframe$actualBill[k]
  if(is.na(val)) {
    actual_bill_range[k] <- NA
  } else{
    if(val < 500) {
      actual_bill_range[k] <- 1
    } else if(val < 1000){
      actual_bill_range[k] <- 2
    } else if(val < 1500){
      actual_bill_range[k] <- 3
    } else if(val < 2000){
      actual_bill_range[k] <- 4
    } else {
      actual_bill_range[k] <- 5
    }
  }
  }
  sframe$actualBillrange <- actual_bill_range
  lframe <- sframe[!is.na(sframe$actualBillrange),]
  
}

compute_peakcorrectionpercentage_AravaliSurvey <- function() {
  # in this script we calculate how many consumers were correct in determining their peak consumption time
  energydata <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/onlyservyed_flats.csv"
  df <- fread(energydata)
  dfs1 <-  xts(df[,2:dim(df)[2]],fastPOSIXct(df$timestamp)-19800)
  dfs <- dfs1["2016-07-02/2016-11-23"] 
  peak_hour <- vector()
  for( h in 1:dim(dfs)[2])
  {
    dat_flat <-  dfs[,h]
    daywise <- split.xts(dat_flat,f="days",k=1)
    daymatrix <- sapply(daywise,function(x) coredata(x))
    nacol <- apply(daymatrix,2,function(x) all(is.na(x)))
    clean_daymat <- daymatrix[,!nacol]
    mean_consump <- rowMeans(clean_daymat,na.rm = TRUE)
    peak_hour[h] <- which(mean_consump == max(mean_consump))
  }  
  names(peak_hour) <- colnames(dfs)
  
  surfile <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/surveydata.csv"
  sfile <- read.csv(surfile)
  sframe <- sfile[,c(8,6)]
  sframe$flatId <- paste0('F',sframe$flatId)
  
  labs<- c("6amto9pm","9amto12pm","12pmto6pm","6pmto10pm","10pmto6am","none")
  sframe$higEngTime <- factor(sframe$higEngTime,levels=labs,labels=labs)
  sframe$reportedtimeRange <- as.numeric(sframe$higEngTime)
  
  sframe$peakhour <- peak_hour[sframe$flatId]
  
  actual_peak_range <- vector()
  for(k in 1:length(sframe$peakhour))
  { val <- sframe$peakhour[k]
  if(is.na(val)) {
    actual_peak_range[k] <- NA
  } else{
    if(val > 6 & val <= 9) {
      actual_peak_range[k] <- 1
    } else if(val > 9 & val <= 12){
      actual_peak_range[k] <- 2
    } else if(val > 12 & val <= 18){
      actual_peak_range[k] <- 3
    } else if(val > 18 & val <= 20){
      actual_peak_range[k] <- 4
    } else if(val > 20 | val <= 6){
      actual_peak_range[k] <- 5
    }
  }
  }
  sframe$actualtimeRange <- actual_peak_range
  lframe <- sframe[!is.na(sframe$actualtimeRange),]
  table(lframe$reportedtimeRange==lframe$actualtimeRange)
} 

compute_survey_profiles_AravliSurvey <- function() {
  # this function is used to find profiles shown in chi poster
  readfile <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/hourly/all_Aravalimeters_june_nov2016.csv"
  df <- fread(readfile)
  dfs <-  xts(df[,2:dim(df)[2]],fasttime::fastPOSIXct(df$timestamp)-19800)
  remove_meters <- c("X3","X10","X11") # meters containing bogus data
  dfs <- dfs[,!colnames(dfs)%in%remove_meters]
  net_building_consumption(dfs)
  four_day_aggregate_consumption_plot(dfs)
  #range of dates for which we need to do clustering
  #subset1 <- dfs["2016-09-19 00:00:00/2016-09-22 23:59:59"]
  subset1 <- dfs["2016-09-17 00:00:00/2016-09-18 23:59:59"]
  
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
  #view_data(returnob)
  set.seed(123)
  clusob <- kmeans(t(returnob),6)# forced 6 clusters
  labels <- as.factor(clusob$cluster)
  # Approach 0 create Horizontal plots
  
  melob<- reshape2::melt(fortify(returnob),id.vars=c("Index"))
  melob$class <- labels[melob$variable] 
  g <- ggplot(melob,aes(Index,value/1000,group=variable)) + facet_wrap(~class,nrow=1)  +
    stat_summary(data =melob,aes(Index,value/1000),fun.y = mean,geom="line",inherit.aes = FALSE,size=1.2) 
  g <- g + scale_x_datetime(breaks = scales::date_breaks("4 hour"), labels = date_format("%H",tz="Asia/Kolkata")) # use scales package
  g <- g + theme(axis.text = element_text(color="black")) 
  g <- g + labs(y ="Power (kW)",x = "Hour of day")
  g
  # ggsave("weekdays.pdf",width=22,height=5,units=c("cm"),path="/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/buildsys_2017_surve/figures/")  
  # ggsave("weekend.pdf",width=22,height=5,units=c("cm"),path="/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/buildsys_2017_surve/figures/")  
  
  
  
  # APPROACH NO 1 RESULTS IN FACETS
  melob<- reshape2::melt(fortify(returnob),id.vars=c("Index"))
  melob$class <- labels[melob$variable] 
  g <- ggplot(melob,aes(Index,value/1000,group=variable)) + facet_wrap(~class,nrow=2)  +
    stat_summary(data =melob,aes(Index,value/1000),fun.y = mean,geom="line",inherit.aes = FALSE,size=1.2) 
  g <- g + scale_x_datetime(breaks = scales::date_breaks("4 hour"), labels = date_format("%H",tz="Asia/Kolkata")) # use scales package
  g <- g + theme(axis.text = element_text(color="black")) 
  g <- g + labs(y ="Power (kW)",x = "Hour of day")
  g
  # 
  
  # ggsave("weekdays.pdf",width=1.8,height=5.8,path="/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/chiposter/figures/")
  # ggsave("weekend.pdf",width=1.8,height=5.8,path="/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/chiposter/figures/")
  # ggsave("weekend.pdf",width=4,height=2.0,path="/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/questionaire/")
  
  ## APROACH 2 RESULTS IN INDIVIDUAL PLOTS
  for(i in 1:length(unique(labels))) {
    cols <- labels[labels==i]
    dat <- returnob[,names(cols)]
    dat_df <- data.frame(timestamp = index(dat),Power=rowMeans(dat,na.rm = TRUE))
    g <- ggplot(dat_df,aes(x=timestamp,y=Power)) + geom_line(size=1.2)
    g <- g + labs(y ="power (W)",x = "Hour of day") + coord_cartesian(ylim = c(min(returnob), max(returnob))) 
    g <- g + scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%H",tz="Asia/Kolkata")) # use scales package
    g <- g + theme(axis.text = element_text(color="black")) 
    g
    #ggsave(paste("weekend",i,".pdf"),length = 4, width = 4)
  }
}
}

plot_motivationplots_DRpaper <- function(){
  #this function takes input data (mixhomes) from function show_gridformat_selecteddays_consistencyscore ()
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/Writings/iitb_report/predictibility/figures")
  selplots <- c(8,16,11,15)
  for( i in 1:length(selplots)) {
    p <- individualplots(mixhomes[[selplots[i]]]) # saved inside same function
    ggsave(paste0("plot",selplots[i],".pdf"),height = 3,width = 3)
  }
  
  individualplots <- function(dat) {
    # function called by plot_motivationplots_DRpaper
    #this function takes multiday data and outputs plots with multiple lines for multi day data
    daydat <- split.xts(dat,f="days",k=1)
    daymat <- sapply(daydat,function(x) coredata(x))
    colnames(daymat) <- paste0('D',1:dim(daymat)[2])
    flag <- apply(daymat,2,function(x) any(is.na(x)))
    daymat <- daymat[,!flag]
    daymat_xts <- xts(daymat, index(daydat[[1]]))
    df_melt <- reshape2::melt(data.frame(timestamp=index(daymat_xts),coredata(daymat_xts)),id.vars="timestamp")
    df_melt$value <- df_melt$value/1000
    g <- ggplot(df_melt,aes(timestamp,value, group=variable)) + geom_line(show.legend = FALSE)
    
    g <- g + theme_grey() + labs(x = "Hour of Day", y = " Power (kW)") + scale_x_datetime(breaks = date_breaks("3 hour"),date_labels = "%H",timezone = "Asia/Kolkata") + theme(axis.text = element_text(color="black"),plot.title = element_text(hjust=0.5)) 
    return(g)
  }
  
}

siddharth_app_data <- function() {
  # code used to plot siddharths application shots
  setwd("/Volumes/MacintoshHD2/Users/haroonr/Dropbox/siddharth/")
  df <- data.frame(Baseline=rnorm(24,820,3.2),You=rnorm(24,825,3.5),id=c(1:24))
  df_melt <- melt(df,id.vars="id")
  g <- ggplot(df_melt,aes(id,value,color=variable)) + geom_line() + theme_grey(base_size = 24)
  g <- g + labs(x="Day Hour",y = "Power (W)") + theme(legend.title= element_blank(),legend.position=c(0.9,0.9),axis.text=element_text(color="black"))
  g
  ggsave("mycon1_font24.png",width=8,height=6)
  #ggsave("mycon2.png",width=4,height=4)
  #ggsave("mycon3.png",width=4,height=3)
}