# this file is used to play with the data stored in MONGO DB @  stark machine IITB
library(RMongo)
library(xts)
library(data.table)
library(fasttime) #for fastPosixct
Sys.setenv(TZ="Asia/Kolkata")
stamp = "2016-11-24 00:00:00 IST"
as.integer(as.POSIXct(stamp))
epochs =  1481729866
as.POSIXct(epochs,origin="1970-01-01")

fetch_sequential_data <- function(){
  # function used to fetch data from stark based on the timestamp field
  dbName = "data" 
 # dbName = "smartmeters"
  host = "10.129.23.41" 
  port = "27017"
  handle = mongoDbConnect(dbName,host,port)
  dbShowCollections(handle)
  # metername = "aravali_3"
  ts = 1475260200 # 
  #u6 is a column name which refers to aggrgate wattage (w1+w2+w3)
  dbGetQuery(handle, metername, "{'TS':{'$gt':1475260200}}", skip = 0,limit = 10)
  dbGetQueryForKeys(handle,metername, keys = "{'SumW':1,'TS':1}",query = paste0("{'TS':{'$gte':",ts,"}}"),skip=0,limit =  2)
  # 1420050600 [1 jan 2015]
  # 1451586600 [1 jan 2016]
  # 1475260200 [1 oct 2016]
  # 1483036200 [30 Dec 2016]
  for (j in c(1:16)) {
    
    if(ts >= 1483036200) # upper limit
    { cat("exit loop")
      break
    }
    
    dat = list()
    #metername = "aravali_23"
    #fname = paste0(1,".csv")
    # these are some of the apartment meter no.s
    apartments = c(1,3:6,8,10,11,16:18,21:32,34:37,200,207:209,211:212,214,217:219,221,223:229,231,233:243,245:247)
    appo_paste = paste0("aravali_",apartments)
    for(k in 3:length(apartments)) {
      dat = list()
      metername = appo_paste[k]
      fname = paste0(apartments[k],".csv")
     ts = 1479925800 # 
    for (i in c(1:30)){
      dat[[i]] = dbGetQueryForKeys(handle,metername, keys = "{'SumW':1,'TS':1}",query = paste0("{'TS':{'$gte':",ts,"}}"),skip=0,limit =  86400)
      #dat[[i]] = dbGetQuery(handle, metername, query=paste0("{'TS':{'$gte':",ts,"}}"), skip = 0,limit = 86400)
      ts = tail(dat[[i]],1)$TS
      if(is.null(ts)) break
      if(ts > 1483036200 | is.null(ts))
      { cat("exit loop")
        break
      }
    }
    cat(fname)
    #system("say just finished")
    temp <- do.call(rbind,dat)
    if(dim(temp)[1]>0) {
    df <- data.frame(power=round(temp$SumW,2),timestamp= temp$TS)
    df$timestamp <- as.POSIXct(df$timestamp,tz="Asia/Kolkata",origin="1970-01-01")
    #dir = paste0("/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/",metername,"/")
    #fname = paste0(j,"_2016.csv")
    dir = paste0("/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/aravali_iitb/year2016_data/December/")
    write.csv(df,file=paste0(dir,fname),row.names = FALSE)
    
    cat(paste0("done",k))
    }
    }
  system("say just finished")
  # dbDisconnect(handle)
}}

specific_fetch_functions <- function(){
  # this first query fetches all columns
  query = dbGetQuery(handle, "power_k_m", "{'Timestamp_1':{'$gt':1388514600}}", skip = 0,limit = 10)
  data = query[,c('W','Timestamp')]
  # next query fetches selected columns and does not require the limit on the number of queries
  dat = dbGetDistinct(handle,"power_k_m", key = "{'W':1,'Timestamp':1}", query = "{'Timestamp':{'$gt':1451586600, '$lt': 1454178599}}")
  # next query fetches also selected columns but requires upper limint on the number of records feteched
  dbs = dbGetQueryForKeys(handle,"power_k_m", keys = "{'W':1,'Timestamp':1}",query = "{'Timestamp':{'$gte':1451586600, '$lte': 1454178599}}",skip=0,limit =  86400)
}

clean_MonogDB <- function(){
  # function used to clean(duplicate removal) the data and to combine individual files into single file
  dir = "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/"
  dirxx<- "/Volumes/WDFILES/KReSIT_data/"
  mname <- "meter3" 
  fname <- paste0(dirxx,mname,"/")
  fl <- list.files(fname,pattern = "*_2016.csv")
  dat <- lapply(fl,function(x){
    df = fread(paste0(fname,x),header = TRUE)
    df_xts <- xts(df$power,fastPOSIXct(df$timestamp)-19800)
    return(df_xts)
  })
  temp <- do.call(rbind, dat)
  rowno <- which(duplicated(index(temp)))
  temp2 <- temp[-rowno,]
  # write.csv(data.frame(timestamp=index(temp2),power=coredata(temp2)),file=paste0(fname,"complete_2015-16.csv"),row.names = FALSE)
  df_obj <- data.frame(timestamp=as.character(index(temp2)),power=coredata(temp2))
  fwrite(df_obj,file=paste0(fname,"complete_2016.csv"))
  sampled_xts <- resample_data(temp2,60)
  #plot(sampled_xts)
  write.csv(data.frame(timestamp=index(sampled_xts),power=coredata(sampled_xts)),file=paste0(fname,"hourly_complete_2016.csv"),row.names = FALSE)
#  h5write(df_obj,h5filename,"power_meter3_2016.csv")
}

create_hourly <- function() {
  # function used to create hourly samples from half
  file <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/main_meter_kresit/complete_2014.csv"
  df <- fread(file,header=TRUE)
  df_xts <- xts(df$power,fastPOSIXct(df$timestamp) - 19800)
  sd <- resample_data(df_xts,60)
  
  newtimestamp <- vector()
  for (i in c(1:dim(sd)[1])) {
    if(.indexmin(sd[i,]) %in% c(30)) {
      newtimestamp[i] <-  index(sd[i,]) + 60*60*0.5
    } else{
      newtimestamp[i] <-  index(sd[i,]) + 0
    }
  }
  sd2 <- xts(coredata(sd),as.POSIXct(newtimestamp,origin = "1970-01-01"))
  sd2 <- data.frame( index(sd2), coredata(sd2) )
  
  colnames(sd2) <- c("timestamp","power")
  # write.csv(sd2,file="/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/main_meter_kresit/hourly_complete_2014.csv",row.names = FALSE)
  
  resample_data <- function(df_xts, xminutes){
    
    ds_data <- period.apply(df_xts,INDEX = endpoints(index(df_xts)-3600*0.5, on = "minutes", k = xminutes ), FUN= mean) 
    # align data to nearest time boundary
    align_data <- align.time(ds_data, xminutes*60 - 3600*0.5) # aligning to x minutes
    return(align_data)
  }
}

read_2015_2016 <- function( ){
  dirx = "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/power_k_f2_a/"
  f2015 <- fread(paste0(dirx,"complete_2015.csv"),header = TRUE)
  f2016 <- fread(paste0(dirx,"complete_2016.csv"),header = TRUE)
  #f2015_xts <- xts(f2015$power,fastPOSIXct(f2015$timestamp)-19800)
  #f2016_xts <- xts(f2016$power,fastPOSIXct(f2016$timestamp)-19800)
  
}

write_h5_kresitdata <- function( ) {
  
  library(rhdf5)
  # fdir <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/wiki-energy/"
  #fdir2 <-  "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/main_meter_kresit/"
  f5storepath <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/kresit_iitb/"
  h5filename <- paste0(f5storepath,"kresit_IITB.h5")
  # h5createFile(filename)
  handle <- h5createFile(h5filename)
  #option 1: when we want to write one by one file
  df = fread(paste0(fdir2,fil))
  h5write(df, filename, x)
  #Option 2: when we want to wite in bluk
  files = list.files(fdir2,pattern="*.csv")
  lapply(files,function(x) {
    df = fread(paste0(fdir2,x))
    h5write(df, h5filename, x)
  })
  H5close(fhandle)
  
}
#object,file,name
h5write(df_obj,h5filename,"power_k_p_2016.csv")
h5write(f2016,h5filename,"power_k_ch_p1_2016.csv")

h5ls(hanlde)$name

readhandle = H5Fopen(filename)
#list of files stored in object: h5ls(readhandle)$name
h5read(readhandle,"101.csv")
