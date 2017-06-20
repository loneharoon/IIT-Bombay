add_columnname <- function() {
  # this function adds column names to each day file and stores these new files in separte folders identified by CSV extension
  # NOT REQUIRED FOR MONODB DATA
  parent_dir = "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Aravali_iitb/month_9/"
  days = list.dirs(parent_dir,recursive = FALSE,full.names = FALSE)
  for (i in 1:length(days)){
    day_path =  paste0(parent_dir,days[i])
    dir.create(paste0(day_path,"_csv"))
    files = list.files(day_path,pattern=".gz")
    for (j in 1:length(files)){
      data = read.csv(gzfile(paste0(day_path,"/",files[j])), header = FALSE)
      if(length(labels)!=dim(data)[2])
        stop("Data columns are not of same width!")
      colnames(data) = labels
      meterno = strsplit(files[j],"[_]")[[1]][1]
      write.csv(data,file = gzfile(paste0(day_path,"_csv/",meterno,".csv.gz")),row.names = FALSE )
    }
  } }

extract_nams_fromjson <- function() {
  # this function extracts the column name from json file created by ROHIT
  library(jsonlite)
  colnames <- fromJSON(txt="/Volumes/MacintoshHD2/Users/haroonr/Desktop/kresit_header.json", flatten=TRUE)
  paste("'",colnames[[1]]$name,"'",sep="",collapse = ",")
  labels = c('Srl','TS1','TS2','V1','V2','V3','A1','A2','A3','W1','W2','W3','VA1','VA2','VA3','VAR1','VAR2','VAR3','PF1','PF2','PF3','Ang1','Ang2','PF3r','Ang1r','Ang2r','Ang3','u1','u2','u3','u4','u5','u6','u7','u8','u9','u10','u12','u13','u14','u15','F','FwdWh')
}
labels_kresit <- c('srl','TS1','SQNo','TS2','VA','W','VAR','PF','VLL','VLN','A','F','VA1','W1','VAR1','PF1','V12','V1','A1','VA2','W2','VAR2','PF2','V23','V2','A2','VA3','W3','VAR3','PF3','V31','V3','A3','FwdVAh','FwdWh','FwdVARhR','FwdVARhC')

combine_daydata <- function(){
  # this function is used to combine named day data into a single aggregate file
  parent_dir = "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Aravali_iitb/month_9/"
  csvfiles = list.files(parent_dir,pattern="_csv")
  no_of_files = sapply(csvfiles,function(x) length(list.files(paste0(parent_dir,x,"/"))))
  if(!length(unique(no_of_files))==1)
    stop("Few meters are missing on some days!")
  subdir_files = list.files(paste0(parent_dir,"/",csvfiles[1],"/"))
  for (i in 1:length(subdir_files)){
    filename <- subdir_files[i]
    daydata <- lapply(csvfiles, function(x){
      data = read.csv(gzfile(paste0(parent_dir,x,"/",filename)), header = TRUE)
    })
    agg_data <- do.call(rbind,daydata)
    write.csv(agg_data,file = gzfile(paste0(parent_dir,"aggregate/",filename)),row.names = FALSE )
  }
}

understand_data_redundancy <- function(){
  parent_dir = "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Aravali_iitb/"
  file = "month_9/aggregate_12-30sept16/1_2016-09csv.gz"
   df1 <- read.csv(paste0(parent_dir,file),header = TRUE)
   df1$TS2 <- as.POSIXct(df1$TS2,tz="Asia/Kolkata",origin = "1970-01-01")
   df1[,1:6,with=FALSE]
   dim(df1)
}

understand_specificdata_portion <- function(){
  parent_dir = "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Aravali_iitb/"
  file = "month_9/aggregate_1_9sept16/247.csv.gz"
  #file = "month_9/aggregate_12-30sept16/247.csv.gz"
  df1 <- read.csv(paste0(parent_dir,file),header = TRUE)
  df1$TS2 <- as.POSIXct(df1$TS2,tz="Asia/Kolkata",origin = "1970-01-01")
  df1[,1:6,with=FALSE]
  dim(df1)
}


correct_day_13 <- function () {
  library(R.utils)
  parent_dir = "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Aravali_iitb/13/"
  files = list.files(paste0(parent_dir,"rishp1"))
  for (i in 13:length(files)){
    fname <- files[i]
    file1 = paste0(parent_dir,"rishp1/",fname)
    file2 = paste0(parent_dir,"rishp2/",fname)
    linecount  <- countLines(file1)
    df1_p1 <- fread(file1, nrows = linecount - 11)
    df1_p2 <- fread(file1, skip = linecount - 11)
    df2 <- fread(file2)
    temp_df2 <-rbind(df1_p2,df2)
    temp_df1 <- data.frame(df1_p1[,V1:V2,with=FALSE],0,df1_p1[,V3:V43,with=FALSE])
    colnames(temp_df1) <- paste0("V",c(1:44))
    temp <- rbind(temp_df1,temp_df2)
    write.csv(temp,file = paste0(parent_dir,"temp/",fname),row.names = FALSE)
  }
  
  parent_dir = "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Aravali_iitb/13/"
  files = list.files(paste0(parent_dir,"temp/"),pattern = ".csv")
  for (j in 2:length(files)){
    fname = files[j]
    df = fread(paste0(parent_dir,"temp/",fname))
    #df2 <- data.frame(df[,V1:V2,with=FALSE],0,df[,V3:V43,with=FALSE])
    write.table(df,file = paste0(parent_dir,"temp2/",fname),row.names = FALSE,col.names = FALSE,sep = ",")
  }
  parent_dir = "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Aravali_iitb/"
}

clean_days12_30 <- function(){
parent_dir = "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Aravali_iitb/"
files <- list.files(paste0(parent_dir,"12/"),pattern = "*.csv")
folder_names <- c(12:17,19:21,26:30)
for (x in 57:length(files))
  {
  fname_parts <- unlist(strsplit(files[x],"[-]"))
  fname <- paste0( fname_parts[1],"-",fname_parts[2])
  for (i in 1:length(folder_names)){
    df <- fread(paste0(parent_dir,folder_names[i],"/",fname,"-",folder_names[i],".csv"))
    if(i == 1){
      f <- df 
    } else{
      f <- rbind(f,df)
    }
    #browser()
  }
  labels_aravali = c('Srl','TS1','SQNo','TS2','V1','V2','V3','A1','A2','A3','W1','W2','W3','VA1','VA2','VA3','VAR1','VAR2','VAR3','PF1','PF2','PF3','Ang1','Ang2','PF3r','Ang1r','Ang2r','Ang3','u1','u2','u3','u4','u5','u6','u7','u8','u9','u10','u12','u13','u14','u15','F','FwdWh')
  colnames(f) <- labels
  #browser()
  write.csv(f,file = gzfile(paste0(parent_dir,"month_9/aggregate_12-30sept16/",unlist(strsplit(fname_parts[1],"[_]"))[1],".csv.gz")),row.names = FALSE)
cat(paste0("Done with :",x))
  }}

create_power-timestamp <- function() {
parent1 = "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Aravali_iitb/month_9/aggregate_1_9sept16/"
parent2 = "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Aravali_iitb/month_9/aggregate_12-30sept16/"
write_path <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Aravali_iitb/month_9/power/"
meters <- list.files(parent1)
for(i in 42:length(meters)) {
# file no.  41 have some problem
  meter_no = strsplit(meters[i],"[.]")[[1]][1]
  meter_name1 = paste0(parent1,meter_no,".csv.gz")
  meter_name2 = paste0(parent2,meter_no,".csv.gz")
  df1 = read.csv(gzfile(meter_name1),header = TRUE)
  df1 = data.frame(timestamp = as.POSIXct(df1$TS2,tz="Asia/Kolkata",origin = "1970-01-01"),power = df1$VA1+df1$VA2+df1$VA3)
  df2 = read.csv(gzfile(meter_name2),header = TRUE)
  df2 = data.frame(timestamp = as.POSIXct(df2$TS2,tz="Asia/Kolkata",origin = "1970-01-01"),power = df2$VA1+df2$VA2+df2$VA3)
  temp <- rbind(df1,df2)
  
  write.csv(temp,file = paste0(write_path, meter_no,".csv"),row.names = FALSE)
  #power_frame  <- xts(df$power,df$timestamp)
}
}