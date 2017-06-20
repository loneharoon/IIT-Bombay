#install.packages("RMySQL")
#this is used to fetch the temperature data from seil server
library(RMySQL)
mydb = dbConnect(MySQL(), user='reader', password='datapool', dbname='datapool', host='10.129.23.161')
dbListTables(mydb)# lists all tables
query = "select timestamp,temperature from temperature where class = 213 and zone = 5 and lane = 2 and timestamp >= 1451586599;"
rs = dbSendQuery(mydb, query)
data = dbFetch(rs,n = -1)
dbClearResult(rs)
dbDisconnect(mydb)
data$timestamp <- as.POSIXct(data$timestamp, tz="Asia/Kolkata",origin = "1970-01-01")
#write.csv(data,file="temperature_class213_zone5_lane2_2016.csv",row.names = FALSE)
