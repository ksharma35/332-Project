writeRegion <- function(user_data){
  library(RMySQL)
  mydb <- dbConnect(MySQL(), user='g1109698', password='SEVERELY26',
                    dbname='g1109698', host='mydb.itap.purdue.edu') #open db connection
  db_region <- c(user_data$name[1],user_data$Center.Latitude[1],
                 user_data$Center.Longitude[1],user_data$Altitude[1])
  db_region <-as.data.frame(db_region)
  
  dbWriteTable(mydb, "Region", db_region, overwrite=FALSE, row.names=FALSE)
  on.exit(dbDisconnect(mydb))
}