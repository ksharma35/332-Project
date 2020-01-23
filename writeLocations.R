writeLocation <- function(user_data,region_name){
  library(RMySQL)
  mydb <- dbConnect(MySQL(), user='g1109698', password='SEVERELY26',
                    dbname='g1109698', host='mydb.itap.purdue.edu') #open connection to db
  Name<-rep(region_name,nrow(user_data)) #get region name
  L_ID<-user_data$Location #get location ids
  Type<-user_data$Type #get location types
  X_coord<-user_data$X #get x coordinates
  Y_coord<-user_data$Y #get y coordinates
  Longitude <- seq(0,0,length.out = length(X_coord)) #set longitutudes
  Latitude <- seq(0,0,length.out = length(X_coord)) #set latitudes
  center_lat <- user_data$Center.Latitude[1] #set center lat
  center_long <- user_data$Center.Longitude[1] #set center long
  for (r in 1:nrow(user_data)){
    Longitude[r] <- (center_long + 0.0009*X_coord[r]) #update long
    Latitude[r] <- (center_lat +0.0009*Y_coord[r]) #update lat
  }
  Level <-user_data$Level #get industrial levels
  
  db_location<-data.frame(cbind(Name,L_ID,as.character(Type),X_coord,Y_coord,Longitude,Latitude,Level)) #make data frame
  
  colnames(db_location)<-c("Name", "L_ID", "Type","X_coord", "Y_coord", "Longitude", "Latitude", "Level")
  dbWriteTable(mydb, "Locations", db_location, overwrite=FALSE, row.names=FALSE,append=TRUE) #write data frame to db
  on.exit(dbDisconnect(mydb))
  }