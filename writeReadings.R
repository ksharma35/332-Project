## Sensor Tracking ##

writeReadings <- function(initial_placement,num_hours,user_data,rec_policy,simulated_data){
  
  placement_vec <- seq(0,0,length.out = length(placement))
  fixedLoc <- which(placement == 1)
  if(length(fixedLoc) != 0){
    placement_vec[fixedLoc] <- 1
  }
  
  naLoc <- which(is.na(placement))
  if(length(naLoc) != 0){
    placement_vec[naLoc] <- NA
  }
  mobileLoc <- which(placement == 2)
  
  
  #expansion 
  placement<-placement_vec
  placement<-as.matrix(as.data.frame(placement))
  for(i in 1:num_hours){
    placement<-insertCol(placement,i,placement_vec)
  }
  placement<-t(placement)
  placement<-as.data.frame(placement)
  colnames(placement)<-c(paste("L_",1:nrow(user_data),sep=""))
  row.names(placement)<-c(paste("Hour ",1:num_hours),sep="")
  placement<-placement[-(num_hours+1),]
  
  num_sensors <- sum(length(mobileLoc),length(fixedLoc))
  dbTable <- matrix(0,num_sensors*num_hours, 9)
  sensor_numb <- 0
  
  multitempreture <- temp_multiplier(user_data, cleantempreture, lat=23)
  noisypressure <- pressurenoise(a) # a is clean tempreture
  krakow_humidity <- humidityfunction(krakowdata)
  humidityforall <- humidity_for_all(krakow_humidity, humidity_level=3, userinput=user_data)
  n <- length(user_data$Type)
  sensor_data <- data.frame(simulated_data[,2:(n+1)],multitempreture[,2:(n+1)],noisypressure[,2:(n+1)],humidityforall[,2:(n+1)])
  
  #Initial mobile placement for hour 1
  if(length(mobileLoc)!=0){
    placement[1,mobileLoc] <- 2
    
    #initalizes Database table
    for (i in 1: length(mobileLoc)){
      mobile_sensor <- mobileLoc[i] #finds locations for all mobile sensors
      Date<-as.POSIXct("2020-01-01 00:00:00","%Y-%m-%d %H:%M:%OS") #finds Date
      dbTable[1 + (i-1)*num_hours,1] <- as.Date(Date) #sets date in table
      dbTable[1 + (i-1)*num_hours,2] <- "00:00:00" #sets time 
      dbTable[1 + (i-1)*num_hours,3] <- as.character(user_data$Name[1]) #gets region name
      dbTable[1 + (i-1)*num_hours,4] <- mobile_sensor #gets location id
      dbTable[1 + (i-1)*num_hours,5] <- i #gets sensor ID
      dbTable[1 + (i-1)*num_hours,6] <- runif(1,45,150) #gets random number for pressure value
      dbTable[1 + (i-1)*num_hours,7] <- simulated_data[1,mobileLoc] #gets appropriate value for PM
      dbTable[1 + (i-1)*num_hours,8] <- runif(1,-10,25) #gets random number for temp
      dbTable[1 + (i-1)*num_hours,9] <- runif(1,0,100) #gets random number for pressure
      AQI_info<-AQIcalc(dbTable[1 + (i-1)*num_hours,7]) #calculates AQI 
      dbTable[1 + (i-1)*num_hours,10] <- AQI_info[1] #sets AQI
      dbTable[1 + (i-1)*num_hours,11] <- AQI_info[2] #sets AQI rating
      
      for(z_array_index in 1 :(8760)){
        for(j in 1:23){
        number_direction <- which.max(rec_policy[mobile_sensor,,j]) #finds direction to move in 
        mov_direction<- trans_reward(number_direction) 
        state_c <- paste("s",mobile_sensor, sep = "")
        new_state <- env(state_c,mov_direction,n)
            #every state can be set to 2 because if the sensors end up in the same spot they will always move together from there on out
        placement[j,new_state]<- 2
        }
        Date<-Date+60*60 #moves to next hour
        dbTable[(z_array_index + 1 + (i-1)*num_hours),1] <- as.Date(Date) #gets date
        dbTable[(z_array_index + 1 + (i-1)*num_hours),2] <- times((as.numeric(format(Date, "%H"))+as.numeric(format(Date, "%M"))/60)/24) #gets time
        dbTable[(z_array_index + 1 + (i-1)*num_hours),3] <- as.character(user_data$Name[1]) #writes name
        dbTable[(z_array_index + 1 + (i-1)*num_hours),4] <- new_state #get location id
        dbTable[(z_array_index + 1 + (i-1)*num_hours),5] <- i #sensor ID
        dbTable[(z_array_index + 1 + (i-1)*num_hours),6] <- runif(1,45,150)
        dbTable[(z_array_index + 1 + (i-1)*num_hours),7] <- simulated_data[z_array_index,mobileLoc] #gets appropriate PM value
        dbTable[(z_array_index + 1 + (i-1)*num_hours),8] <- runif(1,-10,25) 
        dbTable[(z_array_index + 1 + (i-1)*num_hours),9] <- runif(1,0,100)
        AQI_info<-AQIcalc(dbTable[(z_array_index + 1 + (i-1)*num_hours),7])
        dbTable[(z_array_index + 1 + (i-1)*num_hours),10] <- AQI_info[1]
        dbTable[(z_array_index + 1 + (i-1)*num_hours),11] <- AQI_info[2]
        
      }
      sensor_numb<-i
    }
    
  }
  for(i in 1:length(fixedLoc)){
    fixed_sensor <- fixedLoc[i]
    ##########Same functionality as lines 47-60##########################
    Date<-as.POSIXct("2020-01-01 00:00:00","%Y-%m-%d %H:%M:%OS")
    dbTable[1 + (sensor_numb+i-1)*num_hours,1] <- as.Date(Date)
    dbTable[1 + (sensor_numb+i-1)*num_hours,2] <- "00:00:00"  
    dbTable[1 + (sensor_numb+i-1)*num_hours,3] <- user_data$Name[1]
    dbTable[1 + (sensor_numb+i-1)*num_hours,4] <- fixed_sensor
    dbTable[1 + (sensor_numb+i-1)*num_hours,5] <- i + length(mobileLoc)
    dbTable[1 + (sensor_numb+i-1)*num_hours,6] <- mobile_t[i,3]
    dbTable[1 + (sensor_numb+i-1)*num_hours,7] <- mobile_t[i,1]
    dbTable[1 + (sensor_numb+i-1)*num_hours,8] <- mobile_t[i,2]
    dbTable[1 + (sensor_numb+i-1)*4,9] <- mobile_t[i,4]
    AQI_info<-AQIcalc(dbTable[1 + (sensor_numb+i-1)*num_hours,7])
    dbTable[1 + (sensor_numb+i-1)*num_hours,10] <- AQI_info[1]
    dbTable[1 + (sensor_numb+i-1)*num_hours,11] <- AQI_info[2]
    
    for(z_array_index in 1:(8760)){
      ##################################Same functionality as lines 69-81#############################
      Date<-Date+60*60 #moves to next hour
      dbTable[(z_array_index + 1 + (sensor_numb+i-1)*num_hours),1] <- as.Date(Date)
      dbTable[(z_array_index + 1 + (sensor_numb+i-1)*num_hours),2] <- times((as.numeric(format(Date, "%H"))+as.numeric(format(Date, "%M"))/60)/24)
      dbTable[(z_array_index + 1 + (sensor_numb+i-1)*num_hours),3] <- as.character(user_data$Name[1])
      dbTable[(z_array_index + 1 + (sensor_numb+i-1)*num_hours),4] <-fixed_sensor
      dbTable[(z_array_index + 1 + (i-1)*num_hours),5] <- i + length(mobileLoc)
      dbTable[(z_array_index + 1 + (i-1)*num_hours),6] <- mobile_t[i,3]
      dbTable[(z_array_index + 1 + (i-1)*num_hours),7] <- mobile_t[i,1]
      dbTable[(z_array_index + 1 + (i-1)*num_hours),8] <- mobile_t[i,2]
      dbTable[(z_array_index + 1 + (i-1)*num_hours),9] <- mobile_t[i,4]
      AQI_info<-AQIcalc(mobile_t[i,1])
      dbTable[(z_array_index + 1 + (i-1)*num_hours),10] <- AQI_info[1]
      dbTable[(z_array_index + 1 + (i-1)*num_hours),11] <- AQI_info[2]
    }
  
  }
  dbTable<-as.data.frame(dbTable)
  colnames(dbTable)<-c("Date", "Time", "Name", "L_ID", "L_ID","L_ID","Pressure","PM","Temperature","Humidity", "AQI","AQI_Rating") #creates column names
  
  #insert Query to DB here 
  mydb <- dbConnect(MySQL(), user='g1109698', password='SEVERELY26',
                    dbname='g1109698', host='mydb.itap.purdue.edu') #open connection to db
  dbWriteTable(mydb, "Regions", dbTable, overwrite=FALSE, row.names=FALSE,append=TRUE) #write data frame to db
  on.exit(dbDisconnect(mydb))
  return(dbTable)
  
}




