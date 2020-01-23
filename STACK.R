# data reconstruction
stackfun <- function(city, user_data,placement,type){
  city <- stack(city) ##stack into one pile
  city <- city$values
  city <- jitter(x = city,1,0)
  n <- length(city)/4
  pm <- city[1:n]   #pm
  temp <- city[(1+n):(n * 2)] # tempreture 
  pressure <- city[(1+2*n):(n * 3)] # pressure
  humid <- city[(1+3*n):(4 * n)] #humidity for 8761*number of sensors
  pm[pm < 0] <- 0
  temp[temp > 50] <- 50
  humid[humid < 0] <- 0
  humid[humid > 100] <- 100
  mobile <- data.frame(PM25 = pm, tempreture = temp, pressure = pressure, humidity = humid)[which(placement == 2),]
  fixed_data <- data.frame(PM25 = pm, tempreture = temp, pressure = pressure, humidity = humid)[which(placement == 1),]
  if(type == "mobile"){
    return(mobile)
  }
  else{
    return(fixed_data)
  }
 
}