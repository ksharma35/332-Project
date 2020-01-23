multiplier <- function(cleanregression, lat, long, user_data){
## prediction for pollution level in industrial zones
## pollution level of an industrisl area includes: light, medium, heavy
## They denote level 0-2. S = {0, 1, 2}
  y_fit_residential <- cleanregression[, 3]
  y_fit_industrial <- cleanregression[,5]
  y_fit_rural <- cleanregression[,2]
  y_fit_urban <- cleanregression[,4]
  
  industry_func <- function(level, pm_pred){
  if (level == 0)
    pm_pred <- pm_pred #when the industrial level is zero
  if (level == 1) 
    pm_pred <- pm_pred * 1.10 # when the industrial level is medium
  if (level == 2)
    pm_pred <- pm_pred * 1.20 # when the industrial level is high
  return(pm_pred)
}
 
# calculate the shifting factor for the global pm levels
pm_func <- function(lat, long, pm_pred){
  if (lat >= 30 && lat <= 70 && long >= -168 && long < -50) # USA AND CANADA
    scale_pm<- runif(1, min=0, max=20)
  
  if (lat >= 36 && lat <= 71.12 && long >= -10 && long < 40) # EUROPE
    scale_pm<- runif(1, min=10, max=55)
  
  if (lat >= -56 && lat < 30 && long >= -117.1 && long < -35) # SA & MEXICO 
    scale_pm<- runif(1, min=10, max=35)
  
  if (lat >= -36 && lat < 36 && long >= -17.30 && long < 50) # AFRICA
    scale_pm<- runif(1, min=25, max=60)      
  
  if (lat >= -10 && lat <= 55 && long >= 50 && long < 180) # ASIA
    scale_pm<- runif(1, min=35, max=200)
  
  if (lat >= 22 && lat <= 26 && long >= 88 && long < 92) # Bangladash
    scale_pm<- runif(1, min=250, max=550)
  
  if (lat >= 87 && lat <= 89 && long >= 22.3 && long < 23) # Bangladash
    scale_pm<- runif(1, min=250, max=500)
  
  if (lat >= 70 && lat <= 76 && long >= 30 && long < 34) # Bangladash
    scale_pm<- runif(1, min=250, max=500)
  
  if (lat >= -50 && lat < -10 && long >= 110 && long < 180) # AUSTRALIA
    scale_pm<- runif(1, min=0, max=35)
  
  if (lat >= 19.7 && lat < 20.21 && long >= 50.13 && long < 49.9) # KRAKOW
    scale_pm<- mean(pm_pred, na.rm = T)
  
  ans <- pm_pred - (mean(pm_pred, na.rm = T)-scale_pm)
  
  # logic check
  ans[ans<0] <- 0
  ans[which(ans == max(ans, na.rm=T))] <- NA
  return(ans)
}
 

## pm 2.5 population
############################################################################################################
# check function
checkzeros <- function(onecol){
  onecol[onecol < 0] <- 0
  return(onecol)
}

# new pm values
pm_new <- date
industial_pm <- pm_func(lat, long, y_fit_industrial)
rural_pm <- pm_func(lat, long, y_fit_rural)
residential_pm <- pm_func(lat, long, y_fit_residential)
urban_pm <- pm_func(lat, long, y_fit_urban)

# acount for number of user input sensors
num <- c(1:length(user_data$ï..Location))
user_data$Type <- as.character(user_data$Type)
for(variable in num){
    if (is.na(user_data[variable,2])){
    pm_new[,variable + 1] <- -1
    }
    else if(user_data[variable,2] == "Industrial"){
    pm_new[,variable + 1] <- jitter(industry_func(user_data[variable,3] ,industial_pm),1,0)
    }
  
    else if (user_data[variable,2] == "Rural"){
    pm_new[,variable + 1] <- jitter(rural_pm, 1, 0)
    }
    else if (user_data[variable,2] == "Urban"){
    pm_new[,variable + 1] <- jitter(urban_pm, 1, 0)
    }
    else if (user_data[variable,2] == "Residential"){
    pm_new[,variable + 1] <- jitter(residential_pm, 1, 0)
    }
}
pm_new <- data.frame(sapply(pm_new[,2:(length(user_data$ï..Location)+1)], checkzeros))

pm <- pm_new[8042:8761,]
pm[721:8761,] <- pm_new[1:8041,]
start <- as.POSIXct("2020-1-1 00:00:00", "%Y-%m-%d %H:%M:%OS")
end   <- as.POSIXct("2020-12-31 00:00:00", "%Y-%m-%d %H:%M:%OS")
date <- as.character(seq(start, end, by=3600))
pm <- cbind((date), pm_new)
colnames(pm) <- c("date",paste("pm25",user_data$ï..Location))
#pm[,1]<-as.character(pm[,1])
return(pm)
}