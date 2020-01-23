cleantemp <- function(krakowdata)
{
  # Create data sequence as days
  x <- seq(1, 359, by=1/24)
  
  # date sequence
  start <- as.POSIXct("2020-1-1 00:00:00", "%Y-%m-%d %H:%M:%OS")
  end   <- as.POSIXct("2020-12-31 00:00:00", "%Y-%m-%d %H:%M:%OS")
  date <- seq(start, end, by=3600)
  
  ## about temperature in urban zones
  tempreture_urban <- data.frame(krakowdata$X3_temperature, krakowdata$X140_temperature,krakowdata$X147_temperature,krakowdata$X174_temperature,krakowdata$X176_temperature,krakowdata$X177_temperature,krakowdata$X179_temperature,krakowdata$X180_temperature,krakowdata$X181_temperature,krakowdata$X185_temperature,krakowdata$X187_temperature,krakowdata$X189_temperature,krakowdata$X194_temperature,krakowdata$X195_temperature,krakowdata$X196_temperature,krakowdata$X201_temperature,krakowdata$X204_temperature,krakowdata$X205_temperature,krakowdata$X208_temperature,krakowdata$X210_temperature,krakowdata$X211_temperature,krakowdata$X213_temperature,krakowdata$X216_temperature,krakowdata$X219_temperature,krakowdata$X220_temperature,krakowdata$X221_temperature,krakowdata$X226_temperature,krakowdata$X228_temperature,krakowdata$X622_temperature,krakowdata$X808_temperature)
  # take mean of each hour and each day
  tempreture_urban <- rowMeans(tempreture_urban, na.rm = TRUE, dims = 1)
  tempreture_urban <- as.numeric(tempreture_urban)
  
  # fit 
  tempreture_urban <- as.numeric(tempreture_urban)
  urban_temp_fit <- lm(tempreture_urban ~ poly(x,4,raw=TRUE))


  ## about tempreture in rural zones, polynomial regression
  tempreture_rural <- data.frame(krakowdata$X209_temperature,krakowdata$X212_temperature,krakowdata$X225_temperature)
  # take mean of each hour and each day
  tempreture_rural <- rowMeans(tempreture_rural, na.rm = TRUE, dims = 1)
  tempreture_rural <- as.numeric(tempreture_rural)
  
  # fit regression
  tempreture_rural <- as.numeric(tempreture_rural)
  rural_temp_fit <- lm(tempreture_rural ~ poly(x,4,raw=TRUE))


  ## about tempreture in industrial zones

  tempreture_industrial <- data.frame(krakowdata$X169_temperature)
  # take mean of each hour and each day
  tempreture_industrial <- rowMeans(tempreture_industrial, na.rm = TRUE, dims = 1)
  tempreture_industrial <- as.numeric(tempreture_industrial)
  
  # fitted data for industrial zones
  tempreture_industrial <- as.numeric(tempreture_industrial)
  industrial_temp_fit <- lm(tempreture_industrial ~ poly(x,4,raw=TRUE))
  

  ## about tempreture in residential zones
  temperature_residential <- data.frame(krakowdata$X142_temperature,krakowdata$X170_temperature,krakowdata$X171_temperature,krakowdata$X172_temperature,krakowdata$X173_temperature,krakowdata$X178_temperature,krakowdata$X182_temperature,krakowdata$X183_temperature,krakowdata$X184_temperature,krakowdata$X192_temperature,krakowdata$X202_temperature,krakowdata$X203_temperature,krakowdata$X214_temperature,krakowdata$X215_temperature,krakowdata$X218_temperature,krakowdata$X222_temperature,krakowdata$X223_temperature,krakowdata$X227_temperature,krakowdata$X263_temperature,krakowdata$X713_temperature,krakowdata$X857_temperature,krakowdata$X895_temperature)
  # take mean of each hour and each day
  temperature_residential <- rowMeans(temperature_residential, na.rm = TRUE, dims = 1)
  temperature_residential <- as.numeric(temperature_residential)
  
  # fit data for resdential zones
  temperature_residential <- as.numeric(temperature_residential)
  residential_temp_fit <- lm(temperature_residential ~ poly(x,4,raw=TRUE))
  
  ##output section
  x <- seq(1, 366, by=1/24) # redefine x
  # dataframe
  cleantempreture <- data.frame(date, predict(residential_temp_fit, data.frame(x)), predict(rural_temp_fit, data.frame(x)),predict(urban_temp_fit, data.frame(x)),a <- predict(industrial_temp_fit, data.frame(x)))
  # reame columns
  colnames(cleantempreture) <- c("date","residential temp", "rural temp","urban temp", "industrial temp")
  return(cleantempreture)
}

###################################################################################################
# add noise to the funtion  
tempreturenoise <- function(cleantempreture){
    
  ## data simulation with corruption noise function
    corruptfunction <- function(newfit){
    corrupt <- rbinom(length(newfit),1,0.1)    # 10 percent data is randomly corrupted and assigned one deviation away
    corrupt <- as.logical(corrupt) # change to logical vector
    noise <- rnorm(sum(corrupt),0,5) # generate the noise to add
    newfit[corrupt] <- newfit[corrupt] + noise      # about 10% of x are added with noise
    return(newfit)
    }
    
  cleantempreture[2:length(cleantempreture)] <- sapply(cleantempreture[2:length(cleantempreture)], corruptfunction)
  return(cleantempreture)
  }
#########################################################################################################
  
 temp_multiplier <- function(userinput, cleantempreture, lat){
    
  #scale is how much the graph will be shifted up or down
  x <- seq(1,366,1/24)
  temp_industrial <- cleantempreture$`industrial temp`
  
  # 
  temp_urban <- cleantempreture$`urban temp`
  
  temp_rural <- cleantempreture$`rural temp`
  
  temp_residential <- cleantempreture$`residential temp`
  
  tempcalc <- function(pred_temp){pred_temp + (-1/130.645)*(lat ^ 2) + 27 - 7.77}
  
  user_data$Type <- as.character(user_data$Type)
  
  # to populate a randomm city, Mumbai, or example
  temp_Mumbai <- data.frame(date)
  num <- c(1:length(userinput$Type)) # set loop variance
  for (variable in num) {
    if (is.na(userinput[variable,2]))
      temp_Mumbai[,(variable + 1)] <- -1 # check user input
    
    else if (userinput[variable,2] == "Industrial")
      temp_Mumbai[,(variable + 1)] <- tempcalc(temp_industrial) # if the zone is industrial
    
    else if (userinput[variable,2] == "Residential") # if zone is residential
      temp_Mumbai[,(variable + 1)] <- tempcalc(temp_residential)
    
    else if (userinput[variable,2] == "Urban") # if zone is urban
      temp_Mumbai[,(variable + 1)] <- tempcalc(temp_urban)
    
    else if (userinput[variable,2] == "Rural") # if userinput is rural
      temp_Mumbai[,(variable + 1)] <- tempcalc(temp_rural)
  }
  colnames(temp_Mumbai) <- c("date",paste("tempreture", as.character(userinput$ï..Location))) # set column names
  return(temp_Mumbai)
}
  