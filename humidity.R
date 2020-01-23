humidityfunction <- function(krakowdata){
## clean function
clean <- function(data){
  data <- data[which(data >= 0 && data <= 100,arr.ind = T)] # set evry humidity within range [0,100]
}
x <- pm_industrial$x # date as numeric

## about humidity in urban zones, load
humidity_urban <- data.frame(krakowdata$X3_humidity, krakowdata$X140_humidity,krakowdata$X147_humidity,krakowdata$X174_humidity,krakowdata$X176_humidity,krakowdata$X177_humidity,krakowdata$X179_humidity,krakowdata$X180_humidity,krakowdata$X181_humidity,krakowdata$X185_humidity,krakowdata$X187_humidity,krakowdata$X189_humidity,krakowdata$X194_humidity,krakowdata$X195_humidity,krakowdata$X196_humidity,krakowdata$X201_humidity,krakowdata$X204_humidity,krakowdata$X205_humidity,krakowdata$X208_humidity,krakowdata$X210_humidity,krakowdata$X211_humidity,krakowdata$X213_humidity,krakowdata$X216_humidity,krakowdata$X219_humidity,krakowdata$X220_humidity,krakowdata$X221_humidity,krakowdata$X226_humidity,krakowdata$X228_humidity,krakowdata$X622_humidity,krakowdata$X808_humidity)
humidity_urban[humidity_urban < 0] <- NA
humidity_urban[humidity_urban >100] <- NA # set boundaries

# take row mean
humidity_urban <- rowMeans(humidity_urban, na.rm = TRUE, dims = 1)
humidity_urban <- as.numeric(humidity_urban)

# fit urban data, predicted
humidity_urban <- as.numeric(humidity_urban)
urban_humidity_fit <- lm(humidity_urban ~ poly(x,4,raw=TRUE))

## about humidity in rural zones
humidity_rural <- data.frame(krakowdata$X209_humidity,krakowdata$X212_humidity,krakowdata$X225_humidity)
humidity_rural <- rowMeans(humidity_rural, na.rm = TRUE, dims = 1)# take row mean
humidity_rural <- as.numeric(humidity_rural)
humidity_rural[humidity_rural < 0] <- NA
humidity_rural[humidity_rural >100] <- NA # boundaries

# fitted rural level  
humidity_rural <- as.numeric(humidity_rural)
rural_humidity_fit <- lm(humidity_rural ~ poly(x,4,raw=TRUE))


## about humidity in industrial zones
humidity_industrial <- data.frame(krakowdata$X169_humidity)
humidity_industrial <- rowMeans(humidity_industrial, na.rm = TRUE, dims = 1) # take row mean
humidity_industrial <- as.numeric(humidity_industrial)
humidity_industrial[humidity_industrial < 0] <- NA
humidity_industrial[humidity_industrial >100] <- NA #boundaries

# fit 
humidity_industrial <- as.numeric(humidity_industrial)
industrial_humidity_fit <- lm(humidity_industrial ~ poly(x,4,raw=TRUE))

## residential area, predicted
humidity_residential <- data.frame(krakowdata$X142_humidity,krakowdata$X170_humidity,krakowdata$X171_humidity,krakowdata$X172_humidity,krakowdata$X173_humidity,krakowdata$X178_humidity,krakowdata$X182_humidity,krakowdata$X183_humidity,krakowdata$X184_humidity,krakowdata$X192_humidity,krakowdata$X202_humidity,krakowdata$X203_humidity,krakowdata$X214_humidity,krakowdata$X215_humidity,krakowdata$X218_humidity,krakowdata$X222_humidity,krakowdata$X223_humidity,krakowdata$X227_humidity,krakowdata$X263_humidity,krakowdata$X713_humidity,krakowdata$X857_humidity,krakowdata$X895_humidity)
humidity_residential <- rowMeans(humidity_residential, na.rm = TRUE, dims = 1)
# humidity_residential <- as.numeric(humidity_residential)
humidity_residential[humidity_residential < 0] <- NA
humidity_residential[humidity_residential >100] <- NA

# fit 
humidity_residential <- as.numeric(humidity_residential) 
residential_humidity_fit <- lm(humidity_residential ~ poly(x,4,raw=TRUE))

##output, x sequence of a year
x <- seq(1, 366, by=1/24)
cleanhumidity <- data.frame(date, predict(residential_humidity_fit, data.frame(x)), predict(rural_humidity_fit, data.frame(x)),predict(urban_humidity_fit, data.frame(x)),a <- predict(industrial_humidity_fit, data.frame(x)))
colnames(cleanhumidity) <- c("date","residential humidity", "rural humidity","urban humidity", "industrial humidity")
return(cleanhumidity)
}

#############################################################################################################
# add noise to humidity data
humiditynoise <- function(humidity){
  humidity[2:length(humidity)] <- sapply(humidity[2:length(humidity)], corruptfunction) # set loop to add noise to every predictors
  return(humidity)
}
#############################################################################################################
## data simulation
corruptfunction <- function(newfit){
  corrupt <- rbinom(length(newfit),1,0.1)    # choose an average of 10% to corrupt at random
  corrupt <- as.logical(corrupt) # set as T/F
  noise <- rnorm(sum(corrupt),0,5) # generate the noise to add
  newfit[corrupt] <- newfit[corrupt] + noise      # about 10% of x has been corrupted
  return(newfit)
}

#############################################################################################
## humidity type includes: dry, moderate1, moderate2, moderate3, tropical
## They denote level 0-4. S = {0, 1, 2, 3, 4}
humidity_func <- function(humidity_level, hum_pred){
  if(humidity_level == 0)
    scale_hum<- runif(1, min=0, max=30)  
  if(humidity_level == 1)
    scale_hum<- runif(1, min=10, max=30)
  if(humidity_level == 2)
    scale_hum<- runif(1, min=30, max=70)
  if(humidity_level == 3)
    scale_hum<- runif(1, min=70, max=100)
  if(humidity_level == 4)
    scale_hum<- runif(1, min=60, max=100)
  
  ans <- hum_pred - (mean(hum_pred, na.rm = T)-scale_hum)
  ans[ans<0] <- 0
  ans[ans > 100] <- 100
  jitter(ans,1,0)
  return(ans)
}
############################################################################################
# humidity population
# check function
humidity_for_all <- function(humidity, humidity_level, userinput){
checkhum <- function(onecol){
  onecol[onecol < 0] <- 0
  onecol[onecol > 100] <- 100
  return(onecol)
}


x <- seq(1,366,1/24)
hum_industrial <- humidity$`industrial humidity`

hum_urban <- humidity$`urban humidity`

hum_rural <- humidity$`rural humidity`

hum_residential <- humidity$`residential humidity`

# Mumbai
humidity_Mumbai <- date
num <- c(1:length(userinput$Type))
for (variable in num) {
  if (is.na(userinput[variable,2]))
    humidity_Mumbai[,(variable + 1)] <- -1 # check user input
  
  else if (userinput[variable,2] == "Industrial")
    humidity_Mumbai[,variable + 1] <- jitter(humidity_func(humidity_level,hum_industrial),1,0)
  
  else if (userinput[variable,2] == "Residential")
    humidity_Mumbai[,variable + 1] <- jitter(humidity_func(humidity_level,hum_residential),1,0)
  
  else if (userinput[variable,2] == "Urban")
    humidity_Mumbai[,variable + 1] <- jitter(humidity_func(humidity_level,hum_urban),1,0)
  
  else if (userinput[variable,2] == "Rural")
    humidity_Mumbai[,variable + 1] <- jitter(humidity_func(humidity_level,hum_rural),1,0)
  
  newfit <- humidity_Mumbai[,variable + 1]
  corrupt <- rbinom(length(newfit),1,0.25)    # choose an average of 25% to corrupt at random
  corrupt <- as.logical(corrupt)
  noise <- rnorm(sum(corrupt),0,5) # generate the noise to add
  newfit[corrupt] <- newfit[corrupt] + noise      # about 25% of x has been corrupted
  humidity_Mumbai[,variable + 1] <- newfit
}
return(humidity_Mumbai)
}
