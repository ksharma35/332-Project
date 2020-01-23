# clean pressure function
pressure_function <- function(cleantempreture, altitude, userinput){
  temp <- cleantempreture[2:length(cleantempreture)] # extract tempreture
  temp_kelv <- temp + 273.15 # convert tempreture to Kelvin
  press <- (101.325) * exp((-1 * 9.80665 * .0289644 * altitude) / (8.31432 * temp_kelv)) # calculate pressure based on tempreture and altitude
  pressure <- data.frame(cleantempreture$date,press)
  colnames(pressure) <- c("date", paste("pressure", userinput$ID))
  return(pressure)
}
################################################################################################
pressurenoise <- function(pressure){
  
  ## data simulation, corrupt 10 % of the data randomly
  corruptfunction <- function(newfit){
    corrupt <- rbinom(length(newfit),1,0.1)    # choose an average of 10% to corrupt at random
    corrupt <- as.logical(corrupt)
    noise <- rnorm(sum(corrupt),0,5) # generate the noise to add
    newfit[corrupt] <- newfit[corrupt] + noise      # about 10% of x has been corrupted
    return(jitter(newfit,1,0))
  }
  pressure[2:length(pressure)] <- sapply(pressure[2:length(pressure)], corruptfunction)
  return(pressure)
}