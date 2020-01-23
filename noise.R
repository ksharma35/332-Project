noise <- function(cleanregression){
  ## data simulation: residential
  x <- seq(1, 366, by=1/24)
  y_fit_residential <- cleanregression[, 3]
  y_fit_industrial <- cleanregression[,5]
  y_fit_rural <- cleanregression[,2]
  y_fit_urban <- cleanregression[,4]
  
  
  corrupt <- rbinom(length(y_fit_residential),1,0.25)    # choose an average of 10% to corrupt at random
  corrupt <- as.logical(corrupt)
  noise <- rnorm(sum(corrupt),0,5) # generate the noise to add
  y_fit_residential[corrupt] <- y_fit_residential[corrupt] + noise      # about 10% of x has been corrupted
  
  newfit <- y_fit_residential[50:1200]
  corrupt <- rbinom(length(newfit),1,0.1)    # choose an average of 10% to corrupt at random
  corrupt <- as.logical(corrupt)
  noise <- rnorm(sum(corrupt),0,50) # generate the noise to add
  newfit[corrupt] <- newfit[corrupt] + abs(noise)      # about 10% of x has been corrupted
  y_fit_residential[50:1200] <- newfit
  
  newfit <- y_fit_residential[7200:8761]
  corrupt <- rbinom(length(newfit),1,0.1)    # choose an average of 10% to corrupt at random
  corrupt <- as.logical(corrupt)
  noise <- rnorm(sum(corrupt),0,40) # generate the noise to add
  newfit[corrupt] <- newfit[corrupt] + abs(noise)      # about 10% of x has been corrupted
  y_fit_residential[7200:8761] <- newfit
  
  y_fit_residential[y_fit_residential <= 0] <- NA
  
  
  ## data simulation: industrial
  x <- seq(1, 366, by=1/24)
  
  corrupt <- rbinom(length(y_fit_industrial),1,0.25)    # choose an average of 10% to corrupt at random
  corrupt <- as.logical(corrupt)
  noise <- rnorm(sum(corrupt),0,5) # generate the noise to add
  y_fit_industrial[corrupt] <- y_fit_industrial[corrupt] + noise      # about 10% of x has been corrupted
  
  newfit <- y_fit_industrial[50:1200]
  corrupt <- rbinom(length(newfit),1,0.1)    # choose an average of 10% to corrupt at random
  corrupt <- as.logical(corrupt)
  noise <- rnorm(sum(corrupt),0,50) # generate the noise to add
  newfit[corrupt] <- newfit[corrupt] + abs(noise)      # about 10% of x has been corrupted
  y_fit_industrial[50:1200] <- newfit
  
  newfit <- y_fit_industrial[7200:8761]
  corrupt <- rbinom(length(newfit),1,0.1)    # choose an average of 10% to corrupt at random
  corrupt <- as.logical(corrupt)
  noise <- rnorm(sum(corrupt),0,40) # generate the noise to add
  newfit[corrupt] <- newfit[corrupt] + abs(noise)      # about 10% of x has been corrupted
  y_fit_industrial[7200:8761] <- newfit
  
  y_fit_industrial[y_fit_industrial <= 0] <- NA
  
  
  ## data simulation: rural
  x <- seq(1, 366, by=1/24)
  
  corrupt <- rbinom(length(y_fit_rural),1,0.25)    # choose an average of 10% to corrupt at random
  corrupt <- as.logical(corrupt)
  noise <- rnorm(sum(corrupt),0,5) # generate the noise to add
  y_fit_rural[corrupt] <- y_fit_rural[corrupt] + noise      # about 10% of x has been corrupted
  
  newfit <- y_fit_rural[50:1200]
  corrupt <- rbinom(length(newfit),1,0.1)    # choose an average of 10% to corrupt at random
  corrupt <- as.logical(corrupt)
  noise <- rnorm(sum(corrupt),0,50) # generate the noise to add
  newfit[corrupt] <- newfit[corrupt] + abs(noise)      # about 10% of x has been corrupted
  y_fit_rural[50:1200] <- newfit
  
  newfit <- y_fit_rural[7200:8761]
  corrupt <- rbinom(length(newfit),1,0.1)    # choose an average of 10% to corrupt at random
  corrupt <- as.logical(corrupt)
  noise <- rnorm(sum(corrupt),0,40) # generate the noise to add
  newfit[corrupt] <- newfit[corrupt] + abs(noise)      # about 10% of x has been corrupted
  y_fit_rural[7200:8761] <- newfit
  
  y_fit_rural[y_fit_rural <= 0] <- NA
  
  ## data simulation: urban
  x <- seq(1, 366, by=1/24)
  
  corrupt <- rbinom(length(y_fit_urban),1,0.25)    # choose an average of 10% to corrupt at random
  corrupt <- as.logical(corrupt)
  noise <- rnorm(sum(corrupt),0,5) # generate the noise to add
  y_fit_urban[corrupt] <- y_fit_urban[corrupt] + noise      # about 10% of x has been corrupted
  
  newfit <- y_fit_urban[50:1200]
  corrupt <- rbinom(length(newfit),1,0.1)    # choose an average of 10% to corrupt at random
  corrupt <- as.logical(corrupt)
  noise <- rnorm(sum(corrupt),0,50) # generate the noise to add
  newfit[corrupt] <- newfit[corrupt] + abs(noise)      # about 10% of x has been corrupted
  y_fit_urban[50:1200] <- newfit
  
  newfit <- y_fit_urban[7200:8761]
  corrupt <- rbinom(length(newfit),1,0.1)    # choose an average of 10% to corrupt at random
  corrupt <- as.logical(corrupt)
  noise <- rnorm(sum(corrupt),0,40) # generate the noise to add
  newfit[corrupt] <- newfit[corrupt] + abs(noise)      # about 10% of x has been corrupted
  y_fit_urban[7200:8761] <- newfit
  
  y_fit_urban[y_fit_urban <= 0] <- NA
  
  ## output
  start <- as.POSIXct("2020-1-31 00:00:00", "%Y-%m-%d %H:%M:%OS")
  end   <- as.POSIXct("2021-1-30 00:00:00", "%Y-%m-%d %H:%M:%OS")
  date <- data.frame(seq(start, end, by=3600))
  cleanregression <- cbind(date, y_fit_rural, y_fit_residential, y_fit_urban, y_fit_industrial)
  colnames(cleanregression) <- c("date", "rual pm25", "residential pm25", "urban pm25", "industrial pm25")
  return(cleanregression)
}

# create sequential data that follows pm pattern of Krakow
pm_noisefunction <- function(cleanpm){
  x <- seq(1, 366, by=1/24)
  newfit <- cleanpm[50:1200]
  corrupt <- rbinom(length(newfit),1,0.1)    # choose an average of 10% to corrupt at random
  corrupt <- as.logical(corrupt)
  noise <- rnorm(sum(corrupt),0,50) # generate the noise to add
  newfit[corrupt] <- newfit[corrupt] + abs(noise)      # about 10% of x has been corrupted
  y_fit_rural[50:1200] <- newfit
  newfit <- cleanpm[7200:8761]
  corrupt <- rbinom(length(newfit),1,0.1)    # choose an average of 10% to corrupt at random
  corrupt <- as.logical(corrupt)
  noise <- rnorm(sum(corrupt),0,40) # generate the noise to add
  newfit[corrupt] <- newfit[corrupt] + abs(noise)      # about 10% of x has been corrupted
  noisypm[7200:8761] <- newfit
  noisypm[noisypm < 0] <- 0
  return(y_fit)
}
# pmdatawithnoise[,2:length(pmdata)] <- sapply(pmdata[,2:length(pmdata)], pm_noisefunction)