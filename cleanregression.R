regression <- function(pm_residential, pm_rural,pm_urban, pm_industrial){

library(MASS)
## pm data
x <- seq(1, 366, by=1/24)

## for industrial area
y <- pm_industrial$X169_pm25
y <- y[which(y > 0,arr.ind = T)]
## day 30 is the day with highest pm values 
y1 <- pm_industrial$X169_pm25[720:8593]
y2 <- pm_industrial$X169_pm25[1:719]
y[1:7874] <- y1
y[7875:8043] <- NA
y[8043:8761] <- y2
y[y <= 0] <- NA
# realize the day on actually starts from day 30. which is the end of jan
fit_industrial <- lm(y ~ I(1/x)+poly(x,3,raw=TRUE))
y_fit_industrial <- predict(fit_industrial, data.frame(x))

  

## For Urban area
 
# take the mean of rows
y <- data.frame(pm_urban$X3_pm25,pm_urban$X140_pm25, pm_urban$X147_pm25,pm_urban$X174_pm25,pm_urban$X176_pm25,pm_urban$X177_pm25,pm_urban$X179_pm25,pm_urban$X180_pm25,pm_urban$X181_pm25,pm_urban$X185_pm25,pm_urban$X187_pm25,pm_urban$X189_pm25,pm_urban$X194_pm25,pm_urban$X195_pm25,pm_urban$X196_pm25,pm_urban$X201_pm25,pm_urban$X204_pm25,pm_urban$X205_pm25,pm_urban$X208_pm25,pm_urban$X210_pm25,pm_urban$X211_pm25,pm_urban$X213_pm25,pm_urban$X216_pm25,pm_urban$X219_pm25,pm_urban$X220_pm25,pm_urban$X221_pm25,pm_urban$X226_pm25,pm_urban$X228_pm25,pm_urban$X622_pm25,pm_urban$X205_pm25,pm_urban$X808_pm25)
y[y <= 0] <- NA
y <- rowMeans(y, na.rm = TRUE, dims = 1)
y1 <- y[720:8593]
y2 <- y[1:719]
y[1:7874] <- y1
y[7875:8043] <- NA
y[8043:8761] <- y2

fit_urban <- lm(y ~ I(1/x)+poly(x,3,raw=TRUE))
y_fit_urban <- fit_urban$coefficients[2]/x + fit_urban$coefficients[3]*x+fit_urban$coefficients[4]*x^2+fit_urban$coefficients[5]*x^3 + fit_urban$coefficients[1]

  

## For Residential area
 
# take the mean of rows
y <- data.frame(pm_residential$X142_pm25,pm_residential$X170_pm25,pm_residential$X171_pm25,pm_residential$X172_pm25,pm_residential$X173_pm25,pm_residential$X178_pm25,pm_residential$X182_pm25,pm_residential$X183_pm25,pm_residential$X184_pm25,pm_residential$X192_pm25,pm_residential$X202_pm25,pm_residential$X203_pm25,pm_residential$X214_pm25,pm_residential$X215_pm25,pm_residential$X218_pm25,pm_residential$X222_pm25,pm_residential$X223_pm25,pm_residential$X227_pm25,pm_residential$X263_pm25,pm_residential$X713_pm25,pm_residential$X857_pm25,pm_residential$X895_pm25)
y[y <= 0] <- NA
y <- rowMeans(y, na.rm = TRUE, dims = 1)
y1 <- y[720:8593]
y2 <- y[1:719]
y[1:7874] <- y1
y[7875:8043] <- NA
y[8043:8761] <- y2

fit_residential <- lm(y ~ I(1/x)+poly(x,3,raw=TRUE))
y_fit_residential <- fit_residential$coefficients[2]/x + fit_residential$coefficients[3]*x+fit_residential$coefficients[4]*x^2+fit_residential$coefficients[5]*x^3+fit_residential$coefficients[1]
  

## For Rural area
 
# take the mean of rows
y <- data.frame(pm_rural$X209_pm25,pm_rural$X212_pm25,pm_rural$X225_pm25)
y[y <= 0] <- NA
y <- rowMeans(y, na.rm = FALSE, dims = 1)
y1 <- y[720:8593]
y2 <- y[1:719]
y[1:7874] <- y1
y[7875:8043] <- NA
y[8043:8761] <- y2

fit_rural <- lm(y ~ I(1/x)+poly(x,3,raw=TRUE))
y_fit_rural <- fit_rural$coefficients[2]/x + fit_rural$coefficients[3]*x+fit_rural$coefficients[4]*x^2+fit_rural$coefficients[5]*x^3+fit_rural$coefficients[1]

## output
start <- as.POSIXct("2020-1-31 00:00:00", "%Y-%m-%d %H:%M:%OS")
end   <- as.POSIXct("2021-1-30 00:00:00", "%Y-%m-%d %H:%M:%OS")
date <- data.frame(seq(start, end, by=3600))
cleanregression <- cbind(date, y_fit_rural, y_fit_residential, y_fit_urban, y_fit_industrial)
colnames(cleanregression) <- c("date", "rual pm25", "residential pm25", "urban pm25", "industrial pm25")
cleanregression
}