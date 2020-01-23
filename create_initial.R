#REQUIRES Package "miscTools"

                                   ####Inital Placement Function####

#Returns vector of 0&1 of where sensors are placed, all Fixed 

create_initial <- function(budget, user_data,num_hours){
  
  #creates a 0 vector of length corresponding to all possible placement locations
  placement <- seq(0,0,length.out = nrow(user_data))
  #determines which locations can not have sensors placed in them 
  x <- which(c(is.na(user_data$Type)))
  #assigns NA to the indices where sensors can not be placed 
  placement[x] <- NA
  #max num of Fixed Sensors 
  sfnum <- floor(budget/500)
  #Vector of valid L_id  
  pos <- which(is.na(placement) != 1)
  #checks to see if more fixed sensors can be placed than availible spots in the grid
  if(sfnum > (length(placement) - length(x))){
    placement[-x]<-1
  }
  else{
    #vector of positions where sesnsors will be placed
    spos <- sample(pos,sfnum,replace = F)
    #placement vector updated, fixed sensors placed
    placement[spos] <- 1
  }

  #defining the Global Variable n
  n <<- sqrt(length(placement))
 
  return(placement)
}
