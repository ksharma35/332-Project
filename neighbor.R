#### Neighbor Function #### 

neighbor <- function(placement, budget){
  x <- runif(1)
  if (x >= 0.35){
   #### Location Shift ####
 
   #num of each sensor type
   fixed <- which(placement == 1)
   mobile <- which(placement == 2)
   
   #calculates number of sensors placed
   snum <- length(placement[fixed]) + length(placement[mobile])
   
   #calcualtes lambda value
   lambda <- ceiling(0.15*(snum))
   if(lambda == 0){
      lambda <- ceiling(0.05*snum)
   
   }
   
   
   #samples from poi dist'n for # of sensors that will be moved 
   num_shift <- 0
   sensors <- which(placement!=0) #may already remove NA points 
   while(num_shift == 0){
   num_shift <- rpois(1,lambda)
   
   }
   
   #samples which sensors to move 
   
   
   smove <- sort(sample(sensors,num_shift, replace = F))

   
   #Sample for which direction to move
   directions <- c("North","South","East","West", "NorthWest", "SouthWest","NorthEast", "SouthEast")
   ndir <- sample(directions,1)
   
   #movement, nloc contains new location of fixed sensors
   #n <- sqrt(length(placement)) #Defined as Global Variable in the Initial Placement Function 
   
   #########start of feasibility checking######
   
   #PROBLEM CHILD#
   
   #Checks for NA points
   probPts<-feasibility_check(smove,placement,ndir,n,"NA")
   #print(probPts)
   if(!is.na(probPts)){
      smove <- smove[-which(smove == probPts)]
   }
   
   if(length(smove) == 0){
      return(placement)
   }
   
   #Checks for points that have not moved
   newplacement <-feasibility_check(smove,placement,ndir,n,"sameLoc")
      return(newplacement)
}

  else {
    #### Sesnsor Swap ####
     
     #calcualtes leftover budget
     budget_l <- budget - (500*length(which(placement == 1)) + 3500*length(which(placement == 2)))
   
     if (budget_l >= 3500){
       empty <- which(placement ==  0)
       nspot <- sample(empty,1)
       placement[nspot] <- 2
     }
     else if(budget_l >= 500){
       empty <- which(placement ==  0)
       nspot <- sample(empty,1)
       placement[nspot] <- 1
     }
     else{
       fixed <- which(placement == 1)
       if(length(which(placement == 1)) < 7){
          return(placement)
       }
       remove_pts <- sort(sample(fixed,7))
       for_swap <- sample(remove_pts,1)
       #replaces fixed for mobile
       placement<-replace(placement, for_swap,2)
       #this makes the contents of remove points the placement index positions of the sensors I want to remove 
       remove_pts <- remove_pts[-which(remove_pts == for_swap)]
       placement<-replace(placement,remove_pts, 0)
        
     }
     
  }
  return(placement)
}