#### Movement #### 
movement<- function(placement, rec_policy, num_hours){
placement_vec <- seq(0,0,length.out = length(placement)) #makes placement vector
fixedLoc <- which(placement == 1) #finds fixed locations
if(length(fixedLoc) != 0){
  placement_vec[fixedLoc] <- 1
}

naLoc <- which(is.na(placement)) #finds na locations
if(length(naLoc) != 0){
  placement_vec[naLoc] <- NA
}
mobileLoc <- which(placement == 2) #finds mobile locations


#expansion to length of number of hours
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

#Initial mobile placement for hour 1

if(length(mobileLoc)!=0){
  placement[1,mobileLoc] <- 2

  
#returns best policy for hr i, state j
  for (z_array_index in 1: (num_hours-1)){
    mobileLoc<- which(placement[z_array_index,] == 2) #finds all mobile sensors
    for(i in 1:length(mobileLoc)){
      number_direction <- which.max(rec_policy[mobileLoc[i],,z_array_index]) #finds best direction
      mov_direction<- trans_reward(number_direction) #moves sensors in right direction
      state_c <- paste("s",mobileLoc[i], sep = "") #makes character array of states
      new_state <- env(state_c,mov_direction,n) #sets new state
     
      #every state can be set to 2 because if the sensors end up in the same spot they will always move together from there on out
      placement[z_array_index+1,new_state]<- 2
    }
  }
}
  return(placement)
}