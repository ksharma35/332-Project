#feasibility check Function

feasibility_check <- function(smove,placement,ndir,n,type){
   
  #generates new potential locations

  c_smove<-paste("s",smove,sep ="")
  nloc <- as.vector(sapply(c_smove,env, action=ndir, n=n))
  
  probPts<-0
  new_placement<-c()
  #checks and identifies which points are in NA locations
  
  if(type == "NA"){
    naloc <- which(c(is.na(placement)))
    probPts<-intersect(naloc,nloc)
    if(length(probPts) == 0)
    {
      probPts <- NA
    }
    return(probPts)
  }
  
#this portion is accounting for same location issues
  else{
  
  #updates smove and nloc to remove boundry points 
    same <- which(smove == nloc)
    if(length(same) > 0){
    smove <- smove[-same]
    nloc <- nloc[-same]
    }
      
   pts_changed<-1
   counter<-0
   while(pts_changed!=0 && length(nloc) > 0){
     pts_changed<-0
    
#moves one sensor at a time 
     remove_vector<-c()
     for(i in 1:length(nloc)){
       if(placement[nloc[i]] == 0){ 
         new_placement<-replace(placement,smove[i],0)
         new_placement<-replace(new_placement,nloc[i],placement[smove[i]])
         counter<-counter+1
         pts_changed<-pts_changed + 1
         remove_vector<-c(remove_vector,i)
         placement<-new_placement
       }
       
     }
     #when remove_vector (the vector containing moved sensors) is not 0 we remove those points from smove
     if(length(remove_vector) !=0){
      nloc<-nloc[-remove_vector]
      smove<-smove[-remove_vector]
     }
   }
  }
  if(counter == 0){
    return(placement)
  }

  return(new_placement)
}
 
  


