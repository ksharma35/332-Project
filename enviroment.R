#The Enviroment Function 
env <- function(state, action, n){
  library(stringr)
  numextract <- function(string){ 
    str_extract(string, "\\-*\\d+\\.*\\d*")
  } 
  
  next_state <- state

  statenum = as.numeric(numextract(state))
  next_state <- state;
  if ((statenum >= 1 && statenum <= n) || (statenum >= n*n-n+1 && statenum <= n*n) || ((statenum %% n)==0) || ((statenum - 1) %% n == 0) ){
    if (state == "s1" && action == "East"){
      statenum = statenum + 1;
      next_state <- state(paste("s",statenum, sep = "")) 
    }
    
    else if (state == "s1" && action == "North"){
      next_state <- state
    }
    
    else if (state == "s1" && action == "North East"){
      next_state <- state
    }
    
    else if (state == "s1" && action == "North West"){
      next_state <- state
    }
    else if (state == "s1" && action == "Stay"){
      next_state <- state
    }
    
    else if (state == "s1" && action == "West"){
      next_state <- state
    }
    
    else if (state == "s1" && action == "South West"){
      next_state <- state
    }
    
    else if (state == "s1" && action == "South"){
      statenum = statenum + n;
      next_state <- state(paste("s",statenum, sep = "")) 
    }
    
    else if (state == "s1" && action == "South East"){
      statenum = statenum + n + 1;
      next_state <- state(paste("s",statenum, sep = ""))  
    }
    
    else if (state == paste("s",n*n-n+1, sep = "") && action == "North" ){
      statenum = statenum - n;
      next_state <- state(paste("s",statenum, sep = ""))  
    }
    
    else if (state == paste("s",n*n-n+1, sep = "") && action == "East" ){
      statenum = statenum + 1;
      next_state <- state(paste("s",statenum, sep = ""))  
    }
    
    else if (state == paste("s",n*n-n+1, sep = "") && action == "North East" ){
      statenum = statenum - n + 1;
      next_state <- state(paste("s",statenum, sep = ""))  
    }
    
    else if (state == paste("s",n*n-n+1, sep = "") && (action == "South" || action == "Stay") ){
      next_state <- state
    }
    
    else if (state == paste("s",n*n-n+1, sep = "") && action == "South West" ){
      next_state <- state
    }
    
    else if (state == paste("s",n*n-n+1, sep = "") && action == "South East" ){
      next_state <- state
    }
    
    else if (state == paste("s",n*n-n+1, sep = "") && action == "West" ){
      next_state <- state
    }
    
    else if (state == paste("s",n*n-n+1, sep = "") && action == "North West" ){
      next_state <- state
    }
    
    else if (state == paste("s",n*n, sep = "") && action == "North" ){
      statenum = statenum - n;
      next_state <- state(paste("s",statenum, sep = ""))  
    }
    
    else if (state == paste("s",n*n, sep = "") && action == "West" ){
      statenum = statenum - 1;
      next_state <- state(paste("s",statenum, sep = ""))  
    }
    
    else if (state == paste("s",n*n, sep = "") && action == "North West" ){
      statenum = statenum - n + 1;
      next_state <- state(paste("s",statenum, sep = ""))  
    }
    
    else if (state == paste("s",n*n, sep = "") && (action == "North East" || action == "Stay")){
      next_state <- state
    }
    else if (state == paste("s",n*n, sep = "") && action == "South" ){
      next_state <- state
    }
    else if (state == paste("s",n*n, sep = "") && action == "South West" ){
      next_state <- state
    }
    else if (state == paste("s",n*n, sep = "") && action == "South East" ){
      next_state <- state
    }
    else if (state == paste("s",n*n, sep = "") && action == "East" ){
      next_state <- state
    }
    
    else if (state == paste("s",n, sep = "") && action == "South"){
      statenum = statenum + n;
      next_state <- state(paste("s",statenum, sep = ""))  
    }
    
    else if (state == paste("s",n, sep = "") && action == "West" ){
      statenum = statenum - 1;
      next_state <- state(paste("s",statenum, sep = ""))  
    }
    
    else if (state == paste("s",n, sep = "") && action == "South West" ){
      statenum = statenum + n - 1;
      next_state <- state(paste("s",statenum, sep = "")) 
    }
    else if (state == paste("s",n, sep = "") && action == "North"){
      next_state <- state
    }
    else if (state == paste("s",n, sep = "") && (action == "North West" || action == "Stay")){
      next_state <- state
    }
    else if (state == paste("s",n, sep = "") && action == "North East"){
      next_state <- state
    }
    else if (state == paste("s",n, sep = "") && action == "East"){
      next_state <- state
    }
    else if (state == paste("s",n, sep = "") && action == "South East"){
      next_state <- state
    }
    
    # Direction logic for top row excluding corners
    
    else if (statenum > 1 && statenum < n && action == "South"){
      statenum = statenum + n;
      next_state <- state(paste("s",statenum, sep = ""))  
    }
    
    else if (statenum > 1 && statenum < n && action == "West"){
      statenum = statenum - 1;
      next_state <- state(paste("s",statenum, sep = ""))  
    }
    
    else if (statenum > 1 && statenum < n && action == "East"){
      statenum = statenum + 1;
      next_state <- state(paste("s",statenum, sep = ""))   
    }
    
    else if (statenum > 1 && statenum < n && action == "South West"){
      statenum = statenum + n - 1;
      next_state <- state(paste("s",statenum, sep = ""))  
    }
    
    else if (statenum > 1 && statenum < n && action == "South East"){
      statenum = statenum +n + 1;
      next_state <- state(paste("s",statenum, sep = ""))  
    }
    
    else if (statenum > 1 && statenum < n && (action == "North East" || action == "Stay")){
      next_state <- state
    }
    else if (statenum > 1 && statenum < n && action == "North"){
      next_state <- state
    }
    else if (statenum > 1 && statenum < n && action == "North West"){
      next_state <- state
    }
    
    # Direction logic for the bottom row excluding corners
    
    else if (statenum > n*n-n+1 && statenum < n*n && action == "North"){
      statenum = statenum - n;
      next_state <- state(paste("s",statenum, sep = ""))  
    }
    
    else if (statenum > n*n-n+1 && statenum < n*n && action == "East"){
      statenum = statenum + 1;
      next_state <- state(paste("s",statenum, sep = ""))  
    }
    
    else if (statenum > n*n-n+1 && statenum < n*n && action == "West"){
      statenum = statenum - 1;
      next_state <- state(paste("s",statenum, sep = ""))  
    }
    
    else if (statenum > n*n-n+1 && statenum < n*n && action == "North East"){
      statenum = statenum - n + 1;
      next_state <- state(paste("s",statenum, sep = ""))  
    }
    
    else if (statenum > n*n-n+1 && statenum < n*n && action == "North West"){
      statenum = statenum - n - 1;
      next_state <- state(paste("s",statenum, sep = ""))  
    }
    else if (statenum > n*n-n+1 && statenum < n*n && (action == "South West" || action == "Stay")){
      next_state <- state
    }
    else if (statenum > n*n-n+1 && statenum < n*n && action == "South East"){
      next_state <- state
    }
    else if (statenum > n*n-n+1 && statenum < n*n && action == "South"){
      next_state <- state
    }
    
    # Direction logic for right vertical side excluding corners
    
    else if ((statenum %% n) == 0 && (statenum != n) && (statenum != n*n) && action == "North"){
      statenum = statenum - n;
      next_state <- state(paste("s",statenum, sep = "")) 
    }
    
    else if ((statenum %% n) == 0 && (statenum != n) && (statenum != n*n) && action == "South"){
      statenum = statenum + n;
      next_state <- state(paste("s",statenum, sep = ""))
    }
    
    else if ((statenum %% n) == 0 && (statenum != n) && (statenum != n*n) && action == "West"){
      statenum = statenum - 1;
      next_state <- state(paste("s",statenum, sep = ""))  
    }
    
    else if ((statenum %% n) == 0 && (statenum != n) && (statenum != n*n) && action == "North West"){
      statenum = statenum - n - 1;
      next_state <- state(paste("s",statenum, sep = "")) 
    }
    else if ((statenum %% n) == 0 && (statenum != n) && (statenum != n*n) && action == "South West"){
      statenum = statenum + n - 1;
      next_state <- state(paste("s",statenum, sep = "")) 
    }
    
    else if ((statenum %% n) == 0 && (statenum != n) && (statenum != n*n) && (action == "North East" || action == "Stay")){
      next_state <- state
    }
    else if ((statenum %% n) == 0 && (statenum != n) && (statenum != n*n) && action == "South East"){
      next_state <- state
    }
    else if ((statenum %% n) == 0 && (statenum != n) && (statenum != n*n) && action == "East"){
      next_state <- state
    }
    
    # Direction logic for left vertical side of matrix excluding corners
    
    else if (((statenum-1) %% n) == 0 && (statenum != 1) && (statenum != (n*n-n+1)) && (action == "North")){
      statenum = statenum - n;
      next_state <- state(paste("s",statenum, sep = "")) 
    }
    
    else if (((statenum-1) %% n) == 0 && (statenum != 1) && (statenum != (n*n-n+1)) && (action == "South")){
      statenum = statenum + n;
      next_state <- state(paste("s",statenum, sep = "")) 
    } 
    
    else if (((statenum-1) %% n) == 0 && (statenum != 1) && (statenum != (n*n-n+1)) && (action == "East")){
      statenum = statenum + 1;
      next_state <- state(paste("s",statenum, sep = "")) 
    }
    
    else if (((statenum-1) %% n) == 0 && (statenum != 1) && (statenum != (n*n-n+1)) && (action == "West" || action == "Stay")){
      next_state <- state;
    }
    
    else if (((statenum-1) %% n) == 0 && (statenum != 1) && (statenum != (n*n-n+1)) && (action == "North West")){
      next_state <- state;
    }
    
    else if (((statenum-1) %% n) == 0 && (statenum != 1) && (statenum != (n*n-n+1)) && (action == "North East")){
      statenum = statenum -n + 1;
      next_state <- state(paste("s",statenum, sep = "")) 
    }
    
    else if (((statenum-1) %% n) == 0 && (statenum != 1) && (statenum != (n*n-n+1)) && (action == "South West")){
      next_state <- state;
    }
    
    else if (((statenum-1) %% n) == 0 && (statenum != 1) && (statenum != (n*n-n+1)) && (action == "South East")){
      statenum = statenum + n + 1;
      next_state <- state(paste("s",statenum, sep = "")) 
    }
  }
  
  else {
    if (action == "North"){
      statenum = statenum - n;
      
      next_state <- state(paste("s",statenum, sep = "")); 
    }
    else if (action == "North West"){
      statenum = statenum - n - 1;
      next_state <- state(paste("s",statenum, sep = "")); 
    }
    else if (action == "North East"){
      statenum = statenum - n + 1;
      next_state <- state(paste("s",statenum, sep = "")); 
    }
    else if (action == "South"){
      statenum = statenum + n;
      next_state <- state(paste("s",statenum, sep = "")); 
    }
    else if (action == "South West"){
      statenum = statenum + n - 1;
      next_state <- state(paste("s",statenum, sep = "")); 
    }
    else if (action == "South East"){
      statenum = statenum + n + 1;
      next_state <- state(paste("s",statenum, sep = "")); 
    }
    else if (action == "East"){
      statenum = statenum + 1;
      next_state <- state(paste("s",statenum, sep = "")); 
    }
    else if (action == "West"){
      statenum = statenum - 1;
      next_state <- state(paste("s",statenum, sep = "")); 
    }
    else if( action == "Stay"){
      next_state <- state
    }
  }
  return(statenum)
}