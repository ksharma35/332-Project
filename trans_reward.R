## Translate Max Reward ## 
trans_reward <- function(number_direction){
  if(number_direction == 1){
    return("East")
  }
  else if(number_direction == 2){
    return("North")
  }
  else if(number_direction == 3){
    return("North East")
  }
  else if(number_direction == 4){
    return("North West")
  }
   else if(number_direction == 5){
    return("South")
  }
  else if(number_direction == 6){
    return("South East")
  }
  else if(number_direction == 7){
    return("South West")
  }
  else if(number_direction == 8){
    return("Stay")
  }
  else if(number_direction == 9){
    return("West")
  }
}