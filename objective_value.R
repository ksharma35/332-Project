#### Objective Value Function ####

score_layout<-function(placement, simulated_data, num_hours, rec_policy){
  
  layout_movement <- movement(placement, rec_policy, num_hours)
  #print(layout_movement)
  
  sensor_locations <- which(layout_movement > 0)
  #print(sensor_locations)
  pm_at_locations<-unlist(simulated_data,use.names = F)
  obj_value <- sum(pm_at_locations[sensor_locations])
  return(obj_value)
}

