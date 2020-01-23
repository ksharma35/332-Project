#### Simulated Annealing ####
SA <- function(budget, num_hours, user_data, simulated_data, rec_policy, temperature=600, maxit=1000, cooling=0.95, just_values=TRUE) {
  # budget: the user defined budget 
  # user_data: data.frame of user information
  # num_hours: number of hours that you are simulating over
  # temperature: initial temperature
  # maxit: maximum number of iterations to execute for
  # cooling: rate of cooling
  # just_values: only return a list of best objective value at each iteration
  c_sol <- create_initial(budget, user_data,num_hours) 
  # need to add simulation data function here
  c_obj <- score_layout(c_sol, simulated_data, num_hours, rec_policy)     # evaluate initial solution
  best  <- c_sol                                              # track the best solution found so far
  best_obj <- c_obj                                           # track the best objective value found so far
  # to keep best objective values through the algorithm
  obj_vals <- best_obj
  cnt <- 0
  # run Simulated Annealing
  for(i in 1:maxit) {
    neigh <- neighbor(c_sol,budget)                                      # generate a neighbor solution
    neigh_obj <- score_layout(neigh, simulated_data, num_hours, rec_policy)   # calculate tje objective value of the new solution
     if ( neigh_obj >= best_obj ) {                                # keep neigh if it is the new global best solution
      c_sol <- neigh
      c_obj <- neigh_obj
      best <- neigh
      best_obj <- neigh_obj
     } else if( runif(1) <= exp(-(c_obj-neigh_obj)/temperature) ) {    # otherwise, probabilistically accept
      c_sol <- neigh
      c_obj <- neigh_obj
      cnt <- cnt +1
     }
    
    temperature <- temperature * cooling                          # update cooling
    obj_vals <- c(obj_vals, best_obj)                             # maintain list of best found so far
  }
  ifelse(just_values, return(best)) #,return(list(best=best, best_obj=best_obj, values=obj_vals)))
}