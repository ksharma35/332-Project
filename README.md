# IE332-Semester Project
   This was a school group project that I took part in. For the project I had to develop an optimization algorithm in R that used 
Reinforcement Learning to determine the best path that mobile and fixed air quality sensors should take to effectively cover
any region given a budget. The output of the algorithm was written to a database that was also created as a part of this project using
MySQL. This was done with the writeLocations, writeReadings, and writeRegion functions. The Reinforcement Learning was implemented with
in the function called main and environment. The input to the ReinforcementLearning function is the output of a simulation that outputs
the expected particulate matter values for a specific region. The files responsible for the simulation are Stack, CleanRegression,
humidity, multiplier, noise, pressure, and temperature. 

  I used a simulated annealing algorithm to determine the initial placement of the sensors in the region. The files responsible for the
neighbor, objective_value, feasibility_checks, and Simulated Annealing. After creating an initial placement with these files the optimal
movement strategy for the mobile sensors is determined using the output from the ReinforcementLearning algorithm. This takes place in the
translation, trans_reward, and movement.  

  
