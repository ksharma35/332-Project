library(miscTools)
library(stringr)
library(devTools)
library(ReinforcementLearning)
#### creates initial Solution ####

user_data<-read.csv("initial_test.csv")

n <<- sqrt(nrow(user_data))
budgetmin<- as.numeric(user_data$BudgetMinimum[1])
budgetmax<- as.numeric(user_data$BudgetMaximum[1])

cat("Budget Min:", 0.9*budgetmin)
cat("Budget Max:", 0.9*budgetmax)

budget <- as.numeric(readline(prompt = "Enter budget:"))

num_hours<-24
region_name <- user_data$Name[1] 
lat <- user_data$Center.Latitude[1]
long <- user_data$Center.Longitude[1]
writeRegion(user_data)
writeLocation(user_data,region_name)



simulated_data<- trunc(data.frame(replicate(144,rnorm(24,26,7))),1) 

numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 
# Load environment function for nxn gridworld 

sim_pm_hourly <<- c()
action <- c("North East", "North West", "South East", "South West","North", "South","East", "West","Stay");
state <- matrix(paste("s", 1:(n*n), sep = ""),nrow=n, ncol=n, byrow= TRUE);
rec_policy<- array(0,dim=c(nrow(user_data),9,num_hours))
env_m <- gridnbyn;



for(i in 1:(nrow(simulated_data))){

sim_pm_hourly <-simulated_data[i,]  
# Sample N = 1000 random sequences from the environment
data <- data.frame(sampleExperience(N = 20000, 
                                    env = env_m, 
                                    states = state, 
                                    actions = action))

# Define control object
control <- list(alpha = 0.2, gamma = 0.4, epsilon = 0.2)

# Pass learning parameters to reinforcement learning function

model <- ReinforcementLearning(data, iter = 10, control = control, s="State", a="Action", r="Reward" , s_new="NextState")

#orders the rows of the Q df by row number in ascending order and orders the columns of Q alphabetically
row.names(model$Q)<-as.numeric(numextract(row.names(model$Q)))
model$Q<-model$Q[ order(as.integer(row.names(model$Q))),order((colnames(model$Q))) ]

#saves model$q as a new entry in the multidimensional array called rec_policy 
rec_policy[,,i]<-as.array(model$Q)

}

#### Simulated Annealing ####
placement <- SA(budget, num_hours, user_data, simulated_data, rec_policy, temperature=1000, maxit=1000, cooling=0.95, just_values=TRUE)
writeSensors(placement,num_hours,user_data,rec_policy,region_name)
writeReadings(placement,num_hours,user_data,rec_policy,simulated_data)


cat("Budget:", budget)
cat("Objective Score:", soln_score)
cat("Number of Locations Covered:", max(which(placement != 0)))
