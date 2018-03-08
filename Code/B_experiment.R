#Author: Alden Porter
#This script performs the actual experiment found in Athey (2017)


######################################################################################
# Clear data, import functions
######################################################################################

rm(list=ls(all=TRUE))

source("A_data_generation.R")

library(causalTree)

library(FNN)

library(randomForestCI)

######################################################################################
# Experiment 1
######################################################################################




run_experiment <- function(n, D, m, t, e)
  #n: Number of observations
  #D: Vector of the number of columns
  #m: main effect function
  #t: treatment effect function
  #e: treatment propensity function
  for (d in D){
    sim_data_full <- generate_data(n, d, m, t, e) #generate the data
    
    #Create a dataframe with only Y, W, and X
    drops <- c("Y0","Y1")
    sim_data = sim_data_full[ , !(names(sim_data_full) %in% drops)]
#   https://www.datacamp.com/community/tutorials/r-formula-tutorial    <- Formulas in R
#   cf <- causalForest( , data = sim_data, treatment = sim_data$W, sim_data)
#    nn <- kNN(form, train, test, norm = T, norm.stats = NULL, ...)
    return(sim_data)    
  }
causalForest()