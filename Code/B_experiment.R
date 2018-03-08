#Author: Alden Porter
#This script performs the actual experiment found in Athey (2017)


######################################################################################
# Clear data, import functions
######################################################################################

rm(list=ls(all=TRUE))

source("A_data_generation.R")

#library(causalTree)

library(FNN)

#library(randomForestCI)

library(grf)
######################################################################################
# Experiment 1
######################################################################################


run_experiment <- function(n, D, s, B, m, t, e)
  #n: Number of observations
  #D: Vector of the number of columns
  #s: Subsample Size
  #B: Number of Trees
  #m: main effect function
  #t: treatment effect function
  #e: treatment propensity function
  
  for (d in D){
    sim_data_full <- generate_data(n, d, m, t, e) #generate the data
    
    #Create a dataframe with only Y, W, and X
    drops <- c("Y0","Y1")
    sim_data = sim_data_full[ , !(names(sim_data_full) %in% drops)]
    
    # #Create A Formula Object to Be passed into causalForest and the like.
    # Xnam <- paste("X", 1:(dim(sim_data)[2]-2), sep="")
    # fmla <- as.formula(paste("Y~W+", paste(Xnam, collapse= "+")))
    
    
    # cf <- causal_forest(fmla, data = sim_data, treatment = sim_data$W , num.trees = B, ncolx = d+1,
    #                    split.Rule = "CT", split.Bucket = F , split.alpha = .19, sample.size.total = n,
    #                    sample.size.train.frac = .5, double.Sample = T, ncov_sample = 2 )
    
    causal_forest(X = sim_data[,3:ncol(sim_data)], Y = sim_data[,1], W = sim_data[,2], num.trees = B, honesty = TRUE,
                   sample.fraction = .5)
    
    nn <- knn.reg(train = sim_data, test = NULL, k = 10, y = sim_data$Y)
     
     
     
    return(sim_data[,3:ncol(sim_data)])    
  }

n = 100
D = c(5)
s = 20
B = 100


df <- run_experiment(n, D, s, B, m2, t2, e2)