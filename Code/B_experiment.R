#Author: Alden Porter
#This script performs the actual experiment found in Athey (2017)


######################################################################################
# Clear data, import functions
######################################################################################

rm(list=ls(all=TRUE))

source("A_data_generation.R")
source("multiplot.R")

#library(causalTree)

library(FNN)

#library(randomForestCI)

library(grf)

library(ggplot2)
######################################################################################
# Experiment 1
######################################################################################


get_predictors <- function(n, d, s, B, m, t, e){
  #n: Number of observations
  #D: Vector of the number of columns
  #s: Subsample Size
  #B: Number of Trees
  #m: main effect function
  #t: treatment effect function
  #e: treatment propensity function
  
    sim_data_full <- generate_data(n, d, m, t, e) #generate the data
    
    #Create a dataframe with only Y, W, and X
    drops <- c("Y0","Y1")
    sim_data = sim_data_full[ , !(names(sim_data_full) %in% drops)]
    
    Y0 <- sim_data_full["Y0"]
    Y1 <- sim_data_full["Y1"]
    X  <- sim_data[,3:ncol(sim_data)]
    Y  <- sim_data[,1]
    W  <- sim_data[,2]
    
    cf <- causal_forest(X, Y, W, num.trees = B, honesty = TRUE,
                   sample.fraction = s/n)
    
    nn <- knn.reg(train = X, test = NULL, k = 10, y = Y)
    
    output <- list("cf" = cf, "nn" = nn, "X" = X, "Y" =Y, "W"= W, "Y0" = Y0, "Y1" = Y1)
  }


run_experiment <-function(n, D, s, B, m, t, e){
  #n: Number of observations
  #D: Vector of the number of columns
  #s: Subsample Size
  #B: Number of Trees
  #m: main effect function
  #t: treatment effect function
  #e: treatment propensity function
  
  #Run The Causal Forest Alg. On the Data
  pred_data <- get_predictors(n, D, s, B, m, t, e)

  cf <- pred_data[["cf"]]
  nn <- pred_data[["nn"]]
  Y0 = pred_data[["Y0"]]
  Y1 = pred_data[["Y1"]]
  Y = pred_data[["Y"]]
  X = pred_data[["X"]]
  
  #Find Predictions, get mse
  cf.pred = predict(cf, X, estimate.variance = TRUE)
  nn.pred = nn[["pred"]]
  
  cf.mse = mean( (cf.pred[["predictions"]] - Y)^2 )
  nn.mse = mean( (nn.pred - Y)^2 )
  
  
  
  output = list("cf mse" = cf.mse, "nn mse" = nn.mse, "cf pred" = cf.pred, "nn.pred" = nn.pred, "X" = X)
  
  return(output)
}



draw_fig2_piece <- function(X, Z){
  
  ## Example data
  X1 = X[,1]
  X2 = X[,2]
  
  data <- X
  data$Z <- Z
  
  # ggplot(data, aes(X, Y, z= Z)) + geom_tile(aes(fill = Z)) + theme_bw()
  
  # To change the color of the gradation :
  p <- ggplot(data, aes(X1, X2)) + geom_point(aes(colour = Z)) + scale_colour_gradient(low = "red", high = "yellow")
  
  return(p) 
  
}












n = 1000
d = 5
s = 250
B = 1000


start_time <- Sys.time()
result <- run_experiment(n, d, s, B, m1, t1, e1)
end_time <- Sys.time()
end_time-start_time

p1 <- draw_fig2_piece(result[["X"]],t2(result[["X"]]) )
p2 <- draw_fig2_piece(result[["X"]],t2(result[["X"]]))

multiplot(p1, p2, cols=2)
