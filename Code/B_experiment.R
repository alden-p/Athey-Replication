######################################################################################
# Clear data, import functions
######################################################################################

rm(list=ls(all=TRUE))
setwd("C:/Users/Porte/Documents/Courses/EC711/Athey-Replication/Code")

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
  sample.fraction = s/n, min.node.size = 1)
  
  nn <- knn.reg(train = X, test = NULL, k = 10, y = W)
  
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
  W = pred_data[["W"]]
  WX = data.frame(W,X)
  
  
  #Find Predictions, get mse
  cf.pred = predict(cf, X, estimate.variance = TRUE)
  nn.pred = nn[["pred"]]
  cf.mse = mean( (cf.pred[["predictions"]] - Y)^2 )
  nn.mse = mean( (nn.pred - Y)^2 )
  treat = estimate_average_effect(cf, target.sample = "all", method = "AIPW")
  
  
  output = list("cf mse" = cf.mse, "nn mse" = nn.mse, "cf pred" = cf.pred, "nn.pred" = nn.pred, "X" = X, "cf" = cf,
  "treat" = treat, "nn" = nn)
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
  
  p <- ggplot(data, aes(X1, X2)) + geom_point(aes(colour = Z)) + theme(legend.position="none") + scale_colour_gradient(low = "red", high = "yellow")
  return(p)
  }

n = 1000
d = 6
s = 250
B = 1000
start_time <- Sys.time()
result <- run_experiment(n, d, s, B, m1, t1, e1)


#Causal Forest Stuff
cf <- result[["cf"]]
cf.Yhat <- cf[["Y.hat"]]
cf.What <- cf[["W.hat"]]
cf.Yorig <- cf[["Y.orig"]]
cf.Worig <- cf[["W.orig"]]


#Nearest Neighbor stuff
nn <- result[["nn"]]
nn.What <- nn[["pred"]]


treat <- result[["treat"]] #CF treatment

end_time <- Sys.time()
end_time-start_time


p1 <- draw_fig2_piece(result[["X"]],t2(result[["X"]]) )
p2 <- draw_fig2_piece(result[["X"]],tau( cf.Worig, cf.Yorig, cf.What))
p3 <- draw_fig2_piece(result[["X"]],tau( cf.Worig, cf.Yorig, nn.What))
multiplot(p1, p2, p3, cols=3)
