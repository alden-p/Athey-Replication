#Author: Alden Porter
#This code constructs functions for generating the simulation data for Athey(2017)
######################################################################################

######################################################################################
# Clear data
######################################################################################

rm(list=ls(all=TRUE))

######################################################################################
# Define m,t, and e functions 
######################################################################################

###Generic Stuff

sigmoid <- function(x,a,b){
  result <- (1+exp(-a*(x-b)))^-1
  return(result)
}

###First Experiment

m1 <- function(x){
  return(2*x[,1] + 1)
}

t1 <- function(x){
  return(0)
}

e1 <- function(x){
  result <- 1/4 * (1+dbeta(x[,1], 2, 4, ncp = 0, log = FALSE))
  return(result)
}

###Second Experiment

m2 <- function(x){
  return(0)
}

t2 <- function(x){
  result <- ( 1 + sigmoid(x[,1],20,1/3) )*( 1 + sigmoid(x[,2],20,1/3) )
  return(result)
}

e2 <- function(x){
  return(.5)
}


######################################################################################
# Function to Get tau(x) under unconfoundedness
######################################################################################


tau <- function(W, Y, e){
  #X: A matrix of observables
  #W: The treatment
  #Y: The outcome
  #e: The treatment propensity
  
  
  t <- Y*(W/e - (1-W)/(1-e))
  
  return (t)
  
}


######################################################################################
# Define the Actual Generate Data Function
######################################################################################

generate_data <- function(n, d, m, t, e){
  
  X <- matrix(runif(n*d, min = 0, max = 1), nrow=n, ncol=d, byrow = FALSE) # Generate a nxd matrix of U[0,1] numbers 
  
  EY1 <- 2*m(X) - t(X) #Get the expected value of Y1 
  EY0 <- 2*m(X) + t(X) #Get the expected value of Y0
  
  Y0 <- rnorm(n, mean = EY0, sd = 1) #Compute Actual Values of Y1
  Y1 <- rnorm(n, mean = EY1, sd = 1) #Compute Actual Values of Y0
  
  prob <- e(X) #Treatment Probability
  
  W <- rbinom(n,1,prob) #Assign treatment randomly based on e
  
  Y = W*Y1 + (1-W)*Y0 #Observed Outcome
  
  df <- cbind(data.frame( cbind(Y, Y0, Y1, W) ), data.frame(X))
  
  return(df)
}