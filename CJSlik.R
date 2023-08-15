########## Contains functions used to estimate demographic parameters ##########

library(reshape2)
## Printing message to confirm if we have linked files successfully
cat('Loaded functions open population file successfully')

## Function to calculate inverse of logit to convert the parameters into
## desired probabiltiy range
inverse.logit <- function(x) {
  exp_x <- exp(x)
  return(exp_x / (1 + exp_x))
}

####################### Different likelihood functions #########################

CJSlik <- function(theta, x, f, l, n, T) {
  # The data are stored in the array x;
  # n = number of observed individuals; T = number of capture occasions
  # f - array corresponding to first time each individual is observed
  # f can be calculated using e.g.:
  #   for (i in 1:n){f[i] <- which(x[i,]==1)[1]}
  # l - array corresponding to last time each individual is observed
  # l can be calculated using e.g.:
  #   for (i in 1:n){l[i] <- which(x[i,]==1)[length(which(x[i,]==1))]}
  # Theta stores the set of parameter values - specified on the real line.
  # Define the parameter values in terms of capture probabs and population size.
  # Use the transformations: logit phi[1:T-1] = theta[1:T-1]; logit p = theta[T]
  phi <- exp(theta[1:(T-1)])/(1+exp(theta[1:(T-1)]))
  p <- exp(theta[T])/(1+exp(theta[T]))
  # Calculate the chi terms: probability of not observed after time t
  # Initially set up chi to be an array of length T all elements equal to 1
  # chi[T] <- 1
  # Calculate chi[1:T-1] using recursive formula
  chi <- array(1,T)
  for (t in (T-1):1){
    chi[t] <- 1 - phi[t] + phi[t]*(1-p)*chi[t+1]
  }
  # Initialise the log-likelihood value for each individual:
  lik <- array(0,n)
  # Calculate the (log-)likelihood component for each individual capture history
  for (i in 1:n){
    # Consider the times between initial capture and final capture for each individual
    # Contribution only required when l[i] is not equal to f[i]
    if (f[i] != l[i]){
      for (t in f[i]:(l[i]-1)){
        lik[i] <- lik[i] + log(phi[t]) + x[i,t+1]*log(p) + (1-x[i,t+1])*log(1-p)
      } }
    # Note the above function assumes that we do not have any observed
    # histories with a single observation at the final capture occasion
    # I.e. we no not have any histories such that f = l = T.
    # For such histories there is no information (and so can be omitted).
    # Add chi terms (probability of not being observed after time l[i])
    lik[i] <- lik[i] + log(chi[l[i]])
  }
  # Calculate log-likelihood over all individuals:
  sumlik <- sum(lik)
  # Output the log-likelihood value:
  return(sumlik) 
}

## Function to calculate the CJS likelihood with phi as a function of age
## constant recapture and time-varying survival
CJSlik.age.cons <- function(theta, x, f, l, n, age, T) {
  
  # The data are stored in the array x; 
  # n = number of observed individuals; T = number of capture occasions
  
  # f - array corresponding to first time each individual is observed
  # f can be calculated using e.g.: 
  #   for (i in 1:n){f[i] <- which(x[i,]==1)[1]}
  
  # l - array corresponding to last time each individual is observed
  # l can be calculated using e.g.: 
  #   for (i in 1:n){l[i] <- which(x[i,]==1)[length(which(x[i,]==1))]}
  
  # Theta stores the set of parameter values - specified on the real line.
  # Define the parameter values in terms of capture probs and population size.
  # Use the transformations: logit phi[1:T-1] = theta[1:T-1]; logit p = theta[T]
  
  ## Getting the parameter for juveniles
  alpha <- theta[1:(T-1)]
  ## Getting the parameter beta for adults
  beta <- theta[T]
  
  ## Make phi age dependent using logit phi = alpha + beta(I=age)
  ## where I is indicator function
  phi <- matrix(0, nrow = n, ncol = T-1)
  for(i in 1:n){
    for(j in 1:(T-1)){
        phi[i,j] <- exp(alpha[j] + beta * (age[i,j]-1))/(1+exp(alpha[j] + 
                                                      beta * (age[i,j]-1)))
      
    }
  }
  ## Defining the recapture probability
  p <- exp(theta[T+1])/(1+exp(theta[T+1]))
  
  # Calculate the chi terms: probability of not observed after time t for each 
  # individual separately because as phi is dependent on age 
  # Initially set up chi to be an array of length T all elements equal to 1
  # chi[T] <- 1
  # Calculate chi[1:T-1] using recursive formula
  
  chi <- matrix(1, nrow = n, ncol = T)
  
  for (i in 1:n) {
    for (t in (T - 1):1) {
      chi[i, t] <- 1 - phi[i, t] + phi[i, t]*(1-p)*chi[i, t+1]
    }
  }
  
  # Initialise the log-likelihood value for each individual:
  lik <- array(0,n)
  
  # Calculate the (log-)likelihood component for observed individual capture histories
  
  for (i in 1:n){
    
    # Consider the times between initial capture and final capture for each individual
    # Contribution only required when l[i] is not equal to f[i]
    
    if (f[i] != l[i]){
    
      for (t in f[i]:(l[i]-1)){
        lik[i] <- lik[i] + log(phi[i, t]) + x[i,t+1]*log(p) + 
          (1-x[i,t+1])*log(1-p)
      }
    }
    
    # Note the above function assumes that we do not have any observed
    # histories with a single observation at the final capture occasion
    # I.e. we no not have any histories such that f = l = T. 
    # For such histories there is no information (and so can be omitted).
    
    # Add chi terms (probability of not being observed after time l[i])
    lik[i] <- lik[i] + log(chi[i, l[i]])
    
  }
  
# Calculate log-likelihood over all individuals:
  
  sumlik <- sum(lik)
  
  # Output the log-likelihood value:
  
  return(sumlik)
}

## Function to calculate the CJS likelihood with phi as a function of age with
## varying recapture probability
CJSlik.age.var <- function(theta, x, f, l, n, age, T) {
  
  # The data are stored in the array x; 
  # n = number of observed individuals; T = number of capture occasions
  
  # f - array corresponding to first time each individual is observed
  # f can be calculated using e.g.: 
  #   for (i in 1:n){f[i] <- which(x[i,]==1)[1]}
  
  # l - array corresponding to last time each individual is observed
  # l can be calculated using e.g.: 
  #   for (i in 1:n){l[i] <- which(x[i,]==1)[length(which(x[i,]==1))]}
  
  # Theta stores the set of parameter values - specified on the real line.
  # Define the parameter values in terms of capture probs and population size.
  # Use the transformations: logit phi[1:T-1] = theta[1:T-1]; logit p = theta[T]
  
  ## Getting the parameter for juveniles
  alpha <- theta[1:(T-1)]
  ## Getting the parameter beta for adults
  beta <- theta[T]
  
  ## Make phi age dependent using logit phi = alpha + beta(I=age)
  ## where I is indicator function
  phi <- matrix(0, nrow = n, ncol = T-1)
  for(i in 1:n){
    for(j in 1:(T-1)){
      phi[i,j] <- exp(alpha[j] + beta * (age[i,j]-1))/(1+exp(alpha[j] + 
                                          beta * (age[i,j]-1)))
      }
    }
  ## Defining the recapture probability
  p <- exp(theta[(T+1): (T+11)])/(1+exp(theta[(T+1): (T+11)]))
  
  # Calculate the chi terms: probability of not observed after time t for each 
  # individual separately because as phi is dependent on age 
  # Initially set up chi to be an array of length T all elements equal to 1
  # chi[T] <- 1
  # Calculate chi[1:T-1] using recursive formula
  
  chi <- matrix(1, nrow = n, ncol = T)
  
  for (i in 1:n) {
    for (t in (T - 1):1) {
      chi[i, t] <- (1 - phi[i, t]) + phi[i, t]*(1-p[t+1])*chi[i, t+1]
    }
  }
  
  # Initialise the log-likelihood value for each individual:
  lik <- array(0,n)
  
  # Calculate the (log-)likelihood component for observed individual capture histories
  
  for (i in 1:n){
    
    # Consider the times between initial capture and final capture for each individual
    # Contribution only required when l[i] is not equal to f[i]
    
    if (f[i] != l[i]){
      
      for (t in f[i]:(l[i]-1)){
        lik[i] <- lik[i] + log(phi[i, t]) + x[i,t+1]*log(p[t+1]) + 
          (1-x[i,t+1])*log(1-p[t+1])
      }
    }
    
    # Note the above function assumes that we do not have any observed
    # histories with a single observation at the final capture occasion
    # I.e. we no not have any histories such that f = l = T. 
    # For such histories there is no information (and so can be omitted).
    
    # Add chi terms (probability of not being observed after time l[i])
    lik[i] <- lik[i] + log(chi[i, l[i]])
    
  }
  
  # Calculate log-likelihood over all individuals:
  
  sumlik <- sum(lik)
  
  # Output the log-likelihood value:
  
  return(sumlik)
}

################################################################################
################## Below are functions used to maximise different models #######
 
## Function to maximize the log-likelihood to obtain MLE with no age-dependence 
## CJS model
log.mle.open <- function(theta, x){
  ## Getting the number of observations
  n <- nrow(x)
  T <- ncol(x)
  ## creating empty arrays to store initial and final capture occasions
  f <- rep(0, n)
  l <- rep(0, n)
  ## Stores first individual is observed
  for (i in 1:n){f[i] <- which(x[i,]==1)[1]}
  ## Storing the last time individual is observed
  for (i in 1:n){l[i] <- which(x[i,]==1)[length(which(x[i,]==1))]}
  ## Using the optim to get MLE
  res <- optim(par = theta, fn = CJSlik, x=x, 
               n=n, f=f, l=l, T=T, control = list(fnscale=-1, temp = 20,  
               maxit = 10000), method = "SANN")
  print(res)
  ## Returning the estimated parameters 
  return(res$par)
}





## Function to maximize the log-likelihood to obtain MLE for constant recapture
## probability with time-varying survival age-dependece CJS model
log.mle.open.age.p.cons <- function(theta, x, age){
  ## Getting the number of observations
  n <- nrow(x)
  T <- ncol(x)
  ## creating empty arrays to store initial and final capture occasions
  f <- rep(0, n)
  l <- rep(0, n)
  ## Stores first individual is observed
  for (i in 1:n){f[i] <- which(x[i,]==1)[1]}
  ## Storing the last time individual is observed
  for (i in 1:n){l[i] <- which(x[i,]==1)[length(which(x[i,]==1))]}
  ## Using the optim to get MLE
  res <- optim(par = theta, fn = CJSlik.age.cons, x=x, 
          n=n, f=f, l=l, age=age, T=T, control = list(fnscale=-1), 
          method = "BFGS")
  ## Storing the estimated MLEs for all parameters
  theta <- res$par
  ## Returning the paramters
  return(theta)
}

## Function to maximize the log-likelihood to obtain MLE with varying recapture
## and varying survrial 
log.mle.open.age.p.var <- function(theta, x, age){
  ## Getting the number of observations
  n <- nrow(x)
  T <- ncol(x)
  ## creating empty arrays to store initial and final capture occasions
  f <- rep(0, n)
  l <- rep(0, n)
  ## Stores first individual is observed
  for (i in 1:n){f[i] <- which(x[i,]==1)[1]}
  ## Storing the last time individual is observed
  for (i in 1:n){l[i] <- which(x[i,]==1)[length(which(x[i,]==1))]}
  ## Setting initial difference between old and estimate observed 
  res <- optim(par = theta, fn = CJSlik.age.var, x=x, 
               n=n, f=f, l=l, age=age, T=T, control = list(fnscale=-1, temp=20, 
               maxit = 10000), method = "SANN")
  
  ## Returning the estimated parameters 
  return(res$par)
}

## Function to get the arameter estimate using inverse of logit based on MLE
popEstimate.open <- function(theta, T, flag){
  ## Getting the alpha and beta from the estimated parameter theta
  alpha <- theta[1:(T-1)]
  beta <- theta[T]
  
  ## Calculating the juvenile and adult survival probabilities based on 
  ## logit phi = a (for juvenile) and for adult logit phi = alpha + beta
  phi_juve <- exp(alpha)/(1+exp(alpha))
  phi_adul <- exp(alpha + beta)/(1+exp(alpha + beta)) 
  
  if(flag ==1){
  ## Getting the constant recapture probability
    p <- exp(theta[T+1])/(1+exp(theta[T+1]))
  }
  else{
    ## Getting the time-varying recapture probability
    p <- exp(theta[(T+2): (T+11)])/(1+exp(theta[(T+2): (T+11)]))
  }
  ## Returning the parameters in a list format
  return(list(phi_juve, phi_adul, p))
}



## Function for creating m-arrays and observed proabilities
m_array <- function(yearly_data, T){
  
  ## Below Example array taken from the book Analysis of capture-recapture data
  ## by morgan and MCcrea to check the functioning of m-arrays
  # yearly_data <- matrix(0, nrow = 4, ncol = 5)
  # yearly_data[1, 1] <- 0
  # yearly_data[1, 2] <- 1
  # yearly_data[1, 3] <- 0
  # yearly_data[1, 4] <- 1
  # yearly_data[1, 5] <- 1
  #  
  # yearly_data[2, 1] <- 0
  # yearly_data[2, 2] <- 1
  # yearly_data[2, 3] <- 0
  # yearly_data[2, 4] <- 1
  # yearly_data[2, 5] <- 0
  #  
  # yearly_data[3, 1] <- 1
  # yearly_data[3, 2] <- 1
  # yearly_data[3, 3] <- 0
  # yearly_data[3, 4] <- 1
  # yearly_data[3, 5] <- 1
  #  
  # yearly_data[4, 1] <- 1
  # yearly_data[4, 2] <- 1
  # yearly_data[4, 3] <- 1
  # yearly_data[4, 4] <- 1
  # yearly_data[4, 5] <- 1
  # T <- 5
  
  
  ## creating a empty m-array matrix to store 
  m <- matrix(0, nrow = T-1, ncol = T)
  
  ## Calculating total number of capture in each capture occasion
  total_cap <- colSums(yearly_data)
  
  ## Storing the results in first column of m-array
  m[,1] <- total_cap[-length(total_cap)]
  
  ## Creating array for the birds that were never captured
  NCap <- rep(0, T-1)
  
  ## Below for-loop compares capture occasion i with capture occasions from i+1 
  ## till T(Last capture occasion)
  for (i in 1:(T-1)) {
    init_capidx <- yearly_data[, i]
    totalCap <- m[i, 1]
    for (j in (i+1):T) {
      cap_idx <- yearly_data[, j]
      curr_sum <- sum(init_capidx[init_capidx ==cap_idx])
      if (totalCap > 0 & curr_sum<=totalCap) {
        m[i, j] <- curr_sum
        totalCap <- totalCap - m[i, j]
      } 
      else if(totalCap>=0 & curr_sum>totalCap)
      {
        m[i,j] <- totalCap
        totalCap <- totalCap - m[i, j]
      }
      else {
        m[i, j] <- 0
      }
    }
    NCap[i] <- totalCap
  }
  ## Combining the m-array with the birds never sighted again array  
  m <- cbind(m, NCap)
  # print(m)
  # 
   ## Calculating the observed probabilities in m-array
   for (i in 1:(T-1)) {
     R <- m[i,1]
     ## Skipping if the total captured on occasion i==0 
     if(R==0){
       next
     }
     
     else{
       ## Calculating the observed probability
       m[i,-1] <- m[i,-1]/R
     }
   }
  
  
  ## returning the converted m-array probabilities
  return(m)
}

## Function to create expected m-array
expected_m_array <- function(m, phi.open_juv, phi.open_adul, p.open, T){
  
  ## Creating a empty matrix for 
  ex_m <- matrix(0, nrow = T-1, ncol = T)
  ## Getting the numbers of birds captured in each occasion from m
  ex_m[,1] <- m
  N_exm <- rep(0, T-1)
  ## Calculating the expected m-array
  for (i in 1:(T-2)) 
  {
    ex_m[i, (i+1)] <- ex_m[i,1] * p.open * (phi.open_juv[i]+phi.open_adul[i])
    for (j in (i+2):T) 
    {
      ex_m[i, j] <-  ex_m[i, (j-1)] * (1-p.open) * (phi.open_juv[j-1] + phi.open_adul[j-1])
    }
      if( m[i] - sum(ex_m[i,-1])<0){
        N_exm[i]<-0
      }
    else{
      N_exm[i] <- m[i] - sum(ex_m[i,-1]) 
      }
    }
  ## For last row
  ex_m[T-1, T] <- ex_m[T-1,1] * (phi.open_juv[T-1] + phi.open_adul[T-1]) * p.open
  N_exm[T-1] <- ex_m[T-1,1] - sum(ex_m[T-1,-1])
  
  ex_m = cbind(ex_m, N_exm)
  
  ## calculating the frequencies of the expected m-arrays
   for (i in 1:(T-1)) {
       R <- m[i]
      
      if(R==0){
        next
      }
      
      else{
        ex_m[i,-1] <- ex_m[i,-1]/R
      }
    }
  
  ## Combining expected m-array with expected never captured again
  return(ex_m)
}

## Function to calculate AIC using log_likelihood at MLE
AIC <- function(log_likelihood, n_par){
  ## Calculating the AIC statistics
  aic <- -2 * log_likelihood + 2* n_par
  ## Return AIC score for the model
  return(aic)
}
## Function to calculate BIC using log_likelihood at MLE
BIC <- function(log_likelihood, n_obs, n_par) {
  ## Calculating the BIC statistic
  bic <- -2 * log_likelihood + n_par * log(n_obs)
  ## Return BIC score for the model
  return(bic)
}

