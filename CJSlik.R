## setting the seed
set.seed(123)
## Printing message to confirm if we have linked files successfully
cat('Loaded functions open population file successfully')

CJSlik <- function(theta, x, f, l, n, age, T) {
  
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
  a <- theta[1:(T-1)]
  b <- theta[T]
  phi <- exp(a)/(1+exp(a))
  p <- exp(theta[T+1])/(1+exp(theta[T+1]))
  
  
  # Initialise the log-likelihood value for each individual:
  
  lik <- array(0,n)
  
  # Calculate the (log-)likelihood component for observed individual capture histories
  
  for (i in 1:n){
    
    # Consider the times between initial capture and final capture for each individual
    # Contribution only required when l[i] is not equal to f[i]
    
    if (f[i] != l[i]){
    
      for (t in f[i]:(l[i]-1)){
        if(age[i,t]==1){
          phi[t] <- exp(a[t])/(1+exp(a[t]))
        }
        else{
          phi[t] <- exp(a[t] + b)/(1+exp(a[t] + b))
        }
        lik[i] <- lik[i] + log(phi[t]) + x[i,t+1]*log(p) + (1-x[i,t+1])*log(1-p)
      }
    }
    
    # Note the above function assumes that we do not have any observed
    # histories with a single observation at the final capture occasion
    # I.e. we no not have any histories such that f = l = T. 
    # For such histories there is no information (and so can be omitted).
    
    # Calculate the chi terms: probability of not observed after time t
    # Initially set up chi to be an array of length T all elements equal to 1
    # chi[T] <- 1
    # Calculate chi[1:T-1] using recursive formula
    
    chi <- array(1,T)
    
    for (t in (T-1):1){
      chi[t] <- 1 - phi[t] + phi[t]*(1-p)*chi[t+1]
    }
    
    
    
    # Add chi terms (probability of not being observed after time l[i])
    
    lik[i] <- lik[i] + log(chi[l[i]])
    
  }
  
# Calculate log-likelihood over all individuals:
  
  sumlik <- sum(lik)
  
  # Output the log-likelihood value:
  
  return(-sumlik)
}

## Function to maximize the log-likelihood to obtain MLE 
log.mle.open <- function(theta, x, age){
  ## Getting the number of observations
  n <- nrow(x)
  T <- ncol(x)
  ## creating empty arrays to store initial and final capture occasions
  f <- rep(0, n)
  l <- rep(0, n)
  ## Stores first individual is observed
  for (i in 1:n){f[i] <- which(x[i,]>=1)[1]}
  ## Storing the last time individual is observed
  for (i in 1:n){l[i] <- which(x[i,]>=1)[length(which(x[i,]>=1))]}
  ## Setting initial difference between old and estimate observed 
  diff <- 1
  ## Setting the tolerance value between new and old parameters
  eps <- 1e-3
  
  ## performing optim till diff becomes less than eps
  while(diff > eps){
    ## storing the estimate to check the difference 
    theta.old <- theta
    
    ## Using the optim to get MLE
    res <- optim(par = theta.old, fn = CJSlik, x=x, 
    n=n, f=f, l=l, age=age, T=T)
    
    ## Storing the estimate MLEs
    theta <- res$par
    ## updating the absolute difference between old and new estimates
    diff <- sum(abs(theta-theta.old))
  }
  ## Returning the estimated parameters 
  return(theta)
}

## Function to get parameter estimate based on MLE
popEstimate.open <- function(theta, T){
  ## Getting the alpha and beta from the estimated parameter theta
  alpha <- theta[1:(T-1)]
  beta <- theta[T]
  
  ## Calculating the juvenile and adult survival probabilities based on 
  ## logit phi = a (for juvenile) and for adult logit phi = alpha + beta
  phi_juve <- exp(alpha)/(1+exp(alpha))
  phi_adul <- exp(alpha + beta)/(1+exp(alpha + beta)) 
  
  ## Getting the constant recapture probability
  p <- exp(theta[T+1])/(1+exp(theta[T+1]))
  
  ## Returining the parameters in a list format
  return(list(phi_juve, phi_adul, p))
}

## Function to perform bootstrapping and calculate bootstrap intervals
bootstrap_intervals.open <- function(data, T, n_bootstrap, age) {
  ## data: Capture-recapture data
  ## T: Number of capture occasions
  ## n_bootstrap: Number of bootstrap samples
  
  ## Set the seed for reproducibility
  set.seed(123)
  
  ## Total Number of observed individuals
  n <- nrow(data)
  
  ## Number of parameters to estimate 
  n_params <- T+1
  ## Initialize a matrix to store the bootstrap parameter estimates
  bootstrap_estimates <- matrix(0, nrow = n_bootstrap, ncol = n_params)
  
  ## Perform bootstrapping
  for (j in 1:n_bootstrap) {
    ## Generate a bootstrap sample by resampling from the original data
    bootstrap_sample <- data[sample(n, replace = TRUE), ]
    ## Initialize the parameter values for optimization
    theta_init <- runif(n_params)
    
    ## creating empty arrays to store initial and final capture occasions
    #f <- rep(0, n)
    #l <- rep(0, n)
    ## Stores first individual is observed
    #for (i in 1:n){f[i] <- which(data[i,]>=1)[1]}
    ## Storing the last time individual is observed
    #for (i in 1:n){l[i] <- which(data[i,]>=1)[length(which(data[i,]>=1))]}
    # Estimate the MLE using the bootstrap sample
    #bootstrap_mle <- optim(par = theta_init, fn = CJSlik, x=bootstrap_sample, 
                    #n=n, f=f, l=l, age=age, T=T, control=list(maxit = 5000))
    
    ## Getting the estimate for bootstrapped sample
    bootstrap_mle <- log.mle.open(theta_init, bootstrap_sample, age)
    
    ## Storing the results in the i-th row
    bootstrap_estimates[j, ] <- unlist(bootstrap_mle)
  }
  
  ## Calculating the bootstrap intervals and mean for the estimates
  bootstrap_lower <- apply(bootstrap_estimates, 2, quantile, probs = 0.025)
  mean <- apply(bootstrap_estimates, 2, mean)
  bootstrap_upper <- apply(bootstrap_estimates, 2, quantile, probs = 0.975)
  
  ## Returning the bootstrap intervals
  list(lower = bootstrap_lower, mean = mean, upper = bootstrap_upper)
}