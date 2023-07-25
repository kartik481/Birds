## setting the seed
set.seed(123)
## Printing message to confirm if we have linked files successfully
cat('Loaded functions open population file successfully')

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
  
  # Calculate the (log-)likelihood component for observed individual capture histories
  
  for (i in 1:n){
    
    # Consider the times between initial capture and final capture for each individual
    # Contribution only required when l[i] is not equal to f[i]
    
    if (f[i] != l[i]){
    
      for (t in f[i]:(l[i]-1)){
        lik[i] <- lik[i] + log(phi[t]) + x[i,t+1]*log(p) + (1-x[i,t+1])*log(1-p)
      }
    }
    
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
  
  return(-sumlik)
}

## Function to maximize the log-likelihood to obtain MLE 
log.mle.open <- function(theta, x, T){
  ## Stored 
  n <- nrow(x)
  f <- rep(0, n)
  l <- rep(0, n)
  ## Stores first indivdual is observed
  for (i in 1:n){f[i] <- which(x[i,]==1)[1]}
  ## Storing the last time individual is observed
  for (i in 1:n){l[i] <- which(x[i,]==1)[length(which(x[i,]==1))]}
  
  ## 
  diff <- 1
  eps <- 1e-3
  while(diff > eps){
    theta.old <- theta
  res <- optim(par = theta.old, fn = CJSlik, x=x, 
    n=n, f=f, l=l, T=T)
  theta <- res$par
  diff <- sum(abs(theta-theta.old))
  }
 
  return(theta)
}

## Function to get parameter estimate based on MLE
popEstimate.open <- function(theta, T){
  
  phi <- exp(theta[1:(T-1)])/(1+exp(theta[1:(T-1)]))
  p <- exp(theta[T])/(1+exp(theta[T]))
  chi <- array(1,T)
  
  for (t in (T-1):1){
    chi[t] <- 1 - phi[t] + phi[t]*(1-p)*chi[t+1]
  }
  
  return(p)
}

## Function for calculating the survival probabilities
surProb.open <- function(theta, T){
  
  phi <- exp(theta[1:(T-1)])/(1+exp(theta[1:(T-1)]))
  p <- exp(theta[T])/(1+exp(theta[T]))
  chi <- array(1,T)
  
  for (t in (T-1):1){
    chi[t] <- 1 - phi[t] + phi[t]*(1-p)*chi[t+1]
  }
  
  # Calculate survival probabilities
  survivalProb <- array(0, T)
  
  for (t in 1:(T-1)) {
    survivalProb[t] <- (1-chi[t])/p[t+1]*(1-p[t+1])*(1-chi[t+1])
  }
  
  # Print the survival probabilities
  return(survivalProb)
}

# Function to perform bootstrapping and calculate bootstrap intervals
bootstrap_intervals.open <- function(data, T, n_bootstrap) {
  # data: Capture-recapture data
  # T: Number of capture occasions
  # n_bootstrap: Number of bootstrap samples
  
  # Set the seed for reproducibility
  set.seed(123)
  
  # Number of observed individuals
  n <- nrow(data)
  
  # Number of parameters (p[1:T], log N)
  n_params <- T + 1
  
  # Initialize a matrix to store the bootstrap parameter estimates
  bootstrap_estimates <- matrix(0, nrow = n_bootstrap, ncol = n_params)
  
  # Perform bootstrapping
  for (i in 1:n_bootstrap) {
    # Generate a bootstrap sample by resampling from the original data
    bootstrap_sample <- data[sample(n, replace = TRUE), ]
    
    # Initialize the parameter values for optimization
    theta_init <- runif(n_params)
    
    # Estimate the MLE using the bootstrap sample
    bootstrap_mle <- log.mle.open(theta = theta_init, data = bootstrap_sample, 
                                  T = T)
    
    # Store the parameter estimates
    bootstrap_estimates[i, ] <- bootstrap_mle
  }
  
  # Calculate the bootstrap intervals
  bootstrap_lower <- apply(bootstrap_estimates, 2, quantile, probs = 0.025)
  bootstrap_upper <- apply(bootstrap_estimates, 2, quantile, probs = 0.975)
  
  # Return the bootstrap intervals
  list(lower = bootstrap_lower, upper = bootstrap_upper)
}