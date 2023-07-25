## setting the seed
set.seed(123)

## Printing message to confirm if we have linked files successfully
cat('Loaded functions closed population file successfully')

closedlik <- function(theta, x, n, T) {
  
  # The data are stored in the array x; 
  # n = number of observed individuals; T = number of capture occasions
  # Theta stores the set of parameter values - specified on the real line.
  # Define the parameter values in terms of capture probabs and population size.
  # Use the transformations: logit p[1:T] = theta[1:T]; log N = theta[T+1]
  
  p <- exp(theta[1:T])/(1+exp(theta[1:T]))
  unobs <- exp(theta[T+1])
  N <- unobs + n
  # Initialise the log-likelihood value:
  
  lik <- 0
  
  # Calculate the (log-)likelihood component for observed individual capture histories
  
  for (i in 1:n){
    for (t in 1:T){
      lik <- lik + x[i,t]*log(p[t]) + (1-x[i,t])*log(1-p[t])
    }
  }
  
  # Calculate the (log) probability of not being observed within the study  
  
  noprob <- sum(log(1-p[]))
  
  # Add the log-likelihood contribution of the probability of unobserved individuals
  
  lik <- lik + (N-n)*noprob
  
  # Add the Multinomial coefficient likelihood component:
  
  lik <- lik + lgamma(N+1) - lgamma(N-n+1)
  # Output
  return(-lik)
}

## maximising the log-likelihood to obtain MLE 
log.mle.closed <- function(theta, data, T){
    res <- optim(par = theta, fn=closedlik, x=data, 
                 n=nrow(data), T=T, method='Nelder-Mead') 
    if (res$convergence == 0) {
      cat("Optimization converged.\n")
    } else {
      cat("Optimization did not converge.\n")
    }
    theta <- res$par
  return(theta)
}



## Function to get parameter estimate based on MLE
popEstimate <- function(theta){
  
  p <- exp(theta[1:T])/(1+exp(theta[1:T]))
  unobs <- exp(theta[T+1])
  N <- unobs + n

  #cat("Capture probabilities are:", p, '\n')
  cat("Total population is:", N)
  return(p)
}

## Function for calculating the survival probabilities
surProb <- function(p){
   # Calculate survival probabilities
   survivalProb <- 1 - p 

   # Print the survival probabilities
   return(survivalProb)
}


# Function to perform bootstrapping and calculate bootstrap intervals
bootstrap_intervals.closed <- function(data, T, n_bootstrap) {
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
    theta_init <- rnorm(n_params)
    
    # Estimate the MLE using the bootstrap sample
    bootstrap_mle <- log.mle.closed(theta = theta_init, data = bootstrap_sample, T = T)
    
    # Store the parameter estimates
    bootstrap_estimates[i, ] <- bootstrap_mle
  }
  
  # Calculate the bootstrap intervals
  bootstrap_lower <- apply(bootstrap_estimates, 2, quantile, probs = 0.025)
  bootstrap_upper <- apply(bootstrap_estimates, 2, quantile, probs = 0.975)
  
  # Return the bootstrap intervals
  list(lower = bootstrap_lower, upper = bootstrap_upper)
}