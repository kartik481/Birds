############################ Birds project #####################################
############################ chifchaf estimates #############################
## setting the seed
set.seed(123)

## Loading the required Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(Rcapture)
library(gridExtra)
library(vcd)
library(generalhoslem)
library(ResourceSelection)

## Loading the open_population file in which functions for maximising different
## models are stored(Please change the file path accordingly)
source("~/Documents/Birds/CJSlik.R")

## Loading the chifchaf dataset
chifchaf <- read.csv("~/Documents/Birds/CR.chifchaf_FixRing.csv", sep =';'
                     ,header=TRUE)

## Printing the dimensions of the chifchaf DataFrame
cat("Dimension of chifchaf data is:", dim(chifchaf))

## Removing the first four months from the data
chifchaf <- chifchaf[, -c(1:4)]

## Checking if operation was successful
##View(chifchaf)

## Checking if there is any rows with missing data 
naRows<- colSums(is.na(chifchaf)) > 0
cat("No. of rows with missing data:", sum(naRows))

## Calculating number of zeroes in the observations
zeros <- length(which(rowSums(chifchaf[, -ncol(chifchaf)])==0))
cat("No. of zeros in data are:", zeros)

## Removing zero observations
chifchaf <- chifchaf[rowSums(chifchaf[, -ncol(chifchaf)] != 0) > 0, ]

## Getting unique value
ageRing <- unique(chifchaf$age_at_ringing)
cat("No. of unique values in age_at_ringing:",ageRing)


## Getting the rows numbers where unkown is present and removing them
rowNum <- which(chifchaf$age_at_ringing == "Unknown")
cat("Number of rows with unknown:",length(rowNum))

## Strong unknown observations in a separate dataFrame
unknownObs <- chifchaf[rowNum,]

## Removing unkonwn from the dataFrame
chifchaf <- chifchaf[-rowNum, ]




## Create separate data frames for adults and juveniles
adultschifchaf <- chifchaf[chifchaf$age_at_ringing == "adult", ]
juvenileschifchaf <- chifchaf[chifchaf$age_at_ringing == "juvenile", ]

## Printing the number of adults and juveniles
cat('No. of adults in chifchaf:', nrow(adultschifchaf),'\n')
cat('No. of juveniles in chifchaf:', nrow(juvenileschifchaf),'\n')



## Getting the number of adult and juvenile populations
juvenile_freq <- nrow(adultschifchaf)
adult_freq <- nrow(juvenileschifchaf)

## Getting the captured(1) and uncaptured(0) frequenci.chifchafes
freq_0 <- apply(chifchaf, 1, function(x) sum(x == 0))
freq_1 <- apply(chifchaf, 1, function(x) sum(x == 1))



## Removing the last column(age at ringing) from the adult and juvenile data
adultschifchaf <- adultschifchaf[, -ncol(adultschifchaf)]
juvenileschifchaf <-juvenileschifchaf[, -ncol(juvenileschifchaf)]

## Transposing the dataFrame
plotchifchafjuve <- t(juvenileschifchaf)
plotchifchafadul <- t(adultschifchaf)

## Converting all values in the data frame from string to integer
plotchifchafjuve <- as.data.frame(apply(plotchifchafjuve, 2, as.numeric))
plotchifchafadul <- as.data.frame(apply(plotchifchafadul, 2, as.numeric))


## Calculating the number of birds captured each day
plotchifchafjuve$CapSum <- rowSums(plotchifchafjuve)
plotchifchafadul$CapSum <- rowSums(plotchifchafadul)


## generating the month names from October 2007 to April 2018 
Months <- seq(as.Date("2007-10-01"), as.Date("2018-04-01"), by = "months")

# Exclude speci.chifchaffic months from the sequence
excluded_months <- grepl("May|June|July|August|September", format(Months, "%B"))

# Convert the Months vector to a sequence of month names
Months <- format(Months, format = "%B %Y")

# Create a new sequence of months that excludes the speci.chifchaffied months
Month <- Months[!excluded_months]

## Making a combined dataFrame for captured adults and juveniles
combData <- data.frame(Month = c(1:77), Juvenile = plotchifchafjuve$CapSum, 
                       Adult = plotchifchafadul$CapSum)

## Reshape the data frame to a long format to plot
combData_long <- pivot_longer(combData, cols = c(Juvenile, Adult), 
                              names_to = "Group", values_to = "Captured")

## Using ggplot to plotting the captured birds over different months
p <- ggplot(combData_long, aes(x = Month, y = Captured, color = Group)) + 
  geom_point() + geom_line() +
  xlab("Months (October 2007 to April 2018)") +
  ylab("No. of captured (per session)") +
  ggtitle("chifchaf captured over different sessions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        plot.title = element_text(hjust = 0.5))

## Defining custom labels for x-axis
custom_breaks <- seq(1, 77, by = 5)
custom_labels <- Month[custom_breaks]  
## Appending labels to the graph
p <- p + scale_x_continuous(breaks = custom_breaks, labels = custom_labels)
## Printing the plot
print(p)




n <- 77                                ## Total no initial occasions
occassions <- floor(n / 7)             ## Inter-winter period spans for 7 months
## So dividing by 7 to convert to yearly

## Storing age at initial ringing
age_ring <- chifchaf$age_at_ringing

## Removing the last column
chifchaf <- chifchaf[,-ncol(chifchaf)]

# Create a new data frame to store the yearly capture-recapture data
yearly_chifchaf <- matrix(0, nrow = nrow(chifchaf), ncol = occassions)

## This loop sum over first 7 months, then 7... till last occasion and store it 
## in new matrix
for(j in 1:nrow(chifchaf)){
  for (i in 1:occassions) {
    start_idx <- (i - 1) * 7 + 1
    end_idx <- i * 7
    prod <- chifchaf[j, start_idx : end_idx]
    total <- sum(prod)
    yearly_chifchaf[j, i] <- total
  }
}




## Since the data should be binary putting 1 in places where birds are recaptured
## otherwise 0
for(k in 1:nrow(yearly_chifchaf)){
  for(j in 1:ncol(yearly_chifchaf)){
    if(yearly_chifchaf[k,j]!=0){
      yearly_chifchaf[k,j]=1
    }
  }
}
## Getting the total number of birds captured yearly
cat('Birds captured over different inter-winter sessions','\n')
cat(colSums(yearly_chifchaf))



n <- nrow(yearly_chifchaf)            ## Total no of birds
T <- ncol(yearly_chifchaf)            ## Total no. of capture occasions
theta.open <- rnorm(T)                ## Initialising the parameters



## Getting MLE for theta for open population
theta.open.chifchaf.cons <- log.mle.open(theta.open, yearly_chifchaf)


##################### Age dependent CJS-model ##################################
##################### with constant recapture ##################################
## creating a age matrix to store age according to time
age <- matrix(0, nrow = nrow(chifchaf), ncol = ncol(yearly_chifchaf))

## Lopping over the data, if age at ringing at juvenile at first then keeping
## that time 1 otherwise after that it's gonna adult so adding 2 to show bird has
## become adult 
for(i in 1:nrow(yearly_chifchaf)){
  age_at_i <- age_ring[i]
  if(age_at_i=='adult'){
    first_cap <- which(yearly_chifchaf[i,]==1)[1]
    age[i, first_cap:occassions] <- age[i, first_cap:occassions]+2
    
  }
  else if (age_at_i=='juvenile'){
    first_cap <- which(yearly_chifchaf[i,]==1)[1]
    age[i, first_cap] <- 1
    if(first_cap+1>11){
      next
    }
    age[i, (first_cap+1):occassions] <- age[i, (first_cap+1):occassions]+2
    
  }
}

## Following code getting age for the birds based on monthly data ##############
# for(i in 1:nrow(chifchaf)){
#   age_at_i <- age_ring[i]
#   if(age_at_i=='adult'){
#     first_cap <- which(chifchaf[i,]==1)[1]
#     age[i, first_cap:ncol(chifchaf)] <- age[i, first_cap:ncol(chifchaf)]+2
#     
#   }
#   else if (age_at_i=='juvenile'){
#     first_cap <- which(chifchaf[i,]==1)[1]
#     if(first_cap+1>78){
#       next
#     }
#     else if(first_cap+6>=77){
#       age[i, first_cap:ncol(chifchaf)] <- age[i, first_cap:ncol(chifchaf)]+1
#     }
#     else{
#       if(first_cap+7<=77){
#         age[i, first_cap:(first_cap+6)]<-age[i, first_cap:(first_cap+6)]+1
#         age[i, (first_cap+7):ncol(chifchaf)]<-age[i, (first_cap+7):ncol(chifchaf)]+2
#       }
#     }
#   }
# }

## viewing the age matrix
#View(age)




n <- nrow(yearly_chifchaf)            ## Total no of birds
T <- ncol(yearly_chifchaf)            ## Total no. of capture occasions
theta.open <- rep(0.5, T+1)           ## Initialising the parameters


## Getting MLE for theta for open population
theta.open.chifchaf.p.cons <- log.mle.open.age.p.cons(theta.open, yearly_chifchaf, age)



######################### Varying recapture probability #########################


n <- nrow(yearly_chifchaf)            ## Total no of birds
T <- ncol(yearly_chifchaf)            ## Total no. of capture occasions
theta.open <- rep(0.1, 22)           ## Initialising the parameters


## Getting MLE for theta for open population
theta.open.chifchaf.var <- log.mle.open.age.p.var(theta.open, yearly_chifchaf, 
                                                  age)


##################### Relative goodness of fit tests ###########################


## creating empty arrays to store initial and final capture occasions
f <- rep(0, n)
l <- rep(0, n)
## Stores first individual is observed
for (i in 1:n){f[i] <- which(yearly_chifchaf[i,]==1)[1]}
## Storing the last time individual is observed
for (i in 1:n){l[i] <- which(yearly_chifchaf[i,]==1)[length(which(yearly_chifchaf[i,]==1))]}


## Caluclating the vale of log_likeihood at MLE
log_lik <- CJSlik(theta.open.chifchaf.cons, yearly_chifchaf, f, l, n, T)
## Getting the AIC 
AIC_cons <- AIC(log_lik, 10)
cat("AIC for age independent model:", AIC_cons)
## Getting the BIC 
BIC_cons <- BIC(log_lik, nrow(yearly_chifchaf), 10)
cat("BIC for age dependent and constant recapture probability are:", BIC_cons)



## Caluclating the value of log_likeihood at MLE for constant recapture and 
## age dependence
log_lik_age <- CJSlik.age.cons(theta.open.chifchaf.p.cons, yearly_chifchaf, f, l, n, age, T)
## Getting the AIC 
AIC_age <- AIC(log_lik_age, 10)
cat("AIC for age dependent model:", AIC_age)
## Getting the BIC 
BIC_age <- BIC(log_lik_age, nrow(yearly_chifchaf), 10)
cat("BIC for age dependent and constant recapture probability are:", BIC_age)


## Caluclating the vale of log_likeihood at MLE
log_lik_age.var <- CJSlik.age.var(theta.open.chifchaf.var, yearly_chifchaf, f, 
                                  l, n,  age, T)
## Getting the AIC 
AIC_age_p <- AIC(log_lik_age.var, 20)
cat("AIC for age dependent and varying recapture probability are:", AIC_age_p)

## Getting the BIC 
BIC_age_p <- BIC(log_lik_age.var, nrow(yearly_chifchaf), 20)
cat("BIC for age dependent and varying recapture probability are:", BIC_age_p)


################## Getting the parameters based on best model ##################


# Getting the parameters estimates
param.open.chifchaf <- popEstimate.open(theta.open.chifchaf.p.cons, T, 1)

## Extracting the paramters
phi.open_juve.chifchaf <- param.open.chifchaf[[1]]
phi.open_adul.chifchaf <- param.open.chifchaf[[2]]
p.open.chifchaf <- param.open.chifchaf[[3]]

## Printing the results
cat('The constant recapture probability is: \n')
cat(p.open.chifchaf,'\n')
cat('Juvenile survival probability is: \n')
cat(phi.open_juve.chifchaf,'\n')
cat('Adult survival probability is: \n')
cat(phi.open_adul.chifchaf,'\n')



###################### Absolute goodness of fit test ###########################

## Calculating m-array probabilities for observed data
marray <- m_array(yearly_chifchaf, T)


## Total number of birds captued in each occassion
R <- unlist(colSums(yearly_chifchaf)[-T])
## Calculating the expected probabilities for juveniles and adults
ex_m <- expected_m_array(R ,phi.open_juve.chifchaf, phi.open_adul.chifchaf, 
                         p.open.chifchaf, T)

## Removing the first column from observed and expected m-arrays
marray <- marray[,-1]
ex_m <- ex_m[,-1]

## Doing a pearson chi-square test and calculating p-value using monte-carlo
## simulation 
chi_square_test.open <- chisq.test(table(marray, ex_m), 
                                   simulate.p.value = TRUE)
## According to multinomial assumption degrees of freedom can be defined as 
## df = k-1-d (where k=No of multinomial cells, d=Number of estimated parameters)
alpha<- 0.05
df <- 47           
critical_value <- qchisq(1 - alpha, df)
cat("Critical value is:", critical_value)
## Printing the results
chi_square_test.open


## Fisher test for chifchaf dataset
fisher.test(table(marray, ex_m), simulate.p.value = TRUE)
idx <- 1

## There are (T-1)*(T+2)/2 multinomial bins so creating vector of that length to 
## store those values for observed m-array probabilities
select_bin_m <- rep(0, (T-1)*(T+2)/2)
for (i in 1:(T-1)) {
  for (j in (i):T){
    select_bin_m[idx]<- marray[i,j]
    idx<- idx+1
  }
}
## Doing the same thing for expected probabilities
idx <- 1
select_bin_exm <- rep(0, (T-1)*(T+2)/2)
for (i in 1:(T-1)) {
  for (j in (i):T){
    select_bin_exm[idx]<- ex_m[i,j]
    idx<- idx+1
  }
}

## hosmer test for absolute goodness of fit test
hoslem.test(select_bin_m, select_bin_exm, g=11)

############################ Confidence intervals ##############################
## Number of non-parameteric bootstrap samples to take
n_bootstrap <- 500

## Total Number of observed individuals
total_n <- nrow(yearly_chifchaf)

## Number of parameters to estimate 
n_params <- T+1
## Initialize a matrix to store the bootstrap parameter estimates
bootstrap_estimates <- matrix(0, nrow = n_bootstrap, ncol = n_params)
sample_size <- nrow(yearly_chifchaf)
## Perform bootstrapping
for (j in 1:n_bootstrap) {
  indices <- sample(1:sample_size, size = sample_size, replace = TRUE)
  ## Generating a bootstrap sample by resampling from the original data
  bootstrap_sample <- yearly_chifchaf[indices, ]
  ## Initialize the parameter values for optimization
  theta_init <- theta.open.chifchaf.p.cons
  
  bootstrap_mle <- log.mle.open.age.p.cons(theta_init, yearly_chifchaf[indices,], 
                                           age[indices,])
  ## Storing the results in the i-th row
  bootstrap_estimates[j, ] <- unlist(bootstrap_mle)
}

## Calculating the bootstrap intervals and mean for the estimates
bootstrap_lower <- apply(bootstrap_estimates, 2, quantile, probs = 0.025)
bootstrap_mean <- apply(bootstrap_estimates, 2, mean, type =7)
bootstrap_upper <- apply(bootstrap_estimates, 2, quantile, probs = 0.975)


## Returning the bootstrap intervals
ci.chifchaf <- list(lower = bootstrap_lower, Est = bootstrap_mean, upper = bootstrap_upper)

## Getting the estimate for lower confidence intervals
param.open.lower <- popEstimate.open(ci.chifchaf$lower, T, 1)
param.open.juve.lower <- param.open.lower[[1]]
param.open.adul.lower <- param.open.lower[[2]]
p.cons.lower <-  param.open.lower[[3]]

## Getting the estimate for upper confidence intervals
param.open.upper <- popEstimate.open(ci.chifchaf$upper, T, 1)
param.open.juve.upper <- param.open.upper[[1]]
param.open.adul.upper <- param.open.upper[[2]]
p.cons.upper <-  param.open.upper[[3]]

## Printing the CI for recapture probability
print(p.cons.lower)
print(p.cons.upper)




## Create a data frame for the juvenile and adult data with upper 95% CI and
## lower 95% CI
juvenile_data <- data.frame(
  Session = c("2007-2008", "2008-2009", "2009-2010", "2010-2011", "2011-2012", 
              "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017"),
  Estimate = phi.open_juve.chifchaf,
  CI_Lower = param.open.juve.lower,
  CI_Upper = param.open.juve.upper
)

adult_data <- data.frame(
  Session = c("2007-2008", "2008-2009", "2009-2010", "2010-2011", "2011-2012", 
              "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017"),
  Estimate = phi.open_adul.chifchaf,
  CI_Lower = param.open.adul.lower,
  CI_Upper = param.open.adul.upper
)


# Set the common y-axis limits
y_axis_limits <- c(0, 1)

## Printing the plot for juveniles with 95% CI
p <- ggplot(juvenile_data, aes(x = Session, y = Estimate, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = "95% CI"), 
                width = 0.2) +
  geom_text(aes(label = sprintf("%.3f", Estimate)), vjust = -0.5,
            position = position_nudge(x = +0.45)) +
  labs(x = "Session", y = "Survival Probability", title = "Survival Probability of Juveniles") +
  theme_minimal() +  ylim(y_axis_limits) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5),  # Center-align plot title
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12)) + 
  guides(color = guide_legend(title = "Legend"))  # Adjust x-axis text size

# Print the plot
print(p)

## Printing the plot for adults with 95% CI
p <- ggplot(adult_data, aes(x = Session, y = Estimate, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = "95% CI"), 
                width = 0.2) +
  geom_text(aes(label = sprintf("%.3f", Estimate)), vjust = -0.5,
            position = position_nudge(x = +0.45)) +
  labs(x = "Session", y = "Survival Probability", title = "Survival Probability of Adults") +
  theme_minimal() +  ylim(y_axis_limits) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5),  # Center-align plot title
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12)) + 
  guides(color = guide_legend(title = "Legend"))  # Adjust x-axis text size

# Print the plot
print(p)

