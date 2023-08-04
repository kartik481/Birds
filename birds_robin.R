############################ Birds project #####################################

## setting the seed
set.seed(123)

## Loading the Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(Rcapture)
library(gridExtra)
library(vcd)
library(generalhoslem)

## Loading the closed_population file
source("~/Documents/Birds/closedlik.R")
## Loading the open_population file
source("~/Documents/Birds/CJSlik.R")

## Loading the robin dataset
robin <- read.csv("~/Documents/Birds/CR.robin_FixRing.csv", sep =';'
                     ,header=TRUE)

## Printing the dimensions of the robin DataFrame
cat("Dimension of robin data is:", dim(robin))

## Removing the first four months from the data
robin <- robin[, -c(1:4)]

## Checking if operation was successful
##View(robin)

## Checking if there is any rows with missing data 
naRows<- colSums(is.na(robin)) > 0
cat("No. of rows with missing data:", sum(naRows))

## Calculating number of zeroes in the observations
zeros <- length(which(rowSums(robin[, -ncol(robin)])==0))
cat("No. of zeros in data are:", zeros)

## Removing zero observations
robin <- robin[rowSums(robin[, -ncol(robin)] != 0) > 0, ]

## Getting unique value
ageRing <- unique(robin$age_at_ringing)
cat("No. of unique values in age_at_ringing:",ageRing)


## Getting the rows numbers where unkown is present and removing them
rowNum <- which(robin$age_at_ringing == "Unknown")
cat("Number of rows with unknown:",length(rowNum))

## Strong unknown observations in a separate dataFrame
unknownObs <- robin[rowNum,]

## Removing unkonwn from the dataFrame
robin <- robin[-rowNum, ]




## Create separate data frames for adults and juveniles
adultsrobin <- robin[robin$age_at_ringing == "adult", ]
juvenilesrobin <- robin[robin$age_at_ringing == "juvenile", ]

## Printing the number of adults and juveniles
cat('No. of adults in robin:', nrow(adultsrobin),'\n')
cat('No. of juveniles in robin:', nrow(juvenilesrobin),'\n')



## Getting the number of adult and juvenile populations
juvenile_freq <- nrow(adultsrobin)
adult_freq <- nrow(juvenilesrobin)

## Getting the captured(1) and uncaptured(0) frequenci.robines
freq_0 <- apply(robin, 1, function(x) sum(x == 0))
freq_1 <- apply(robin, 1, function(x) sum(x == 1))



## Removing the last column(age at ringing) from the adult and juvenile data
adultsrobin <- adultsrobin[, -ncol(adultsrobin)]
juvenilesrobin <-juvenilesrobin[, -ncol(juvenilesrobin)]

## Transposing the dataFrame
plotrobinjuve <- t(juvenilesrobin)
plotrobinadul <- t(adultsrobin)

## Converting all values in the data frame from string to integer
plotrobinjuve <- as.data.frame(apply(plotrobinjuve, 2, as.numeric))
plotrobinadul <- as.data.frame(apply(plotrobinadul, 2, as.numeric))


## Calculating the number of birds captured each day
plotrobinjuve$CapSum <- rowSums(plotrobinjuve)
plotrobinadul$CapSum <- rowSums(plotrobinadul)


## generating the month names from October 2007 to April 2018 
Months <- seq(as.Date("2007-10-01"), as.Date("2018-04-01"), by = "months")

# Exclude speci.robinfic months from the sequence
excluded_months <- grepl("May|June|July|August|September", format(Months, "%B"))

# Convert the Months vector to a sequence of month names
Months <- format(Months, format = "%B %Y")

# Create a new sequence of months that excludes the speci.robinfied months
Month <- Months[!excluded_months]

## Making a combined dataFrame for captured adults and juveniles
combData <- data.frame(Month = c(1:77), Juvenile = plotrobinjuve$CapSum, 
                       Adult = plotrobinadul$CapSum)

## Reshape the data frame to a long format to plot
combData_long <- pivot_longer(combData, cols = c(Juvenile, Adult), 
                              names_to = "Group", values_to = "Captured")

## Using ggplot to plotting the captured birds over different months
p <- ggplot(combData_long, aes(x = Month, y = Captured, color = Group)) + 
  geom_point() + geom_line() +
  xlab("Months (October 2007 to April 2018)") +
  ylab("No. of captured (per session)") +
  ggtitle("Robin captured over different sessions") +
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
age_ring <- robin$age_at_ringing

## Removing the last column
robin <- robin[,-ncol(robin)]

# Create a new data frame to store the yearly capture-recapture data
yearly_robin <- matrix(0, nrow = nrow(robin), ncol = occassions)

## This loop sum over first 7 months, then 7... till last occasion and store it 
## in new matrix
for(j in 1:nrow(robin)){
  for (i in 1:occassions) {
    start_idx <- (i - 1) * 7 + 1
    end_idx <- i * 7
    prod <- robin[j, start_idx : end_idx]
    product <- sum(prod)
    yearly_robin[j, i] <- product
  }
}
## Getting the total number of birds captured yearly
cat('Birds captured over different inter-winter sessions','\n')
cat(colSums(yearly_robin))


n <- nrow(yearly_robin)            ## Total no of birds
T <- ncol(yearly_robin)            ## Total no. of capture occasions
theta.open <- rnorm(T)           ## Initialising the parameters



## Getting MLE for theta for open population
theta.open.robin.cons <- log.mle.open(theta.open, yearly_robin)

## creating empty arrays to store initial and final capture occasions
f <- rep(0, n)
l <- rep(0, n)
## Stores first individual is observed
for (i in 1:n){f[i] <- which(yearly_robin[i,]>=1)[1]}
## Storing the last time individual is observed
for (i in 1:n){l[i] <- which(yearly_robin[i,]>=1)[length(which(yearly_robin[i,]>=1))]}
## Caluclating the vale of log_likeihood at MLE
log_lik <- CJSlik(theta.open.robin.cons, yearly_robin, f, l, n, T)
## Getting the AIC 
AIC_cons <- AIC(log_lik, 11)
cat("AIC for age independent model:", AIC_cons)



##################### Introduci.robinng age dependent CJS-model ######################

## creating a age matrix to store age according to time
age <- matrix(0, nrow = nrow(robin), ncol = occassions)

## Lopping over the data, if age at ringing at juvenile at first then keeping
## that time 1 otherwise after that it's gonna adult so adding 2 to show bird has
## become adult 
for(i in 1:nrow(yearly_robin)){
  age_at_i <- age_ring[i]
  if(age_at_i=='adult'){
    first_cap <- which(yearly_robin[i,]>=1)[1]
    age[i, first_cap:occassions] <- age[i, first_cap:occassions]+2
    
  }
  else if (age_at_i=='juvenile'){
    first_cap <- which(yearly_robin[i,]>=1)[1]
    age[i, first_cap] <- 1
    if(first_cap+1>11){
      next
    }
    age[i, (first_cap+1):occassions] <- age[i, (first_cap+1):occassions]+2
    
  }
}

## Printing the age matrix
print(age)


## Getting counts for juveniles and adults in each occassion
get_counts <- function(col) {
  table(col)
}

## Apply the function to each column of the age matrix
age_counts <- apply(age, 2, get_counts)



n <- nrow(yearly_robin)            ## Total no of birds
T <- ncol(yearly_robin)            ## Total no. of capture occasions
theta.open <- rnorm(T+1)           ## Initialising the parameters


## Getting MLE for theta for open population
theta.open.robin <- log.mle.open.age(theta.open, yearly_robin, age)

## creating empty arrays to store initial and final capture occasions
f <- rep(0, n)
l <- rep(0, n)
## Stores first individual is observed
for (i in 1:n){f[i] <- which(yearly_robin[i,]>=1)[1]}
## Storing the last time individual is observed
for (i in 1:n){l[i] <- which(yearly_robin[i,]>=1)[length(which(yearly_robin[i,]>=1))]}
## Caluclating the vale of log_likeihood at MLE
log_lik_age <- CJSlik_age(theta.open.robin, yearly_robin, f, l, n, age, T)
## Getting the AIC 
AIC_age <- AIC(log_lik_age, 12)
cat("AIC for age dependent model:", AIC_age)

## Getting the parameters estimates
param.open.robin <- popEstimate.open(theta.open.robin, T)

## Extracting the paramters
p.open.robin <- param.open.robin[[3]]
phi.open_juve.robin <- param.open.robin[[1]]
phi.open_adul.robin <- param.open.robin[[2]]

## Printing the results
cat('Constant recapture probability is: \n')
cat(p.open.robin,'\n')
cat('Juvenile survival probability is: \n')
cat(phi.open_juve.robin,'\n')
cat('Adult survival probability is: \n')
cat(phi.open_adul.robin,'\n')

###################### Absolute goodness of fit test ###########################


# juve_year <- matrix(0, nrow = n, ncol = T)
# for(i in 1:n){
#   for(j in 1:T){
#     if(yearly_robin[i,j]>0 & age[i,j]==1){
#       juve_year[i,j] <- yearly_robin[i,j]
#     }
#   }
# }
# 
# adul_year <- matrix(0, nrow = n, ncol = T)
# for(i in 1:n){
#   for(j in 1:T){
#     if(yearly_robin[i,j]>0 & age[i,j]==2){
#       adul_year[i,j] <- yearly_robin[i,j]
#     }
#   }
# }

## Calculating m-array probabilities for observed based on multinational assumption
marray <- m_array(yearly_robin, T)


## Total number of birds captued in each occassion
R <- unlist(colSums(yearly_robin)[-T])
## Calculating the expected probabilities for juveniles and adults
ex_m <- expected_m_array(R ,phi.open_juve.robin, phi.open_adul.robin, p.open.robin, T)

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


## Fisher test for robin dataset
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


## hosmer test for 11 different capture occassions
logitgof(select_bin_m, select_bin_exm, g = 11, ord = FALSE)




############################ Confidence intervals ##############################
## Calculating 95% ci.robin for juveniles population for 300 samples
ci.robin <- bootstrap_intervals.open(theta.open.robin, yearly_robin, T, 1000, age)
cat("95% ci.robin for total popultion is:")
ci.robin

## getting estimate for mean estimate
param.open <- popEstimate.open(ci.robin[[2]], T)
param.open.juve <- param.open[[1]]
param.open.adul <- param.open[[2]]
p.cons <-  param.open[[3]]

## Getting the estimate for lower confidence intervals
param.open.lower <- popEstimate.open(ci.robin$lower, T)
param.open.juve.lower <- param.open.lower[[1]]
param.open.adul.lower <- param.open.lower[[2]]
p.cons.lower <-  param.open.lower[[3]]

## Getting the estimate for upper confidence intervals
param.open.upper <- popEstimate.open(ci.robin$upper, T)
param.open.juve.upper <- param.open.upper[[1]]
param.open.adul.upper <- param.open.upper[[2]]
p.cons.upper <-  param.open.upper[[3]]

## Printing the CI for recapture probability
print(p.cons.lower)
print(p.cons.upper)



## Creating a data frame to store the parameter estimates and corresponding 
## confidence intervals for juveniles
parameter_df.juve <- data.frame(Estimate = param.open.juve ,
                           ci.robin_lower = param.open.juve.lower,
                           ci.robin_upper = param.open.juve.upper)
## Printing the resulted dataframe
parameter_df.juve

# Creating a data frame to store the parameter estimates and corresponding 
## confidence intervals for adults
parameter_df.adul <- data.frame(Estimate = param.open.adul,
                                ci.robin_lower = param.open.adul.lower,
                                ci.robin_upper = param.open.adul.upper)
## Printing the dataframe
parameter_df.adul
## Plotting the survivial probabilities with error bars

ggplot(parameter_df, aes(x = as.factor(1:10), y = )) +
  geom_violin(trim=FALSE)+geom_boxplot(width = 0.5, position = position_dodge(width = 0.75), color = "black", alpha = 0.5) +
  geom_pointrange(aes(ymin = ci.robin_lower, ymax = ci.robin_upper), width = 0.2, position = position_dodge(width = 0.75), color = "red") +
  labs(x = "Parameter", y = "Estimate") +
  ggtitle("Violin Plots of Parameter Estimates with 95% Confidence Intervals")


############################ Closed population model ##########################

#theta.closed <- runif(T+1)
## Getting MLE for theta
#theta.closed <- log.mle.closed(theta.closed, yearly_robin, T)

## Getting the parameters estimates
#p.closed <- popEstimate(theta.closed)

## Calculating the suffici.robinent statistics for obsevred data 
#observed.closed <-  colSums(yearly_robin)

## Calculating expected individuals monthly
## Calculating the expected captures in a year
#total_individuals.closed <- p.closed
#expected.closed <- theta.closed * total_individuals.closed
#expected.closed <- round(expected.closed[-length(expected.closed)])

## Pearson chi-square test between observed and expected
#chi_square_test.closed <- chisq.test(observed.closed, expected.closed)

## Print the chi-square test results
#cat("Chi-square test:","\n")
#chi_square_test.closed
