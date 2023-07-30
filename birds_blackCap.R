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

## Loading the closed_population file
source("~/Documents/Birds/closedlik.R")
## Loading the open_population file
source("~/Documents/Birds/CJSlik.R")


blackcap <- read.csv("~/Documents/Birds/CR.blackcap_FixRing.csv", sep =';'
                     ,header=TRUE)

## Printing the dimensions of the blackcap DataFrame
cat("Dimension of blackcap data is:", dim(blackcap))

## Removing the first four months from the data
blackcap <- blackcap[, -c(1:4)]

## Checking if operation was successful
##View(blackcap)

## Checking if there is any rows with missing data 
naRows<- colSums(is.na(blackcap)) > 0
cat("No. of rows with missing data:", sum(naRows))

## Calculating number of zeroes in the observations
zeros <- length(which(rowSums(blackcap[, -ncol(blackcap)])==0))
cat("No. of zeros in data are:", zeros)

## Removing zero observations
blackcap <- blackcap[rowSums(blackcap[, -ncol(blackcap)] != 0) > 0, ]

## Getting unique value
ageRing <- unique(blackcap$age_at_ringing)
cat("No. of unique values in age_at_ringing:",ageRing)


## Getting the rows numbers where unkown is present and removing them
rowNum <- which(blackcap$age_at_ringing == "Unknown")
cat("Number of rows with unknown:",length(rowNum))

## Strong unknown observations in a separate dataFrame
unknownObs <- blackcap[rowNum,]

## Removing unkonwn from the dataFrame
blackcap <- blackcap[-rowNum, ]




## Plotting to see adults and juvenile cases
ggplot(blackcap, aes(x = age_at_ringing)) +
  geom_bar(width = 0.5, color = "white", fill = "lightblue") +
  labs(title = "Histogram", x = "Values", y = "Frequency") +
  theme_minimal()


## Create separate data frames for adults and juveniles
adultsblackcap <- blackcap[blackcap$age_at_ringing == "adult", ]
juvenilesblackcap <- blackcap[blackcap$age_at_ringing == "juvenile", ]

## Printing the number of adults and juveniles
cat('No. of adults in blackcap:', nrow(adultsblackcap),'\n')
cat('No. of juveniles in blackcap:', nrow(juvenilesblackcap),'\n')



## Getting the number of adult and juvenile populations
juvenile_freq <- nrow(adultsblackcap)
adult_freq <- nrow(juvenilesblackcap)

## Getting the captured(1) and uncaptured(0) frequencies
freq_0 <- apply(blackcap, 1, function(x) sum(x == 0))
freq_1 <- apply(blackcap, 1, function(x) sum(x == 1))

## Creating a dataFrame for plotting
df <- data.frame(Bird = paste0("Bird ", 1:nrow(blackcap)),
                 Frequency_1 = freq_1,
                 Age = blackcap$age_at_ringing)

## Creating a density plot 
ggplot(df, aes(x = Frequency_1, fill = Age, color = Age)) +
  geom_density(alpha = 0.5) +
  xlab("Frequency 1") +
  ylab("Density") +
  scale_fill_manual(values = c("black", "red"), labels=c("Adult", "Juvenile")) +
  scale_color_manual(values = c("black", "red"), labels= c("Adult", "Juvenile")) +
  theme_minimal()





## Calculate the number of captures for each individual
captureCounts <- rowSums(blackcap[, -ncol(blackcap)])

## Calculating the unique capture histories for each obseervation
capture_histories <- apply(blackcap[, -ncol(blackcap)], 1, paste, collapse = "")
uniqueCaptureHistories <- table(capture_histories)

## Plotting the number of unique capture histories
barplot(uniqueCaptureHistories, xlab = "Capture History", ylab = "Frequency", 
        main = "Number of Unique Capture Histories")

## Plotting the distribution of capture counts
hist(captureCounts, breaks = max(captureCounts), xlab = "Number of Captures", 
     ylab = "Frequency", main = "Distribution of Capture Counts")

## Plotting the recapture pattern for a specific individual (e.g.,individual 1)
individualCaptureHistory <- blackcap[100, -ncol(blackcap)]
plot(1:length(individualCaptureHistory), individualCaptureHistory, type = "b",
     pch = 19, xlab = "Capture Occasion", ylab = "Capture Status", 
     main = "Recapture Pattern - Individual")



## Removing the last column(age at ringing) from the adult and juvenile data
adultsblackcap <- adultsblackcap[, -ncol(adultsblackcap)]
juvenilesblackcap <-juvenilesblackcap[, -ncol(juvenilesblackcap)]

## Transposing the dataFrame
plotblackcapjuve <- t(juvenilesblackcap)
plotblackcapadul <- t(adultsblackcap)

## Converting all values in the data frame from string to integer
plotblackcapjuve <- as.data.frame(apply(plotblackcapjuve, 2, as.numeric))
plotblackcapadul <- as.data.frame(apply(plotblackcapadul, 2, as.numeric))


## Calculating the number of birds captured each day
plotblackcapjuve$CapSum <- rowSums(plotblackcapjuve)
plotblackcapadul$CapSum <- rowSums(plotblackcapadul)



Months <- seq(as.Date("2007-10-01"), as.Date("2018-04-01"), by = "months")

# Exclude specific months from the sequence
excluded_months <- grepl("May|June|July|August|September", format(Months, "%B"))

# Convert the Months vector to a sequence of month names
Months <- format(Months, format = "%B %Y")

# Create a new sequence of months that excludes the specified months
Month <- Months[!excluded_months]

## Making a combined dataFrame for captured adults and juveniles
combData <- data.frame(Month = c(1:77), Juvenile = plotblackcapjuve$CapSum, 
                       Adult = plotblackcapadul$CapSum)

## Reshape the data frame to a long format to plot
combData_long <- pivot_longer(combData, cols = c(Juvenile, Adult), 
                              names_to = "Group", values_to = "Captured")

## Using ggplot to plotting the captured birds over different months
p <- ggplot(combData_long, aes(x = Month, y = Captured, color = Group)) + 
  geom_point() + geom_line() +
  xlab("Months (October 2007 to April 2018)") +
  ylab("No. of captured (per session)") +
  ggtitle("Blackcap's captured over different sessions") +
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

########################### Model fitting check ################################

########################### Closed population ##################################
# Create a new data frame to store the yearly capture-recapture data
n <- 77
occassions <- floor(n / 7)
age_ring <- blackcap$age_at_ringing

blackcap <- blackcap[,-ncol(blackcap)]

yearly_blackcap <- matrix(0, nrow = nrow(blackcap), ncol = occassions)
for(j in 1:nrow(blackcap)){
  for (i in 1:occassions) {
    start_idx <- (i - 1) * 7 + 1
    end_idx <- i * 7
    prod <- blackcap[j, start_idx : end_idx]
    product <- sum(prod)
    yearly_blackcap[j, i] <- product
  }
}
cat('Birds captured over different inter-winter sessions','\n')
cat(colSums(yearly_blackcap))

## creating a age matrix
age <- matrix(0, nrow = nrow(blackcap), ncol = occassions)

for(i in 1:nrow(yearly_blackcap)){
  age_at_i <- age_ring[i]
  if(age_at_i=='adult'){
       first_cap <- which(yearly_blackcap[i,]>=1)[1]
       age[i, first_cap:occassions] <- age[i, first_cap:occassions]+2
       
  }
  else if (age_at_i=='juvenile'){
    first_cap <- which(yearly_blackcap[i,]>=1)[1]
    age[i, first_cap] <- 1
    if(first_cap+1>11){
      next
    }
    age[i, (first_cap+1):occassions] <- age[i, (first_cap+1):occassions]+2
    
  }
}

## Printing the age matrix
print(age)


## Getting MLE for theta
#theta.closed <- log.mle.closed(theta.closed, blackcap, T)

## Getting the parameters estimates
#p.closed <- popEstimate(theta.closed)

## Calculating the sufficient statistics for obsevred data 
#observed.closed <-  colSums(blackcap)

## Calculating expected individuals monthly
## Calculating the expected captures in a month
#total_individuals.closed <- p.closed
#expected.closed <- theta.closed * total_individuals.closed
#expected.closed <- round(expected.closed[-length(expected.closed)])

## Pearson chi-square test between observed and expected
#chi_square_test.closed <- chisq.test(observed.closed, expected.closed)

## Print the chi-square test results
#cat("Chi-square test:","\n")
#chi_square_test.closed

########################### Open population check ##############################


n <- nrow(yearly_blackcap)
T <- ncol(yearly_blackcap)
theta.open <- rnorm(T+1)


## Getting MLE for theta for open population
theta.open <- log.mle.open(theta.open, yearly_blackcap, age)

## Getting the parameters estimates
param.open <- popEstimate.open(theta.open, T)

p.open <- param.open[[3]]
phi.open_juve <- param.open[[1]]
phi.open_adul <- param.open[[2]]

cat('Constant recapture probability is:','\n')
cat(p.open)
cat('Juvenile survival probability is:','\n')
cat(phi.open_juve)
cat('Adult survival probability is:','\n')
cat(phi.open_adul)


## Below Example array taken from the book Analysis of capture-recapture data
## by morgan and MCcrea to check the functioning of m-arrays
#blackcap <- matrix(0, nrow = 4, ncol = 5)
#blackcap[1, 1] <- 0
#blackcap[1, 2] <- 1
#blackcap[1, 3] <- 0
#blackcap[1, 4] <- 1
#blackcap[1, 5] <- 1

#blackcap[2, 1] <- 0
#blackcap[2, 2] <- 1
#blackcap[2, 3] <- 0
#blackcap[2, 4] <- 1
#blackcap[2, 5] <- 0

#blackcap[3, 1] <- 1
#blackcap[3, 2] <- 1
#blackcap[3, 3] <- 0
#blackcap[3, 4] <- 1
#blackcap[3, 5] <- 1

#blackcap[4, 1] <- 1
#blackcap[4, 2] <- 1
#blackcap[4, 3] <- 1
#blackcap[4, 4] <- 1
#blackcap[4, 5] <- 1

## creating a empty m-array matrix
m <- matrix(0, nrow = T-1, ncol = T)

## Calculating total number of capture in each capture occasion
total_cap <- colSums(yearly_blackcap)

## Storing the results in first column of m-array
m[,1] <- total_cap[-length(total_cap)]

## Creating array for the birds that were never captured
NCap <- rep(0, T-1)

## Below for-loop compares capture occasion i with capture occasions from i+1 
## till T(Last capture occasion)
for (i in 1:(T-1)) {
  init_capidx <- yearly_blackcap[, i]
  totalCap <- m[i, 1]
  for (j in (i+1):T) {
    cap_idx <- yearly_blackcap[, j]
    if (totalCap > 0 & sum(init_capidx ==1 & cap_idx==1)<=totalCap) {
      m[i, j] <- sum(init_capidx ==1 & cap_idx==1)
      totalCap <- totalCap - m[i, j]
    } 
    else if(totalCap>=0 & sum(init_capidx==1 & cap_idx==1)>totalCap)
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
## Combining the m-array with the  
m <- cbind(m, NCap)
# Print the m-array
print(m)

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


## Printing the converted m-array probabilities
print(m)


## Calculating the expected m-array
ex_m <- matrix(0, nrow = T-1, ncol = T)
ex_m[,1] <- m[,1]
N_exm <- rep(0, T-1)
for (i in 1:(T-2)) 
  {
  ex_m[i, (i+1) ] <- m[i,1] * phi.open[i] * p.open
  for (j in (i+2):T) 
    {
       ex_m[i, j] <-  ex_m[i, (j-1)] * (1-p.open) * phi.open[j-1]
  }
  N_exm[i] <- m[i,1] - sum(ex_m[i,-1]) 
  }
## For last row
ex_m[T-1, T] <- m[T-1, 1] * phi.open[T-1] * p.open
N_exm[T-1] <- m[T-1, 1] - sum(ex_m[T-1,-1])

ex_m <- cbind(ex_m, N_exm)
print(ex_m)

for (i in 1:(T-1)) {
  R <- m[i,1]
  
  if(R==0){
    next
  }
  
  else{
    ex_m[i,-1] <- ex_m[i,-1]/R
  }
}
## Removing the first column from observed and expected m-arrays
m <- m[,-1]
ex_m <- ex_m[,-1]

## Adding small constant to mitigate numerical overflow
eps <- 1e-100

## Doing a pearson chi-square test and calculating p-value using monte-carlo
## simulation 
chi_square_test.open <- chisq.test(m+eps, ex_m+eps, simulate.p.value = TRUE)

## Fisher test for blackcap dataset
fisher.test(table(m, ex_m), simulate.p.value = TRUE)
## Printing the chi-square test results
cat("Chi-square test statistic:","\n")
chi_square_test.open

## According to multinomial assumption degrees of freedom can be defined as 
## df = k-1-d (where k=No of multinomial cells, d=Number of estimated parameters)
alpha<- 0.05
df <- 47           #2924(for monthly data)
critical_value <- qchisq(1 - alpha, df)
cat("Critical value is:", critical_value)


## Calculating 95% CI for juveniles population for 300 samples
ci <- bootstrap_intervals.open(yearly_blackcap, T, 100, age)
cat("95% CI for total popultion is:")
cat(ci)

## getting estimate for mean estimate
param.open <- popEstimate.open(ci[[2]], T)
param.open.juve <- param.open[[1]]
param.open.adul <- param.open[[2]]
p.cons <-  param.open[[3]]

## Getting the estimate for lower confidence intervals
param.open.lower <- popEstimate.open(ci$lower, T)
param.open.juve.lower <- param.open.lower[[1]]
param.open.adul.lower <- param.open.lower[[2]]
p.cons.lower <-  param.open.lower[[3]]

## Getting the estimate for upper confidence intervals
param.open.upper <- popEstimate.open(ci$upper, T)
param.open.juve.upper <- param.open.upper[[1]]
param.open.adul.upper <- param.open.upper[[2]]
p.cons.upper <-  param.open.upper[[3]]




# Create a data frame to store the parameter estimates and corresponding 
## confidence intervals for juveniles
parameter_df <- data.frame(Estimate = param.open.juve ,
                           CI_lower = param.open.juve.lower,
                           CI_upper = param.open.juve.upper)



ggplot(parameter_df, aes(x = as.factor(1:10), y = )) +
  geom_violin(trim=FALSE)+geom_boxplot(width = 0.5, position = position_dodge(width = 0.75), color = "black", alpha = 0.5) +
  geom_pointrange(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2, position = position_dodge(width = 0.75), color = "red") +
  labs(x = "Parameter", y = "Estimate") +
  ggtitle("Violin Plots of Parameter Estimates with 95% Confidence Intervals")
