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




## Plotting to see adults and juvenile cases
ggplot(robin, aes(x = age_at_ringing)) +
  geom_bar(width = 0.5, color = "white", fill = "lightblue") +
  labs(title = "Histogram", x = "Values", y = "Frequency") +
  theme_minimal()


## Create separate data frames for adults and juveniles
adultsrobin <- robin[robin$age_at_ringing == "adult", ]
juvenilesrobin <- robin[robin$age_at_ringing == "juvenile", ]

## Printing the number of adults and juveniles
cat('No. of adults in robin:', nrow(adultsrobin),'\n')
cat('No. of juveniles in robin:', nrow(juvenilesrobin),'\n')



## Getting the number of adult and juvenile populations
juvenile_freq <- nrow(adultsrobin)
adult_freq <- nrow(juvenilesrobin)

## Getting the captured(1) and uncaptured(0) frequencies
freq_0 <- apply(robin, 1, function(x) sum(x == 0))
freq_1 <- apply(robin, 1, function(x) sum(x == 1))

## Creating a dataFrame for plotting
df <- data.frame(Bird = paste0("Bird ", 1:nrow(robin)),
                 Frequency_1 = freq_1,
                 age = robin$age_at_ringing)

## Creating a density plot 
ggplot(df, aes(x = Frequency_1, fill = age, color = age)) +
  geom_density(alpha = 0.5) +
  xlab("Frequency 1") +
  ylab("Density") +
  scale_fill_manual(values = c("black", "red"), labels=c("Adult", "Juvenile")) +
  scale_color_manual(values = c("black", "red"), labels= c("Adult", "Juvenile")) +
  theme_minimal()





## Calculate the number of captures for each individual
captureCounts <- rowSums(robin[, -ncol(robin)])

## Calculating the unique capture histories for each obseervation
capture_histories <- apply(robin[, -ncol(robin)], 1, paste, collapse = "")
uniqueCaptureHistories <- table(capture_histories)

## Plotting the number of unique capture histories
barplot(uniqueCaptureHistories, xlab = "Capture History", ylab = "Frequency", 
        main = "Number of Unique Capture Histories")

## Plotting the distribution of capture counts
hist(captureCounts, breaks = max(captureCounts), xlab = "Number of Captures", 
     ylab = "Frequency", main = "Distribution of Capture Counts")

## Plotting the recapture pattern for a specific individual (e.g.,individual 1)
individualCaptureHistory <- robin[100, -ncol(robin)]
plot(1:length(individualCaptureHistory), individualCaptureHistory, type = "b",
     pch = 19, xlab = "Capture Occasion", ylab = "Capture Status", 
     main = "Recapture Pattern - Individual")



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

# Exclude specific months from the sequence
excluded_months <- grepl("May|June|July|August|September", format(Months, "%B"))

# Convert the Months vector to a sequence of month names
Months <- format(Months, format = "%B %Y")

# Create a new sequence of months that excludes the specified months
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



##################### Introducing age dependent CJS-model ######################

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

## Calculating m-array probabilities for observed based on multinational assumption
m <- m_array(yearly_robin, T)


R <- unlist(colSums(yearly_robin)[-T])
## Calculating the expected probabilities for juveniles and adults
ex_m_juve <- expected_m_array(R ,phi.open_juve.robin, p.open.robin, T)
ex_m_adul <- expected_m_array(R ,phi.open_adul.robin, p.open.robin, T)

## Removing the first column from observed and expected m-arrays
m <- m[,-1]
ex_m_juve <- ex_m_juve[,-1]
ex_m_adul <- ex_m_adul[,-1]



## Doing a pearson chi-square test and calculating p-value using monte-carlo
## simulation 
chi_square_test.open.juve <- chisq.test(table(m, ex_m_juve), simulate.p.value = TRUE)

## Fisher test for robin dataset
fisher.test(table(m, ex_m_juve), simulate.p.value = TRUE)
## Printing the chi-square test results
cat("Chi-square test statistic for juveniles:","\n")
chi_square_test.open.juve

## Printing the chi-square test results
cat("Chi-square test statistic for juveniles:","\n")
chi_square_test.open.juve


## Doing a pearson chi-square test and calculating p-value using monte-carlo
## simulation 
chi_square_test.open.adul <- chisq.test(table(m, ex_m_adul), 
                                        simulate.p.value = TRUE)

## Fisher test for robin dataset
fisher.test(table(m, ex_m_adul), simulate.p.value = TRUE)
## Printing the chi-square test results
cat("Chi-square test statistic for adults:","\n")
chi_square_test.open.adul


## According to multinomial assumption degrees of freedom can be defined as 
## df = k-1-d (where k=No of multinomial cells, d=Number of estimated parameters)
alpha<- 0.05
df <- 47           #2924(for monthly data)
critical_value <- qchisq(1 - alpha, df)
cat("Critical value is:", critical_value)

############################ Confidence intervals ##############################
## Calculating 95% CI for juveniles population for 300 samples
ci.robin <- bootstrap_intervals.open(theta.open.robin, yearly_robin, T, 500, age)
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




## Creating a data frame to store the parameter estimates and corresponding 
## confidence intervals for juveniles
parameter_df <- data.frame(Estimate = param.open.juve ,
                           CI_lower = param.open.juve.lower,
                           CI_upper = param.open.juve.upper)

## Plotting the survivial probabilities with error bars

ggplot(parameter_df, aes(x = as.factor(1:10), y = )) +
  geom_violin(trim=FALSE)+geom_boxplot(width = 0.5, position = position_dodge(width = 0.75), color = "black", alpha = 0.5) +
  geom_pointrange(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2, position = position_dodge(width = 0.75), color = "red") +
  labs(x = "Parameter", y = "Estimate") +
  ggtitle("Violin Plots of Parameter Estimates with 95% Confidence Intervals")


############################ Closed population model ##########################

#theta.closed <- runif(T+1)
## Getting MLE for theta
#theta.closed <- log.mle.closed(theta.closed, yearly_robin, T)

## Getting the parameters estimates
#p.closed <- popEstimate(theta.closed)

## Calculating the sufficient statistics for obsevred data 
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
