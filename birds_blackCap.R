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

## Loading the blackcap dataset
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
age.blackcapRing <- unique(blackcap$age.blackcap_at_ringing)
cat("No. of unique values in age.blackcap_at_ringing:",age.blackcapRing)


## Getting the rows numbers where unkown is present and removing them
rowNum <- which(blackcap$age.blackcap_at_ringing == "Unknown")
cat("Number of rows with unknown:",length(rowNum))

## Strong unknown observations in a separate dataFrame
unknownObs <- blackcap[rowNum,]

## Removing unkonwn from the dataFrame
blackcap <- blackcap[-rowNum, ]




## Plotting to see adults and juvenile cases
ggplot(blackcap, aes(x = age.blackcap_at_ringing)) +
  geom_bar(width = 0.5, color = "white", fill = "lightblue") +
  labs(title = "Histogram", x = "Values", y = "Frequency") +
  theme_minimal()


## Create separate data frames for adults and juveniles
adultsblackcap <- blackcap[blackcap$age.blackcap_at_ringing == "adult", ]
juvenilesblackcap <- blackcap[blackcap$age.blackcap_at_ringing == "juvenile", ]

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
                 age.blackcap = blackcap$age.blackcap_at_ringing)

## Creating a density plot 
ggplot(df, aes(x = Frequency_1, fill = age.blackcap, color = age.blackcap)) +
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



## Removing the last column(age.blackcap at ringing) from the adult and juvenile data
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


## generating the month names from October 2007 to April 2018 
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
  ggtitle("blackcap captured over different sessions") +
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


n <- 77                                ## Total no initial occasions
occassions <- floor(n / 7)             ## Inter-winter period spans for 7 months
## So dividing by 7 to convert to yearly

## Storing age at initial ringing
age.blackcap_ring <- blackcap$age.blackcap_at_ringing

## Removing the last column
blackcap <- blackcap[,-ncol(blackcap)]

# Create a new data frame to store the yearly capture-recapture data
yearly_blackcap <- matrix(0, nrow = nrow(blackcap), ncol = occassions)

## This loop sum over first 7 months, then 7... till last occasion and store it 
## in new matrix
for(j in 1:nrow(blackcap)){
  for (i in 1:occassions) {
    start_idx <- (i - 1) * 7 + 1
    end_idx <- i * 7
    prod <- blackcap[j, start_idx : end_idx]
    product <- sum(prod)
    yearly_blackcap[j, i] <- product
  }
}
## Getting the total number of birds captured yearly
cat('Birds captured over different inter-winter sessions','\n')
cat(colSums(yearly_blackcap))

## creating a age.blackcap matrix to store age according to time
age.blackcap <- matrix(0, nrow = nrow(blackcap), ncol = occassions)

## Lopping over the data, if age at ringing at juvenile at first then keeping
## that time 1 otherwise after that it's gonna adult so adding 2 to show bird has
## become adult 
for(i in 1:nrow(yearly_blackcap)){
  age.blackcap_at_i <- age.blackcap_ring[i]
  if(age.blackcap_at_i=='adult'){
    first_cap <- which(yearly_blackcap[i,]>=1)[1]
    age.blackcap[i, first_cap:occassions] <- age.blackcap[i, first_cap:occassions]+2
    
  }
  else if (age.blackcap_at_i=='juvenile'){
    first_cap <- which(yearly_blackcap[i,]>=1)[1]
    age.blackcap[i, first_cap] <- 1
    if(first_cap+1>11){
      next
    }
    age.blackcap[i, (first_cap+1):occassions] <- age.blackcap[i, (first_cap+1):occassions]+2
    
  }
}

## Printing the age.blackcap matrix
print(age.blackcap)


n <- nrow(yearly_blackcap)            ## Total no of birds
T <- ncol(yearly_blackcap)            ## Total no. of capture occasions
theta.open <- runif(T+1)           ## Initialising the parameters


## Getting MLE for theta for open population
theta.open.blackcap <- log.mle.open(theta.open, yearly_blackcap, age.blackcap.blackcap)

## Getting the parameters estimates
param.open.blackcap <- popEstimate.open(theta.open.blackcap, T)

p.open.blackcap <- param.open.blackcap[[3]]
phi.open_juve.blackcap <- param.open.blackcap[[1]]
phi.open_adul.blackcap <- param.open.blackcap[[2]]

## Printing the results
cat('Constant recapture probability is: \n')
cat(p.open.blackcap,'\n')
cat('Juvenile survival probability is: \n')
cat(phi.open_juve.blackcap,'\n')
cat('Adult survival probability is: \n')
cat(phi.open_adul.blackcap,'\n')

###################### Absolute goodness of fit test ###########################

## Calculating m-array probabilities for observed based on multinational assumption
m <- m_array(yearly_blackcap, T)


R <- unlist(colSums(yearly_blackcap)[-T])
## Calculating the expected probabilities
ex_m <- expected_m_array(R ,phi.open_adul.blackcap, p.open.blackcap, T)

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

############################ Confidence intervals ##############################
## Calculating 95% CI for juveniles population for 300 samples
ci.blackcap <- bootstrap_intervals.open(yearly_blackcap, T, 200, age.blackcap)
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
#theta.closed <- log.mle.closed(theta.closed, yearly_blackcap, T)

## Getting the parameters estimates
#p.closed <- popEstimate(theta.closed)

## Calculating the sufficient statistics for obsevred data 
#observed.closed <-  colSums(yearly_blackcap)

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
