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
source("~/Downloads/Birds/closedlik.R")
## Loading the open_population file
source("~/Downloads/Birds/CJSlik.R")


blackcap <- read.csv("~/Downloads/Birds/CR.blackcap_FixRing.csv", sep =';'
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
blackcap <- blackcap[,-ncol(blackcap)]
n <- nrow(blackcap)
T <- ncol(blackcap)

theta.closed <- rep(1, T+1)


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

theta.open <- runif(T)


## Getting MLE for theta for open population
theta.open <- log.mle.open(theta.open, blackcap, T)

## Calculating phi and p
phi <- exp(theta.open[1:(T-1)])/(1+exp(theta.open[1:(T-1)]))

## Getting the parameters estimates
p.open <- popEstimate.open(theta.open, T)

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
T<- ncol(blackcap)

m <- matrix(0, nrow = T-1, ncol = T)
total_cap <- colSums(blackcap)
m[,1] <- total_cap[-length(total_cap)]
NCap <- rep(0, T-1)
for (i in 1:(T-1)) {
  init_capidx <- blackcap[, i]
  totalCap <- m[i, 1]
  for (j in (i+1):T) {
    cap_idx <- blackcap[, j]
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
m <- cbind(m, NCap)
# Print the m-array
print(m)

## Calculating the observed probabilities in m-array
for (i in 1:(T-1)) {
  R <- m[i,1]
  
    if(R==0){
      next
    }
    
    else{
      m[i,-1] <- m[i,-1]/R
    }
}


## Printing the converted m-array probabilities
print(m)


## Calculating the expected probabilities
ex_m <- matrix(0, nrow = T-1, ncol = T)
ex_m[,1] <- m[,1]
N_exm <- rep(0, T-1)
for (i in 1:(T-2)) 
  {
  ex_m[i, (i+1) ] <- m[i,1] * phi[i] * p.open
  for (j in (i+2):T) 
    {
       ex_m[i, j] <-  ex_m[i, (j-1)] * (1-p.open) * phi[j-1]
  }
  N_exm[i] <- m[i,1] - sum(ex_m[i,-1]) 
  }
## For last row
ex_m[T-1, T] <- m[T-1, 1] * phi[T-1] * p.open
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
#fisher.test(table(m, ex_m), simulate.p.value = TRUE)
## Printing the chi-square test results
cat("Chi-square test statistic:","\n")
chi_square_test.open

## According to multinomial assumption degrees of freedom can be defined as 
## df = k-1-d (where k=No. of multinomial cells, d=Number of estimated parameters)
alpha<- 0.05
df <- 2924
critical_value <- qchisq(1 - alpha, df)
print("Critical value is:",critical_value)

########################### Monthly Estimates ##################################

############################ Open population ###################################


####################### Estimates for juvenile population ######################
n <- nrow(juvenilesblackcap)
T <- ncol(juvenilesblackcap)
## Defining start values for theta
min_theta <- 0
max_theta <- 1
theta.open.juv <- runif(T, min_theta, max_theta)


## Getting MLE for theta
theta.open.juv <- log.mle.open(theta.open.juv, juvenilesblackcap, T)

## Getting the parameters estimates
p.open.juv <- popEstimate.open(theta.open.juv, T)

## Getting the survival probability
jsurv_blackcap <- surProb.open(theta.open.juv, T)

## Calculating 95% CI for juveniles population
ci <- calculate_ci_total_population(theta.open.juv, juvenilesblackcap, n, T)
cat("95% CI for total popultion is:")
cat(ci)




######################## Estimates for Adult population ########################

## Getting estimate for adult blackcap population
n <- nrow(adultsblackcap)
T <- ncol(adultsblackcap)

## Defining start values for theta
min_theta <- 0
max_theta <- 10
theta.open.adult <- runif(T, min_theta, max_theta)

theta.open.adult <- log.mle.open(theta.open.adult, adultsblackcap, T)

## Getting the parameters estimates
p.open.adult <- popEstimate.open(theta.open.adult, T)

## Getting the survival probability
asurv_blackcap <- surProb.open(theta.open.adult, T)

## Calculating 95% CI for total adult population
ci <- calculate_ci_total_population(theta.open.adult, adultsblackcap, n, T)
cat("95% CI for total popultion is:")
cat(ci)

########################### Annual estimates ###################################


plotblackcapadul$Month <- Month
plotblackcapjuve$Month <- Month

# Create a new data frame to store the yearly capture-recapture data
n <- 77
window_len <- floor(n / 7)
Juveniles <- rep(0, window_len)
Adults <- rep(0, window_len)
for (i in 1:window_len) {
  start_idx <- (i - 1) * 7 + 1
  end_idx <- i * 7
  prod1 <- plotblackcapadul$CapSum[start_idx : end_idx]
  prod2 <- plotblackcapjuve$CapSum[start_idx : end_idx]
  product1 <- sum(prod1)
  product2 <- sum(prod2)
  Adults[i] <- product1
  Juveniles[i] <- product2
}

## Creating a yearly sequence
Year <- seq(2007, 2017)

yearly_adult <- data.frame(Year, Adults)
yearly_juvenile <- data.frame(Year, Juveniles)



# Plot the yearly adult capture-recapture data
A <- ggplot(yearly_adult, aes(x = Year, y = Adults)) +
  geom_point() +
  geom_line() +
  xlab("Year") +
  ylab("Number of individuals captured") +
  ggtitle("Yearly adult capture-recapture data")

# Plot the yearly juvenile capture-recapture data
J <- ggplot(yearly_juvenile, aes(x = Year, y = Juveniles)) +
  geom_point() +
  geom_line() +
  xlab("Year") +
  ylab("Number of individuals captured") +
  ggtitle("Yearly juvenile capture-recapture data")

# Arrange the plots side by side
grid.arrange(A, J, ncol = 2)


########################### Annual Survival ####################################
## Grouping indices annually
group_indices <- rep(1:ceiling(ncol(adultsblackcap)/7), each = 7, 
                     length.out = ncol(adultsblackcap))

## Converting dataFame to matrix format
df_matrix_adult <- as.matrix(adultsblackcap)
df_matrix_juvenile <- as.matrix(juvenilesblackcap)

## Summing the captured birds annually
Adult_annual <- t(apply(adultsblackcap, 1, 
                        function(x) tapply(x, INDEX = group_indices, FUN = sum)))

Juvenile_annual <- t(apply(juvenilesblackcap, 1, 
                           function(x) tapply(x, INDEX = group_indices, FUN = sum)))

## Printing the resulted dataframe
print(Adult_annual)
print(Juvenile_annual)



########################### Estimates for closed population ####################

################################# Juveniles ####################################
n <- nrow(Juvenile_annual)
T <- ncol(Juvenile_annual)

## Defining start values for theta
min_theta <- 0.1
max_theta <- 10
theta.closed.juv <- runif(T+1, min_theta, max_theta)


## Getting MLE for theta on annual data
theta.closed.juv.ann <- log.mle.closed(theta.closed.juv, Juvenile_annual, T)

## Getting the annual parameters estimates
p.closed.juv.ann <- popEstimate(theta.closed.juv.ann)

## Getting the annual survival probability
jsurv_blackcap.ann <- surProb(p.closed.juv.ann)

## Calculating 95% CI for total juvenile population
ci <- calculate_ci(theta.closed.juv.ann, Juvenile_annual, n, T)
cat("95% CI for total popultion is:", ci[nrow(ci), ]+n)

############################### Adults #########################################

n <- nrow(Adult_annual)
T <- ncol(Adult_annual)

## Defining start values for theta
min_theta <- 0.1
max_theta <- 10
theta.closed.adult.ann <- runif(T+1, min_theta, max_theta)


## Getting MLE for theta on annual data
theta.closed.adult.ann <- log.mle.closed(theta.closed.adult.ann, Adult_annual, T)

## Getting the annual parameters estimates
p.closed.adult.ann <- popEstimate(theta.closed.adult.ann)

## Getting the annual survival probability
asurv_blackcap.ann <- surProb(p.closed.adult.ann)

