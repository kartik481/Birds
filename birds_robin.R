############################ Birds project #####################################

## setting the seed
set.seed(123)

## Loading the Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(Rcapture)
library(gridExtra)
library(marked)
library(vcd)

## Loading the closed_population file
source("~/Downloads/Birds/closedlik.R")
## Loading the open_population file
source("~/Downloads/Birds/CJSlik.R")


robin <- read.csv("~/Downloads/Birds/CR.robin_FixRing.csv", sep =';'
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
                 Age = robin$age_at_ringing)

## Creating a density plot 
ggplot(df, aes(x = Frequency_1, fill = Age, color = Age)) +
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
  ggtitle("Robins captured over different sessions") +
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
robin <- robin[,-ncol(robin)]
n <- nrow(robin)
T <- ncol(robin)

## Defining start values for theta
min_theta <- 0
max_theta <- 10
theta.closed <- runif(T+1, min_theta, max_theta)


## Getting MLE for theta
theta.closed <- log.mle.closed(theta.closed, robin, T)

## Getting the parameters estimates
p.closed <- popEstimate(theta.closed)

## Calculating the sufficient statistics for obsevred data 
observed.closed <-  colSums(robin)

## Calculating expected individuals monthly
## Calculating the expected captures in a month
total_individuals.closed <- p.closed
expected.closed <- theta.closed * total_individuals.closed
expected.closed <- round(expected.closed[-length(expected.closed)])

## Pearson chi-square test between observed and expected
chi_square_test.closed <- chisq.test(observed.closed, expected.closed)

## Print the chi-square test results
cat("Chi-square test:","\n")
chi_square_test.closed

########################### Open population check ##############################
min_theta <- 0
max_theta <- 1
theta.open <- runif(T, min_theta, max_theta)


## Getting MLE for theta for open population
theta.open <- log.mle.open(theta.open, robin, T)

## Getting the parameters estimates
p.open <- popEstimate.open(theta.open, T)

## Calculating the expected captures in a month
total_individuals.open <- p.open
expected.open <- theta.open * total_individuals.open
expected.open <- round(expected.open)

#robin <- matrix(0, nrow = 4, ncol = 5)
#robin[1, 1] <- 0
#robin[1, 2] <- 1
#robin[1, 3] <- 0
#robin[1, 4] <- 1
#robin[1, 5] <- 1

#robin[2, 1] <- 0
#robin[2, 2] <- 1
#robin[2, 3] <- 0
#robin[2, 4] <- 1
#robin[2, 5] <- 0

#robin[3, 1] <- 1
#robin[3, 2] <- 1
#robin[3, 3] <- 0
#robin[3, 4] <- 1
#robin[3, 5] <- 1

#robin[4, 1] <- 1
#robin[4, 2] <- 1
#robin[4, 3] <- 1
#robin[4, 4] <- 1
#robin[4, 5] <- 1

m <- matrix(0, nrow = T-1, ncol = T)
total_cap <- colSums(robin)
m[,1] <- total_cap[-length(total_cap)]
NCap <- rep(0, T-1)
for (i in 1:(T-1)) {
  init_capidx <- robin[, i]
  totalCap <- m[i, 1]
  for (j in (i+1):T) {
    cap_idx <- robin[, j]
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

m <- as.matrix(cbind(m, NCap))
# Print the m-array
print(m)



## Calculating observed based on open population


cat("Estimated population size:", round(estimated_size))

## Peforming chi-square test
chi_square_test.open <- chisq.test(observed, expected.open)

## Printing the chi-square test results
cat("Chi-square test:","\n")
chi_square_test.open

########################### Monthly Estimates ##################################

########################### Closed population ##################################

####################### Estimate for juvenile population #######################

## Getting the dimensions for juvenile robin population
n <- nrow(juvenilesrobin)
T <- ncol(juvenilesrobin)

## Defining start values for theta
min_theta <- 0
max_theta <- 1
theta.closed.juv <- runif(T+1, min_theta, max_theta)


## Getting MLE for theta
theta.closed.juv <- log.mle.closed(theta.closed.juv, juvenilesrobin, T)

## Getting the parameters estimates
p.closed.juv <- popEstimate(theta.closed.juv)

## Getting the survival probability
jsurv_robin <- surProb(p.closed.juv)




n_bootstrap <- 1000  # number of bootstrap samples

# Calculate the bootstrap intervals
bootstrap_result <- bootstrap_intervals(data = juvenilesrobin, T = T, 
                                        n_bootstrap = n_bootstrap)

# Print the bootstrap intervals
cat("Bootstrap Intervals (Lower):", bootstrap_result$lower, "\n")
cat("Bootstrap Intervals (Upper):", bootstrap_result$upper, "\n")

###################### Estimate for Adult popultion ############################

## Getting estimate for adult robin population
n <- nrow(adultsrobin)
T <- ncol(adultsrobin)

## Defining start values for theta
min_theta <- 0
max_theta <- 10
theta.closed.adult <- runif(T+1, min_theta, max_theta)

theta.closed.adult <- log.mle.closed(theta.closed.adult, adultsrobin, T)

## Getting the parameters estimates
p.closed.adult <- popEstimate(theta.closed.adult)

## Getting the survival probability
asurv_robin <- surProb(p.closed.adult)



############################ Open population ###################################


####################### Estimates for juvenile population ######################
n <- nrow(juvenilesrobin)
T <- ncol(juvenilesrobin)
## Defining start values for theta
min_theta <- 0
max_theta <- 1
theta.open.juv <- runif(T, min_theta, max_theta)


## Getting MLE for theta
theta.open.juv <- log.mle.open(theta.open.juv, juvenilesrobin, T)

## Getting the parameters estimates
p.open.juv <- popEstimate.open(theta.open.juv, T)

## Getting the survival probability
jsurv_robin <- surProb.open(theta.open.juv, T)

## Calculating 95% CI for juveniles population
ci <- calculate_ci_total_population(theta.open.juv, juvenilesrobin, n, T)
cat("95% CI for total popultion is:")
cat(ci)




######################## Estimates for Adult population ########################

## Getting estimate for adult robin population
n <- nrow(adultsrobin)
T <- ncol(adultsrobin)

## Defining start values for theta
min_theta <- 0
max_theta <- 10
theta.open.adult <- runif(T, min_theta, max_theta)

theta.open.adult <- log.mle.open(theta.open.adult, adultsrobin, T)

## Getting the parameters estimates
p.open.adult <- popEstimate.open(theta.open.adult, T)

## Getting the survival probability
asurv_robin <- surProb.open(theta.open.adult, T)

## Calculating 95% CI for total adult population
ci <- calculate_ci_total_population(theta.open.adult, adultsrobin, n, T)
cat("95% CI for total popultion is:")
cat(ci)

########################### Annual estimates ###################################


plotrobinadul$Month <- Month
plotrobinjuve$Month <- Month

# Create a new data frame to store the yearly capture-recapture data
n <- 77
window_len <- floor(n / 7)
Juveniles <- rep(0, window_len)
Adults <- rep(0, window_len)
for (i in 1:window_len) {
  start_idx <- (i - 1) * 7 + 1
  end_idx <- i * 7
  prod1 <- plotrobinadul$CapSum[start_idx : end_idx]
  prod2 <- plotrobinjuve$CapSum[start_idx : end_idx]
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
group_indices <- rep(1:ceiling(ncol(adultsrobin)/7), each = 7, 
                     length.out = ncol(adultsrobin))

## Converting dataFame to matrix format
df_matrix_adult <- as.matrix(adultsrobin)
df_matrix_juvenile <- as.matrix(juvenilesrobin)

## Summing the captured birds annually
Adult_annual <- t(apply(adultsrobin, 1, 
                        function(x) tapply(x, INDEX = group_indices, FUN = sum)))

Juvenile_annual <- t(apply(juvenilesrobin, 1, 
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
jsurv_robin.ann <- surProb(p.closed.juv.ann)

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
asurv_robin.ann <- surProb(p.closed.adult.ann)

