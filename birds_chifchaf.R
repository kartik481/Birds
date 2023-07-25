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


chifChaf <- read.csv("~/Downloads/Birds/CR.chifChaf_FixRing.csv", sep =';'
                     ,header=TRUE)

## Printing the dimensions of the chifChaf DataFrame
cat("Dimension of chifChaf data is:", dim(chifChaf))

## Removing the first four months from the data
chifChaf <- chifChaf[, -c(1:4)]

## Checking if operation was successful
##View(chifChaf)

## Checking if there is any rows with missing data 
naRows<- colSums(is.na(chifChaf)) > 0
cat("No. of rows with missing data:", sum(naRows))

## Calculating number of zeroes in the observations
zeros <- length(which(rowSums(chifChaf[, -ncol(chifChaf)])==0))
cat("No. of zeros in data are:", zeros)

## Removing zero observations
chifChaf <- chifChaf[rowSums(chifChaf[, -ncol(chifChaf)] != 0) > 0, ]

## Getting unique value
ageRing <- unique(chifChaf$age_at_ringing)
cat("No. of unique values in age_at_ringing:",ageRing)


## Getting the rows numbers where unkown is present and removing them
rowNum <- which(chifChaf$age_at_ringing == "Unknown")
cat("Number of rows with unknown:",length(rowNum))

## Strong unknown observations in a separate dataFrame
unknownObs <- chifChaf[rowNum,]

## Removing unkonwn from the dataFrame
chifChaf <- chifChaf[-rowNum, ]




## Plotting to see adults and juvenile cases
ggplot(chifChaf, aes(x = age_at_ringing)) +
  geom_bar(width = 0.5, color = "white", fill = "lightblue") +
  labs(title = "Histogram", x = "Values", y = "Frequency") +
  theme_minimal()


## Create separate data frames for adults and juveniles
adultschifChaf <- chifChaf[chifChaf$age_at_ringing == "adult", ]
juvenileschifChaf <- chifChaf[chifChaf$age_at_ringing == "juvenile", ]

## Printing the number of adults and juveniles
cat('No. of adults in chifChaf:', nrow(adultschifChaf),'\n')
cat('No. of juveniles in chifChaf:', nrow(juvenileschifChaf),'\n')



## Getting the number of adult and juvenile populations
juvenile_freq <- nrow(adultschifChaf)
adult_freq <- nrow(juvenileschifChaf)

## Getting the captured(1) and uncaptured(0) frequencies
freq_0 <- apply(chifChaf, 1, function(x) sum(x == 0))
freq_1 <- apply(chifChaf, 1, function(x) sum(x == 1))

## Creating a dataFrame for plotting
df <- data.frame(Bird = paste0("Bird ", 1:nrow(chifChaf)),
                 Frequency_1 = freq_1,
                 Age = chifChaf$age_at_ringing)

## Creating a density plot 
ggplot(df, aes(x = Frequency_1, fill = Age, color = Age)) +
  geom_density(alpha = 0.5) +
  xlab("Frequency 1") +
  ylab("Density") +
  scale_fill_manual(values = c("black", "red"), labels=c("Adult", "Juvenile")) +
  scale_color_manual(values = c("black", "red"), labels= c("Adult", "Juvenile")) +
  theme_minimal()





## Calculate the number of captures for each individual
captureCounts <- rowSums(chifChaf[, -ncol(chifChaf)])

## Calculating the unique capture histories for each obseervation
capture_histories <- apply(chifChaf[, -ncol(chifChaf)], 1, paste, collapse = "")
uniqueCaptureHistories <- table(capture_histories)

## Plotting the number of unique capture histories
barplot(uniqueCaptureHistories, xlab = "Capture History", ylab = "Frequency", 
        main = "Number of Unique Capture Histories")

## Plotting the distribution of capture counts
hist(captureCounts, breaks = max(captureCounts), xlab = "Number of Captures", 
     ylab = "Frequency", main = "Distribution of Capture Counts")

## Plotting the recapture pattern for a specific individual (e.g.,individual 1)
individualCaptureHistory <- chifChaf[100, -ncol(chifChaf)]
plot(1:length(individualCaptureHistory), individualCaptureHistory, type = "b",
     pch = 19, xlab = "Capture Occasion", ylab = "Capture Status", 
     main = "Recapture Pattern - Individual")



## Removing the last column(age at ringing) from the adult and juvenile data
adultschifChaf <- adultschifChaf[, -ncol(adultschifChaf)]
juvenileschifChaf <-juvenileschifChaf[, -ncol(juvenileschifChaf)]

## Transposing the dataFrame
plotchifChafjuve <- t(juvenileschifChaf)
plotchifChafadul <- t(adultschifChaf)

## Converting all values in the data frame from string to integer
plotchifChafjuve <- as.data.frame(apply(plotchifChafjuve, 2, as.numeric))
plotchifChafadul <- as.data.frame(apply(plotchifChafadul, 2, as.numeric))


## Calculating the number of birds captured each day
plotchifChafjuve$CapSum <- rowSums(plotchifChafjuve)
plotchifChafadul$CapSum <- rowSums(plotchifChafadul)



Months <- seq(as.Date("2007-10-01"), as.Date("2018-04-01"), by = "months")

# Exclude specific months from the sequence
excluded_months <- grepl("May|June|July|August|September", format(Months, "%B"))

# Convert the Months vector to a sequence of month names
Months <- format(Months, format = "%B %Y")

# Create a new sequence of months that excludes the specified months
Month <- Months[!excluded_months]

## Making a combined dataFrame for captured adults and juveniles
combData <- data.frame(Month = c(1:77), Juvenile = plotchifChafjuve$CapSum, 
                       Adult = plotchifChafadul$CapSum)

## Reshape the data frame to a long format to plot
combData_long <- pivot_longer(combData, cols = c(Juvenile, Adult), 
                              names_to = "Group", values_to = "Captured")

## Using ggplot to plotting the captured birds over different months
p <- ggplot(combData_long, aes(x = Month, y = Captured, color = Group)) + 
  geom_point() + geom_line() +
  xlab("Months (October 2007 to April 2018)") +
  ylab("No. of captured (per session)") +
  ggtitle("Chiffchaff's captured over different sessions") +
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
chifChaf <- chifChaf[,-ncol(chifChaf)]
n <- nrow(chifChaf)
T <- ncol(chifChaf)

## Defining start values for theta
min_theta <- 0
max_theta <- 1
theta.closed <- runif(T+1, min_theta, max_theta)


## Getting MLE for theta
theta.closed <- log.mle.closed(theta.closed, chifChaf, T)

## Getting the parameters estimates
p.closed <- popEstimate(theta.closed)

## Calculating the sufficient statistics for obsevred data 
observed.closed <-  colSums(chifChaf)

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
theta.open <- log.mle.open(theta.open, chifChaf, T)

## Getting the parameters estimates
p.open <- popEstimate.open(theta.open, T)

## Calculating the expected captures in a month
total_individuals.open <- p.open
expected.open <- theta.open * total_individuals.open
expected.open <- round(expected.open)


chifChaf <- t(chifChaf)
## Calculating observed based on open population
observed.open <- rep(0, T)
observed.open <- rep(0, T)
for (i in 1:T) {
  captured_once <- sum(rowSums(chifChaf[, 1:i]) > 0)
  observed.open[i] <- captured_once / mean(rowSums(chifChaf[, 1:i])) * n
}

cat("Estimated population size:", round(estimated_size))

## Peforming chi-square test
chi_square_test.open <- chisq.test(observed, expected.open)

## Printing the chi-square test results
cat("Chi-square test:","\n")
chi_square_test.open

########################### Monthly Estimates ##################################

########################### Closed population ##################################

####################### Estimate for juvenile population #######################

## Getting the dimensions for juvenile chifChaf population
n <- nrow(juvenileschifChaf)
T <- ncol(juvenileschifChaf)

## Defining start values for theta
min_theta <- 0
max_theta <- 1
theta.closed.juv <- runif(T+1, min_theta, max_theta)


## Getting MLE for theta
theta.closed.juv <- log.mle.closed(theta.closed.juv, juvenileschifChaf, T)

## Getting the parameters estimates
p.closed.juv <- popEstimate(theta.closed.juv)

## Getting the survival probability
jsurv_chifChaf <- surProb(p.closed.juv)




n_bootstrap <- 1000  # number of bootstrap samples

# Calculate the bootstrap intervals
bootstrap_result <- bootstrap_intervals(data = juvenileschifChaf, T = T, 
                                        n_bootstrap = n_bootstrap)

# Print the bootstrap intervals
cat("Bootstrap Intervals (Lower):", bootstrap_result$lower, "\n")
cat("Bootstrap Intervals (Upper):", bootstrap_result$upper, "\n")

###################### Estimate for Adult popultion ############################

## Getting estimate for adult chifChaf population
n <- nrow(adultschifChaf)
T <- ncol(adultschifChaf)

## Defining start values for theta
min_theta <- 0
max_theta <- 10
theta.closed.adult <- runif(T+1, min_theta, max_theta)

theta.closed.adult <- log.mle.closed(theta.closed.adult, adultschifChaf, T)

## Getting the parameters estimates
p.closed.adult <- popEstimate(theta.closed.adult)

## Getting the survival probability
asurv_chifChaf <- surProb(p.closed.adult)



############################ Open population ###################################


####################### Estimates for juvenile population ######################
n <- nrow(juvenileschifChaf)
T <- ncol(juvenileschifChaf)
## Defining start values for theta
min_theta <- 0
max_theta <- 1
theta.open.juv <- runif(T, min_theta, max_theta)


## Getting MLE for theta
theta.open.juv <- log.mle.open(theta.open.juv, juvenileschifChaf, T)

## Getting the parameters estimates
p.open.juv <- popEstimate.open(theta.open.juv, T)

## Getting the survival probability
jsurv_chifChaf <- surProb.open(theta.open.juv, T)

## Calculating 95% CI for juveniles population
ci <- calculate_ci_total_population(theta.open.juv, juvenileschifChaf, n, T)
cat("95% CI for total popultion is:")
cat(ci)




######################## Estimates for Adult population ########################

## Getting estimate for adult chifChaf population
n <- nrow(adultschifChaf)
T <- ncol(adultschifChaf)

## Defining start values for theta
min_theta <- 0
max_theta <- 10
theta.open.adult <- runif(T, min_theta, max_theta)

theta.open.adult <- log.mle.open(theta.open.adult, adultschifChaf, T)

## Getting the parameters estimates
p.open.adult <- popEstimate.open(theta.open.adult, T)

## Getting the survival probability
asurv_chifChaf <- surProb.open(theta.open.adult, T)

## Calculating 95% CI for total adult population
ci <- calculate_ci_total_population(theta.open.adult, adultschifChaf, n, T)
cat("95% CI for total popultion is:")
cat(ci)

########################### Annual estimates ###################################


plotchifChafadul$Month <- Month
plotchifChafjuve$Month <- Month

# Create a new data frame to store the yearly capture-recapture data
n <- 77
window_len <- floor(n / 7)
Juveniles <- rep(0, window_len)
Adults <- rep(0, window_len)
for (i in 1:window_len) {
  start_idx <- (i - 1) * 7 + 1
  end_idx <- i * 7
  prod1 <- plotchifChafadul$CapSum[start_idx : end_idx]
  prod2 <- plotchifChafjuve$CapSum[start_idx : end_idx]
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
group_indices <- rep(1:ceiling(ncol(adultschifChaf)/7), each = 7, 
                     length.out = ncol(adultschifChaf))

## Converting dataFame to matrix format
df_matrix_adult <- as.matrix(adultschifChaf)
df_matrix_juvenile <- as.matrix(juvenileschifChaf)

## Summing the captured birds annually
Adult_annual <- t(apply(adultschifChaf, 1, 
                      function(x) tapply(x, INDEX = group_indices, FUN = sum)))

Juvenile_annual <- t(apply(juvenileschifChaf, 1, 
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
jsurv_chifChaf.ann <- surProb(p.closed.juv.ann)

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
asurv_chifChaf.ann <- surProb(p.closed.adult.ann)

