############################ Birds project #####################################
############################ robin estimates #############################
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
  ggtitle("robin captured over different sessions") +
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
    total <- sum(prod)
    yearly_robin[j, i] <- total
  }
}


## Getting the total number of birds captured yearly
cat('Birds captured over different inter-winter sessions','\n')
cat(colSums(yearly_robin))

for(k in 1:nrow(yearly_robin)){
  for(j in 1:ncol(yearly_robin)){
    if(yearly_robin[k,j]!=0){
      yearly_robin[k,j]=1
    }
  }
}


n <- nrow(yearly_robin)            ## Total no of birds
T <- ncol(yearly_robin)            ## Total no. of capture occasions
theta.open <- rnorm(T)                ## Initialising the parameters



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
AIC_cons <- AIC(log_lik, 10)
cat("AIC for age independent model:", AIC_cons)



##################### Age dependent CJS-model #################################

## creating a age matrix to store age according to time
age <- matrix(0, nrow = nrow(robin), ncol = ncol(yearly_robin))

## Lopping over the data, if age at ringing at juvenile at first then keeping
## that time 1 otherwise after that it's gonna adult so adding 2 to show bird has
## become adult 
for(i in 1:nrow(yearly_robin)){
  age_at_i <- age_ring[i]
  if(age_at_i=='adult'){
    first_cap <- which(yearly_robin[i,]==1)[1]
    age[i, first_cap:occassions] <- age[i, first_cap:occassions]+2
    
  }
  else if (age_at_i=='juvenile'){
    first_cap <- which(yearly_robin[i,]==1)[1]
    age[i, first_cap] <- 1
    if(first_cap+1>11){
      next
    }
    age[i, (first_cap+1):occassions] <- age[i, (first_cap+1):occassions]+2
    
  }
}

# for(i in 1:nrow(robin)){
#   age_at_i <- age_ring[i]
#   if(age_at_i=='adult'){
#     first_cap <- which(robin[i,]==1)[1]
#     age[i, first_cap:ncol(robin)] <- age[i, first_cap:ncol(robin)]+2
#     
#   }
#   else if (age_at_i=='juvenile'){
#     first_cap <- which(robin[i,]==1)[1]
#     if(first_cap+1>78){
#       next
#     }
#     else if(first_cap+6>=77){
#       age[i, first_cap:ncol(robin)] <- age[i, first_cap:ncol(robin)]+1
#     }
#     else{
#       if(first_cap+7<=77){
#         age[i, first_cap:(first_cap+6)]<-age[i, first_cap:(first_cap+6)]+1
#         age[i, (first_cap+7):ncol(robin)]<-age[i, (first_cap+7):ncol(robin)]+2
#       }
#     }
#   }
# }

## viewing the age matrix
#View(age)


# ## Getting counts for juveniles and adults in each occassion
# get_counts <- function(col) {
#   table(col)
# }
# 
# ## Apply the function to each column of the age matrix
# age_counts <- apply(age, 2, get_counts)



n <- nrow(yearly_robin)            ## Total no of birds
T <- ncol(yearly_robin)            ## Total no. of capture occasions
theta.open <- runif(T+1)           ## Initialising the parameters


## Getting MLE for theta for open population
theta.open.robin <- log.mle.open.age(theta.open, yearly_robin, age)

## creating empty arrays to store initial and final capture occasions
f <- rep(0, n)
l <- rep(0, n)
## Stores first individual is observed
for (i in 1:n){f[i] <- which(yearly_robin[i,]==1)[1]}
## Storing the last time individual is observed
for (i in 1:n){l[i] <- which(yearly_robin[i,]==1)[length(which(yearly_robin[i,]==1))]}
## Caluclating the vale of log_likeihood at MLE
log_lik_age <- CJSlik_age(theta.open.robin, yearly_robin, f, l, n, age, T)
## Getting the AIC 
AIC_age <- AIC(log_lik_age, 10)
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

## Calculating m-array probabilities for observed data
marray <- m_array(yearly_robin, T)


## Total number of birds captued in each occassion
R <- unlist(colSums(yearly_robin)[-T])
## Calculating the expected probabilities for juveniles and adults
ex_m <- expected_m_array(R ,phi.open_juve.robin, phi.open_adul.robin, 
                         p.open.robin, T)

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

## hosmer test for absolute goodness of fit test
hoslem.test(select_bin_m, select_bin_exm, g=11)

result <- xmulti(select_bin_m, select_bin_exm, safety = 1e+15)
cat("p-value using multinomial goodness of fit test:", result$observedProb)



############################ Confidence intervals ##############################
## Calculating 95% ci.robin for juveniles population for 500 samples
ci.robin <- bootstrap_intervals.open(theta.open.robin, yearly_robin, T, 500, age)
cat("95% ci.robin for total popultion is:")

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
parameter_df.juve <- data.frame(TimePoint = seq(1,10),
                                Estimate = phi.open_juve.robin ,
                                ci.robin_lower.juve = param.open.juve.lower,
                                ci.robin_upper.juve = param.open.juve.upper)
## Printing the resulted dataframe
parameter_df.juve

# Creating a data frame to store the parameter estimates and corresponding 
## confidence intervals for adults
parameter_df.adul <- data.frame(TimePoint = seq(1,10), 
                                Estimate = phi.open_adul.robin,
                                ci.robin_lower.adul = param.open.adul.lower,
                                ci.robin_upper.dul = param.open.adul.upper)
## Printing the dataframe
parameter_df.adul



# Create a data frame for the juvenile and adult data
juvenile_data <- data.frame(
  Session = c("2007-2008", "2008-2009", "2009-2010", "2010-2011", "2011-2012", 
              "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017"),
  phi_MLE = phi.open_juve.robin,
  CI_lower = param.open.juve.lower,
  CI_upper = param.open.juve.upper
)

adult_data <- data.frame(
  Session = c("2007-2008", "2008-2009", "2009-2010", "2010-2011", "2011-2012", 
              "2012-2013", "2013-2014", "2014-2015", "2015-2016", "2016-2017"),
  phi_MLE = phi.open_adul.robin,
  CI_lower = param.open.adul.lower,
  CI_upper = param.open.adul.upper
)

# Combine the data for plotting
combined_data <- rbind(
  transform(juvenile_data, Group = "Juveniles"),
  transform(adult_data, Group = "Adults")
)


# Create a new numeric column to represent the sessions
combined_data$NumericSession <- as.numeric(factor(combined_data$Session))

# Adjust the x-axis position for adults and juveniles
combined_data <- transform(combined_data,
                           Position = ifelse(Group == "Adults", NumericSession + 0.2, NumericSession - 0.2))
## Adding colors to different species
custom_colors <- c("green3", "red2")
# Create the plot
plot <- ggplot(combined_data, aes(x = Position, y = phi_MLE, color = Group, group = Group)) +
  geom_line() +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  geom_point(position = position_dodge(width = 0.1), size = 3) +  # Adjust dodge width and point size
  labs(title = "Survival Probability Estimates for robin birds",
       y = "Survival Probability",
       x = "Session") +
  scale_x_continuous(breaks = combined_data$Position, labels = combined_data$Session) +  # Use adjusted positions for labels and breaks
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11), plot.title = element_text(hjust = 0.5))

## Adding the colors
plot <- plot + scale_color_manual(values = custom_colors)
# Print the plot
print(plot)