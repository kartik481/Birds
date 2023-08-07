####################### Visualising the data jointly ###########################

## Loading the blackcap dataset
blackcap <- read.csv("~/Documents/Birds/CR.blackcap_FixRing.csv", sep =';'
                     ,header=TRUE)
## Loading the chifchaf dataset
chifchaf <- read.csv("~/Documents/Birds/CR.chifchaf_FixRing.csv", sep =';'
                     ,header=TRUE)
## Loading the robin dataset
robin <- read.csv("~/Documents/Birds/CR.robin_FixRing.csv", sep =';'
                  ,header=TRUE)





## Removing the first four months from the data
blackcap <- blackcap[, -c(1:4)]

## Checking if operation was successful
##View(robin)

## Checking if there is any rows with missing data 
naRows<- colSums(is.na(blackcap)) > 0
cat("No. of rows with missing data:", sum(naRows))

## Calculating number of zeroes in the observations
zeros <- length(which(rowSums(blackcap[, -ncol(blackcap)])==0))
cat("No. of zeros in data are:", zeros)

## Removing zero observations
blackcap <- blackcap[rowSums(blackcap[, -ncol(blackcap)] != 0) > 0, ]

## Getting the rows numbers where unkown is present and removing them
rowNum <- which(blackcap$age_at_ringing == "Unknown")
cat("Number of rows with unknown:",length(rowNum))

## Strong unknown observations in a separate dataFrame
unknownObs <- blackcap[rowNum,]

## Removing unkonwn from the dataFrame
blackcap <- blackcap[-rowNum, ]



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

## Getting the rows numbers where unkown is present and removing them
rowNum <- which(chifchaf$age_at_ringing == "Unknown")
cat("Number of rows with unknown:",length(rowNum))

## Strong unknown observations in a separate dataFrame
unknownObs <- chifchaf[rowNum,]

## Removing unkonwn from the dataFrame
chifchaf <- chifchaf[-rowNum, ]



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

## Getting the rows numbers where unkown is present and removing them
rowNum <- which(robin$age_at_ringing == "Unknown")
cat("Number of rows with unknown:",length(rowNum))

## Strong unknown observations in a separate dataFrame
unknownObs <- robin[rowNum,]

## Removing unkonwn from the dataFrame
robin <- robin[-rowNum, ]


ro <- colSums(robin[,-ncol(robin)])
black <- colSums(blackcap[,-ncol(blackcap)])
chif <- colSums(chifchaf[,-ncol(chifchaf)])



## generating the month names from October 2007 to April 2018 
Months <- seq(as.Date("2007-10-01"), as.Date("2018-04-01"), by = "months")

# Exclude speci.robinfic months from the sequence
excluded_months <- grepl("May|June|July|August|September", format(Months, "%B"))

# Convert the Months vector to a sequence of month names
Months <- format(Months, format = "%B %Y")

# Create a new sequence of months that excludes the speci.robinfied months
Month <- Months[!excluded_months]

## Making a combined dataFrame for captured adults and juveniles
combData <- data.frame(Month = c(1:77), Blackcap = black, 
                       Chiffchaff = chif, Robin=ro)

## Reshape the data frame to a long format to plot
combData_long <- pivot_longer(combData, cols = c(Blackcap, Chiffchaff, Robin), 
                              names_to = "Group", values_to = "Captured")
## Adding colors to different species
custom_colors <- c("#1f78b4", "green3", "red")
## Using ggplot to plotting the captured birds over different months
p <- ggplot(combData_long, aes(x = Month, y = Captured, color = Group)) + 
  geom_point() + geom_line() +
  xlab("Months (October 2007 to April 2018)") +
  ylab("No. of Birds captured") +
  ggtitle("Different species captured over different sessions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5))

## Defining custom labels for x-axis
custom_breaks <- seq(1, 77, by = 5)
custom_labels <- Month[custom_breaks]  
## Appending labels to the graph
p <- p + scale_x_continuous(breaks = custom_breaks, labels = custom_labels)

## Adding the colors
p <- p + scale_color_manual(values = custom_colors)
## Printing the plot
print(p)


