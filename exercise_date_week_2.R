# Data Exploration and Preparation
# Dr. Muhammad lqbal

# Tutorial 2

# How to Handle Missing Data: Example Using the Cars Data Set
# Note: Lines beginning with "#" are comment lines explaining what we are doing. The R

# compiler skips these lines. Lines indented (e.g., Create a Histogram, below) are meant to be
# on the same line as the one above. A semicolon tells R to separate one line into two lines,

# one before and one after the semicolon.


# ================ Input data set Cars into Data Frame "cars" =================

cars <- read.csv(file = "cars.txt", stringsAsFactors = FALSE)
# Show the new Data Frame "cars"
cars
# (only the first seven records shown
# in output)

# ====================== Create subset of "cars" ==============================

# Use records 1 to 5, variables 1, 3, 4, and 8.
cars_tiny <- cars[1:5,c(1, 3, 4, 8)]
cars_tiny

# ======================= Replace the missing value with some constant ========
cars_tiny [2,2] <- cars_tiny[4.4] <-NA
cars_tiny[2,2] <- 0

cars_tiny[4.4] <- "Missing"
cars_tiny


  
# ============== Replace the missing value with the field mean or mode ========

# Recreate the missing value table
cars_tiny[2,2] <- cars_tiny[4,4] <- NA
# Replace cars_tiny[2,2] with cubicinches mean

cars_tiny[2,2] <- mean(na.omit(cars_tiny$cubicinches))

# Replace cars_tiny[4,4] with brand mode
our_table <- table(cars_tiny$brand)

our_mode <- names(our_table)[our_table == max(our_table)]
cars_tiny [4.4] <- our_mode
cars_tiny


# Replace missing values with a value generated at random from the observed distribution

# Recreate the missing value table
cars_tiny[2,2] <- cars_tiny[4,4] <- NA

# Generate random observation from
# observed distribution; results will vary
obs_brand <- sample(na.omit(cars_tiny$brand), 1)

obs_cubicinches <- sample(na.omit(cars_tiny$cubicinches), 1)

# Replace the missing values
cars_tiny[2,2] <- obs_cubicinches
cars_tiny[4,4] <- obs_brand
cars_tiny

# Five Number Summary with Mean
summary (cars$weight)

# Count
length(cars$weight)



# ============================== Create a histogram ===========================

# Input the cars2 dataset
cars2 <- read.csv(file = "cars2.txt", stringsAsFactors = TRUE)

# Set up the plot area
par(mfrow = c(1,1))

# Create the histogram bars
hist( cars2$weight,
      breaks = 30,
      xlim = c(0, 5000),
      col = "blue",
      border = "black",
      ylim = c(0, 40),
      xlab = "Weight",
      ylab = "Counts",
      main = "Histogram of Car Weights")

# Make a box around the plot
box(which = "plot",
    Ity = "solid",
    col="black")



# ========================== Create a scatterplot =============================
plot(cars2$weight, cars2$mpg,
     xlim = c(0, 5000),
     ylim = c(0, 600),
     xlab = "Weight",
     ylab = "MPG",
     main = "Scatterplot of MPG by Weight",
     type = "p",
     pch = 16,
     col = "blue")

#Add open black circles
points(cars2$weight,
       cars2$mpg,
       type = "p",
       col = "black")


# Natural Log transformation
natlog_weight <- log(cars$weight)
natlog_weight

# Inverse Square Root transformation
invsqrt_weight <- 1 / sqrt(cars$weight)
invsqrt_weight


# ========================== Min-Max Normalization ============================
mmnorm.weight <- (cars$weight -
                    min(cars$weight))/(max(cars$weight) - min(cars$weight))
mmnorm.weight

# =============================== Z-score =====================================
zscore.weight <- (cars$weight - mean(cars$weight))/sd(cars$weight)
zscore.weight


# ============================ Calculate skewness =============================
weight_skew <- (3*(mean(cars$weight) -
                     median(cars$weight))) / sd(cars$weight)

zscore.weight_skew <- (3* ( mean(zscore.weight) - median(zscore.weight))) /
  sd(zscore.weight)

weight_skew; zscore.weight_skew


# ============================ Find the skewness ==============================
Inweight_skew <- (3*(mean(natlog_weight) - median(natlog_weight))) / sd(natlog_weight)
Inweight_skew

# ============ Side-by-Side Histograms of Weight and Z-score of Weight =========

par( mfrow = c( 1,2))

hist( cars$weight, breaks = 30,
      xlim = c(1000, 5000),
      main = "Histogram of Weight",
      xlab = "Weight",
      ylab = "Counts")

box(which = "plot",
    Ity = "solid",
    col="black")

hist( zscore.weight,
      breaks = 30,
      xlim = c(-2, 3),
      main = "Histogram of Z-score of Weight",
      xlab = "Z-score of Weight",
      ylab = "Counts")

box(which = "plot",
    Ity = "solid",
    col="black")


# ============================ Normal probability plot =======================

par(mfrow = c(1,1))

qqnorm(invsqrt_weight,
       datax = TRUE,
       col = "red",
       ylim = c(0.01, 0.03),
       main = "Normal Q-Q Plot of Inverse Square Root of Weight")

qqline(invsqrt_weight,
       col = "blue",
       datax = TRUE)

# ================ Inverse Square Root skewness ================================
invsqweight_skew <- (3*(mean(invsqrt_weight) - median(invsqrt_weight)))/sd(invsqrt_weight)
invsqweight_skew



# ==== Create histogram with fitted Normal distribution


# Simulate from a Normal distribution
x <- rnorm(1000000, mean = mean(invsqrt_weight), sd = sd(invsqrt_weight))

par(mfrow = c(1,1))

hist(invsqrt_weight,
     breaks = 30,
     xlim = c(0.0125, 0.0275),
     col = "lightblue",
     prob = TRUE,
     border = "black",
     xlab = "Inverse Square Root of Weight",
     ylab = "Counts",
     main = "Histogram of Inverse Square Root of Weight")

box(which = "plot",
    Ity = "solid",
    col="black")
    lines(density (x),
    col = "red")


# =========== Create three flag variables for four categories =================
    
# Ten observations, with 999 as our placeholder for unassigned values
north_flag <- east_flag <- south_flag <- c(rep(999, 10))
# Create the variable region

region <- c(rep(c("north", "south", "east", "west"),2), "north", "south")

# Change the flag variable values to 0 or 1
for (i in 1:length(region)) {
  if(region[i] == "north") north_flag[i] = 1
  
  else north_flag[i] = 0
  if(region [i] == "east") east_flag[i] = 1 else
    east_flag[i] = 0
  if(region[i] == "south") south_flag[i] = 1
  else south_flag[i] = 0
}

north_flag; east_flag; south_flag
  
  
# ========================= Transforming the data ============================
x <- cars$weight[1 ]; x
# Transform x using y = 1 / sqrt(x)
y <- 1 / sqrt(x); y
# Detransform x using x = 1 / (y)^2
detransformedx <- 1 /y^2; detransformedx

# ==================== Find duplicate records in a data frame =================
anyDuplicated(cars)
# To examine each record, use Duplicated
duplicated(cars)
# "True' indicates a record which is a duplicate of previous records.

# 'False' indicates a record which is not a duplicate of previous records
# Duplicate the first record in 'data' to make new dataset
new.cars <- rbind(cars, cars[1,])
# Check for duplicates

anyDuplicated(new.cars)
# The 262nd record is a duplicate
duplicated(new.cars)
# True indicates a duplicate of a previous record


# =========================== Create an index field ===========================

# For data frames
# Data frames already have an index field;

# the left-most column
# The index of a record will stay with that
# record, even if the records are reordered.
cars
cars[order(cars$mpg),]


# For vectors or matrices
# Add a column to act as an index field
x <- c(1,1,3:1,1:4,3); y <- c(9,9:1)

z <- c(2,1:9)
matrix <- t(rbind(x,y,z)); matrix
indexed_m <- cbind(c(1:length(x)), matrix);
indexed_m
indexed_m[order(z), ]

# ============================== Binning =====================================
# Enter the dataset, call it xdata
xdata <- c( 1,1, 1,1,1,2,2,11,11,12,12,44)
# Get the sample size of the variable

n <- length(xdata)
#Declare number of bins and bin indicator
nbins <- 3
whichbin <- c(rep(0, n))

# Equal frequency
freq <- n/nbins

# Sort the data
xsorted <- sort(xdata)

for (i in 1:nbins) {
  for (j in 1:n) {
    if( (i-1 )* freq < j && j <=i*freq)
      whichbin[j] <- i
  }
}
whichbin
    

# K-means
kmeansclustering <- kmeans(xdata, centers = nbins)
whichbin <- kmeansclustering$cluster;
whichbin

# Equal width
range_xdata <- max(xdata) - min(xdata) + 1
binwidth <- range_xdata/nbins

for (i in 1:nbins) {
  for (j in 1:n) {
    if((i-1 )*binwidth < xdata[j] &&
       xdata[j] <= (i)*binwidth)
      whichbin[j] <- i
  }
}
whichbin

    
# Reference:
# Discovering Knowledge in Data: An Introduction to Data exploration, Second Edition, by
# Daniel Larose and Chantal Larose, John Wiley and Sons, Inc., 2014

