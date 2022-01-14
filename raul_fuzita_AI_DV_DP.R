#@markdown ???? Please, run this cell to import the data set from a source available at [https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/]("https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/)
df <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv")

#@markdown If the data set is imported correctly you should see the five first rows.
head(df)

install.packages("gplots")
library(gplots)

install.packages("reshape2")
library(reshape2)

library(dplyr)
library(ggplot2)
library(tidyverse)

install.packages("DescTools")
library(DescTools)

install.packages("caret")
library(caret)

#@markdown ???? Please, run this cell to create custom function that will be used later on. I cannot stress enough the importance of running this cell. Make sure you have run this code block. <font color="red">(Important)</font>

# https://stackoverflow.com/questions/44050028/min-max-scaling-normalization-in-r-for-train-and-test-data
mmnorm <- function(x, na.rm = TRUE) {
  ((x - min(x)) / (max(x) - min(x)))
}

zscore <- function(x, na.rm = TRUE){
  (x - mean(x))/sd(x)
}

robust_scalar <- function(x, na.rm = TRUE){
  (x - median(x)) /(quantile( x,probs =.75) - quantile( x,probs = .25))
}


#@markdown ???? Please, run this cell to look for NA data. This is an important step in cleaning a data set. A TRUE value is printed if there is any NA value in the data set. Otherwise FALSE.

#@markdown The function to check for any NA value is `any(is.na(df))`
any(is.na(df))


#@markdown ???? Please run this cell. You can also achieve a similar result by running the function `sum(is.na(df))`. This function will sum all the values that contain NA.

#@markdown If there is no NA value you should see a zero. Otherwise the sum of all the occurrences.

# https://datacornering.com/check-if-a-column-has-a-missing-values-na-in-r/
sum(is.na(df))

#@markdown ???? Run this cell to show the following values of a vector shifted by 1.

#@markdown `c(100, 10, 5, 2, 1, 0.5, 0.1, 0.05, 0.01, 0.001, 0.0001)`

# https://www.programmingr.com/tutorial/log-in-r/
# log in R - vector transformation 
v = c(100,10,5,2,1,0.5,0.1,0.05,0.01,0.001,0.0001)
q=log(v+1)
q

#@markdown ???? Run this cell to plot the difference between the vector values before and after the logarithm transformation.

# https://stackoverflow.com/questions/6774339/r-how-do-i-put-two-box-plots-next-to-each-other-keeping-same-y-range-for-both
par(mfrow = c(1, 2))
plot(v, pch=16, col="red", main="No Logarithm Transformation\napplyed")
plot(q, pch=16, col="red", main="Vector Shifted by 1")


#@markdown ???? Run this cell to shift the area variable values by 2.

#@markdown Notice that there is not any 0.0 values.

area.log = log(df$area+2)
area.log


#@markdown ???? Run this cell to plot the difference between the area values before and after the logarithm transformation.

#@markdown By transforming area values we can see on the right plot the dots more spread in the graph.

par(mfrow = c(1, 2))
plot(df$area, pch=16, col="red", main="No Logarithm Transformation\napplyed", ylab="Area in ha")
plot(area.log, pch=16, col="red", main="Variable Shifted by 2", ylab="Area with logarithm transformation")


#@markdown ???? Please, run this cell to retrive all the categorical data and save it in a object called categorical. See below grouped the categorical data.

categorical <- df[lapply(colnames(df), function(x) class(df[[x]]) == 'character') == TRUE]
head(categorical)


#@markdown ???? Please, run this cell to first convert the first letter of each string to upper case. The built-in package and DescTools rely on three-letter abbreviations.

#@markdown Once the values of the variables are converted to the right format they will be converted to integer numbers.

categorical$month <- gsub("\\b([a-z])", "\\U\\1", categorical$month, perl=TRUE)
categorical$day <- gsub("\\b([a-z])", "\\U\\1", categorical$day, perl=TRUE)

month.num <- match(categorical$month, month.abb)
day.week.num <- match(categorical$day, day.abb)

numericalDate <- data.frame(
  month = categorical$month,
  month.num = month.num,
  day = categorical$day,
  day.week.num = day.week.num
)


#@markdown ???? Please, run this cell to show the month, day, and their respective values.

head(numericalDate, 10)


#@markdown ???? Please, run this cell to take a closer look at the data.

head(df)

#@markdown ???? Please, run this cell to show the dimention of the data set.

dim(df)


#@markdown ???? Please, run this cell to show summary of the data set.

summary(df)


#@markdown ???? Please, run this cell to plot the burned area in hectares and tranformed with logarithm tranformation.

par(mfrow = c(1, 2))
hist(df$area, main="Burned area (in hectares)", xlab="")
hist(area.log, main="Log(area+2)", xlab="")


#@markdown ???? Please, run this cell to show the correlation of a variable agaist other.

defcorr <- cor(df[,5:13])
defcorr


#@markdown ???? Please, run this cell to plot the correlation results in a heatmap.

colorheatmap <- colorRampPalette(c("darkred", "orange", "yellow"))
heatmap(defcorr, Rowv=NA, Colv=NA, col=colorheatmap(20), margins = c(12, 12), main="Correlation between variables", xlab="Heatmap")
legend(x="right", legend=c("-1", "-0.5", "0", "0.5", "1"), fill=colorheatmap(5))


#@markdown ???? Please, run this cell to plot a boxplot for each variable.

boxplot(df[,5:13])


#@markdown ???? Please, run this cell to extract the numerical values from the data set.

df_numerical <- df[,5:11]
df_numerical <- cbind(df_numerical, area.log)


#@markdown ???? Please, run this cell to apply PCA for data reduction.

df_pca <- prcomp(df_numerical[,c(5,8)], center=TRUE, scale.=TRUE)
summary(df_pca)


#@markdown ???? Please, run this cell to plot a bar plot comparing the PCAs.

plot(df_pca, main="Result comparison between PCAs", xlab="PC1 (left), and PC2 (right)", col='lightblue')


#@markdown ???? Please, run this cell to show a biplot of all PCAs for the variables log(area+2) and temperature

biplot(df_pca, choices=c(1,2))


#@markdown ???? Please, run this cell to take a look at the current data that will be used for our model which eventually will be used for the data prediction.

head(df_numerical)


#@markdown ???? Please, run this cell to partition the data set and then create the model.

#@markdown 80% of the selected data destined to the training set and 20% to the testing set.

set.seed(100)

TrainingIndex <- createDataPartition(df_numerical$area.log, p=0.8, list=FALSE)
TrainingSet <- df_numerical[TrainingIndex, ]
TestingSet <- df_numerical[-TrainingIndex, ]

Model <- train(area.log ~ ., data=TrainingSet,
               method="lm",
               na.action = na.omit,
               preProcess=c("scale", "center"),
               trControl=trainControl(method="none"))


#@markdown ???? Please, run this cell to start training the model and then test it.

Model.training <- predict(Model, TrainingSet)
Model.testing <- predict(Model, TestingSet)


#@markdown ???? Please, run this cell to plot the results of the training set and testing set.

plot(TrainingSet$area.log, Model.training, main="Training Set to find burned area", xlab="log(area+2)", col="blue")
plot(TestingSet$area.log, Model.testing, main="Testing Set to find burned area", xlab="log(area+2)", col="blue")


#@markdown ???? Please, run this cell to show a summary of the model.

summary(Model)


#@markdown ???? Please, run this cell to inverse the square root of the area variable (log(area+2)).

# Inverse Square Root transformation
invsqrt_area <- 1 / sqrt(area.log)
invsqrt_area



#@markdown ???? Please, run this cell to calculate the Z-Score and plot the result in histogram plot.
zscore_area <- zscore(area.log)

hist(zscore_area,
     breaks = 30,
     xlim = c(-0.8, 2.8),
     ylim = c(0, 40),
     main = "Histogram of Z-score of burned area \nlog(area+2)",
     xlab = "Z-score of area",
     ylab = "Counts")

box(which = "plot",
    Ity = "solid",
    col="black")


#@markdown ???? Please, run this cell to plot the Normal Q-Q of Inverse Square Root of burned area.

par(mfrow = c(1,1))

qqnorm(invsqrt_area,
       datax = TRUE,
       col = "red",
       ylim = c(0.42, 1),
       main = "Normal Q-Q Plot of Inverse Square Root of burned area \nlog(area+2)")

qqline(invsqrt_weight,
       col = "blue",
       datax = TRUE)


#@markdown ???? Please, run this cell to plot a histogram with fitted normal distribution.

x <- mmnorm(invsqrt_area)

par(mfrow = c(1,1))

hist(invsqrt_area,
     breaks = 30,
     xlim = c(0.4, 1.2),
     ylim = c(0, 2.4),
     col = "lightblue",
     prob = TRUE,
     border = "black",
     xlab = "Inverse Square Root of burned area \nlog(area+2)",
     ylab = "Counts",
     main = "Histogram of Inverse Square Root of burned area")

box(which = "plot",
    Ity = "solid",
    col="black")
lines(density(x),
      col = "red")