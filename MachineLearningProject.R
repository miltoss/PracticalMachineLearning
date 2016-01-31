
rm(list = ls())
setwd("~/Documents/Coursera/JHDSS/8.PracticalMachineLearning/Project")

library(ggplot2)
library(caret)
library(rpart)
library(dplyr)
library(rattle)
library(randomForest)
library(e1071)

t <- read.csv("pml-training.csv")
dim(t)

t <- t[, -c(1,2,3,4,5)]

# Get the names of the variables
mynames <- names(t)

dump_cols <- NULL
for (i in 1:length(mynames) ) {
    s <- sum(is.na(t[, i]))
    s <- s / nrow(t)
    if (s > 0.5) {dump_cols <- c(dump_cols, i)}
}
dump_cols

t <- t[, -dump_cols]


mynames <- names(t)
keepvars <- mynames[!grepl( "kurtosis", mynames) & !grepl( "skewness", mynames) & !grepl( "amplitude", mynames) & !grepl( "max", mynames) & !grepl( "min", mynames)]

t <- t[,  keepvars]


# Decision tree



set.seed(26577)

inTrain = createDataPartition(t$classe, p = 0.5)[[1]]
training = t[ inTrain,]
testing = t[-inTrain,]
dim(training); dim(testing)


modFit <- train(classe ~ .,method = "rpart", data = training)

fancyRpartPlot(modFit$finalModel)
pred <- predict(modFit, newdata = testing)

confusionMatrix(pred, testing$classe)

# Fit random forest

inTrain = createDataPartition(t$classe, p = 0.5)[[1]]
training = t[ inTrain,]
testing = t[-inTrain,]
dim(training); dim(testing)

modFit2 <- train(classe ~ roll_belt + num_window + magnet_dumbbell_y + magnet_dumbbell_z, method = "rf", data = training)
pred2 <- predict(modFit2, newdata = testing)
confusionMatrix(pred2, testing$classe)

modFit3 <- train(classe ~ roll_belt + num_window + magnet_dumbbell_y  + pitch_forearm + roll_forearm, method = "rf", data = training)
pred3 <- predict(modFit3, newdata = testing)
confusionMatrix(pred3, testing$classe)


modFit4 <- train(classe ~ roll_belt + num_window + magnet_dumbbell_y + magnet_dumbbell_z + pitch_forearm,method = "gbm", data = training, verbose = FALSE)
pred4 <- predict(modFit4, newdata = testing)
confusionMatrix(pred4, testing$classe)


modFit5 <- svm(classe ~ roll_belt + num_window + magnet_dumbbell_y + magnet_dumbbell_z + pitch_forearm, data = training)
pred5 <- predict(modFit5, newdata = testing)
confusionMatrix(pred5, testing$classe)

modFit6 <- train(classe ~ roll_belt + num_window + magnet_dumbbell_y + magnet_dumbbell_z + pitch_forearm,method = "glm", data = training)
pred5 <- predict(modFit5, newdata = testing)
confusionMatrix(pred5, testing$classe)

#


score <- read.csv("pml-testing.csv")


predict(modFit, newdata = score)
predict(modFit2, newdata = score)
predict(modFit3, newdata = score)

