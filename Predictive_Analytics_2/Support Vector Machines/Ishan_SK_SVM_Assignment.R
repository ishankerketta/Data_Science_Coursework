### HANDWRITTEN DIGIT RECOGNITION ###

# 1. Business understanding

#We have the MNIST data, which comprises of a collection handwritten digits ranging from 0 to 9.
#The objective is to develop a model to identify the digit

#Each handwritten digit is represented in the data in the form of pixels
#Each digit is enclosed within 785 pixels (0 to 784) in a 28X28 pixels frame.
#Each value in the cells for columns 2 to 785 refer to pixel intensity values.


#2. Data Preparation and Data Understanding

#Install required packages

#install.packages("caTools")
#install.packages("caret")
#install.packages("kernlab")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("ggplot2")
#install.packages("gridExtra")

#Loading required packages
library(caTools)
library(readr)
library(dplyr)
library(kernlab)
library(caret)
library(ggplot2)
library(gridExtra)

#Extracting the MNIST data
train <- read_csv("mnist_train.csv", col_names = FALSE)
test <- read_csv("mnist_test.csv", col_names = FALSE)

#Finding the dimensions of the train dataset
dim(train)
# There are 60000 rows and 785 columns

#Structure of training data
str(train)

#Viewing the first few rows of training data
View(head(train))

#The first column represents the class 
# i.e. whether the digit is 0,1,2,3,4,5,6,7,8,9  
#For the remaining 784 columns, ranging from 0 to 783,
#each columns represent a pixel of the 28X28 image of the digit to be identified. 

#-------------------------------------------------------------------------------------#

#Taking a sample from training data for ease in model building
#Using 15% of the train data

set.seed(100)
train_id <- sample.split(train$X1, SplitRatio = 0.15)
train <- train[train_id,]

#Checking the dimensions of new training data
dim(train)
#There are 9000 observations
#There are 785 columns. 
#Taking a sample of 15% of test data for ease of computation

set.seed(100)
test_id <- sample.split(test$X1, SplitRatio = 0.15)
test <- test[test_id,]

#Checking the dimensions of new training data
dim(test)
#There are 1500 observations
#There are 785 columns.


#renaming the digit variable
colnames(train)[1] <- "digit"
colnames(test)[1] <- "digit"

#Converting digit column to factor variable
train$digit <- as.factor(train$digit)
test$digit <- as.factor(test$digit)

#Looking at the summary statistics of digits in train data
summary(train$digit)

#Checking for NA's
sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

#There are no NA values

#Checking for duplicated rows
sum(duplicated(train)) # no duplicate rows
sum(duplicated(test)) # no duplicate rows

#Find maximum value of pixel intensity
max(train[ ,2:ncol(train)]) #255

#Scaling the data
train[ , 2:ncol(train)] <- train[ , 2:ncol(train)]/255
test[ , 2:ncol(test)] <- test[ , 2:ncol(test)]/255


# 3. EXPLORATORY ANALYSIS

#Using the pixel intensity values of a row to form an image

digit <- matrix(as.numeric(train[4,-1]), nrow = 28) #looking at one digit
image(digit, col = grey.colors(255))
#Looks like the digit '3'
#This is how one of the samples looks like

#Looking at the frequency of digits in the training data

train %>%
  group_by(digit) %>%
  summarise(frequency = n()) %>%
  arrange (digit)

# Distribution of digits across train and test data

plot1 <- ggplot(train, aes(x = digit, y = (..count..)/sum(..count..))) + geom_bar() +
  labs(y = "Digit Frequency", title = "Training Data") + 
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.15)) +
  geom_text(stat = "count", 
            aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))

plot1

plot2 <- ggplot(test, aes(x = digit, y = (..count..)/sum(..count..))) + geom_bar() +
  labs(y = "Digit Frequency", title = "Testing Data") + 
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.15)) +
  geom_text(stat = "count", 
            aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))

plot2


# 4. MODEL BUILDING AND MODEL EVALUATION

#Using Linear Kernel
model_linear <- ksvm(digit~ ., data = train, scaled = FALSE, kernel = "vanilladot")

#evaluation of the model on test data
evaluation_linear<- predict(model_linear, test)

#Confusion matrix - Linear Kernel
confusionMatrix(evaluation_linear,test$digit)
#Accuracy: 91.6%

#Using Cross validation with Linear Kernel

set.seed(80)
grid_linear <- expand.grid(C=c(0.01,0.1,1,5,10)) # defining range of C

fit.linear <- train(digit ~ ., data = train, metric = "Accuracy", method = "svmLinear",
                    tuneGrid = grid_linear,
                    trControl = trainControl(method = "cv", number = 2, allowParallel = TRUE))

# printing results of 5 cross validation
print(fit.linear)
#Final value of C = 0.1

#Plotting the Accuracies against C values
plot(fit.linear)

#evaluation of the model on test data
evaluation_linear_cv<- predict(fit.linear, test)
confusionMatrix(evaluation_linear_cv, test$digit)
#Accuracy: 88.67%

#---------------------#

#Using RBF Kernel
model_RBF <- ksvm(digit~ ., data = train, scaled = FALSE, kernel = "rbfdot")

#evaluation of the model on test data
evaluation_RBF<- predict(model_RBF, test)

#confusion matrix - RBF Kernel
confusionMatrix(evaluation_RBF,test$digit)

#Accuracy: 95.73%

### RBF using CROSS VALIDATION

### Tuning Hyperparameters and using Cross Validation ###

#selecting sigma and C values
set.seed(80)
grid_rbf <- expand.grid(.sigma=c(0.001,0.01,0.1,1), .C=c(1,2,5,10,15))

fit.rbf <- train(digit~., data=train, method="svmRadial", metric="Accuracy", 
                 tuneGrid=grid_rbf, trControl = trainControl(method="cv", number=2, 
                                                             verboseIter = TRUE, allowParallel = TRUE))


# Printing cross validation result
print(fit.rbf)
# Best tune at C=10, sigma = 0.01

# Plotting Accuracies against sigma values
plot(fit.rbf)


# Validating the RBF model after cross validation on test data
evaluation_RBF_cv<- predict(fit.rbf, test)
confusionMatrix(evaluation_RBF_cv, test$digit)

#Accuracy: 95.93%


##Using Polynomial Kernel##

model_poly <- ksvm(digit ~ ., data = train, kernel = "polydot", scaled = FALSE, C = 1, 
                    kpar = list(degree = 1, scale = 1, offset = 1))

#evaluation of the model on test data
evaluation_poly <- predict(model_poly, test)
confusionMatrix(evaluation_poly, test$digit)

# Accuracy: 91.53%

##Polynomial Kernel with Cross validation

##Tuning hyperparameters

set.seed(80)
grid_poly = expand.grid(C= c(0.01, 0.1, 1), degree = c(1, 2, 3), 
                        scale = c(-1, 1, 10))

fit.poly <- train(digit ~ ., data = train, metric = "Accuracy", method = "svmPoly",tuneGrid = grid_poly,
                  trControl = trainControl(method = "cv", number = 2, verboseIter = TRUE))

# printing cross validation results
print(fit.poly)
#optimal values after cross validation:
# degree = 2, scale = 10, C = 0.01

# plotting Accuracies
plot(fit.poly)

#evaluation of the model on test data
evaluation_poly_cv <- predict(fit.poly, test)
confusionMatrix(evaluation_poly_cv, test$digit)

# Accuracy: 96.13%

#-----------------------#

## FINAL MODEL ##

#Hence, the final model to be accepted is the SVM model with polynomial kernel
#as it gives the highest Accuracy of 96.13% 
#Hyperparameters: degree = 2, scale = 10, C = 0.01 

model_final <- ksvm(digit ~ ., data = train, kernel = "polydot", scaled = FALSE, C = 0.01, 
                   kpar = list(degree = 2, scale = 10, offset = 1))

#Running confusionMatrix again for easy reference of results

evaluation_final <- predict(model_final, test)
confusionMatrix(evaluation_final, test$digit)

# It has Accuracy of 96.13%
# Sensitivites > 90%
# Specificities > 99%

#-----------------------#
