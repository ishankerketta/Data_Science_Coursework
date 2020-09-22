#loading required packages

library(stringr)
library(car)
library(MASS)
library(ggplot2)

#Loading dataset into R
carprice <- read.csv("CarPrice_Assignment.csv", na.strings = c("", NA))

str(carprice)

#DATA CLEANING

#Checking for duplicates

sum(duplicated(carprice$car_ID))
#no duplicates found

#Converting CarName into string variable
carprice$CarName <- as.character(carprice$CarName)

#Spliting CarName into Brand and Model
car_name <- str_split_fixed(carprice$CarName, pattern = " ", n = 2)
colnames(car_name) <- c("brand","model")
car_name <- data.frame(car_name)

#Checking for errors in brand name
summary(as.factor(car_name$brand))

#correcting spelling errors
car_name$brand[which(car_name$brand == "Nissan")] <- "nissan"
car_name$brand[which(car_name$brand == "porcshce")] <- "porsche"
car_name$brand[which(car_name$brand == "toyouta")] <- "toyota"
car_name$brand[which(car_name$brand == "vokswagen")] <- "volkswagen"
car_name$brand[which(car_name$brand == "vw")] <- "volkswagen"
car_name$brand[which(car_name$brand == "maxda")] <- "mazda"

#dropping unused levels
car_name$brand <- droplevels(car_name$brand)

#Combining car_name to carprice
carprice <- cbind(carprice, car_name)

#Dropping varibles
carprice$CarName <- NULL
carprice$model <- NULL
carprice$car_ID <- NULL #not relevant

#Checking price variable for Outliers

quantile(carprice$price, seq(0,1,0.01))

#There is a relatively high spike at 99% and 100%. Let's cap the price at 36809.60
carprice$price[which(carprice$price>36809.60)] <- 36809.60

#Creating Dummy variables with two categories

#Fuel Type: diesel = 1, gas = 0
levels(carprice$fueltype)<-c(1,0)
carprice$fueltype <- as.numeric(levels(carprice$fueltype))[carprice$fueltype]

#Aspiration: std = 0, turbo = 1
levels(carprice$aspiration)<-c(0,1)
carprice$aspiration <- as.numeric(levels(carprice$aspiration))[carprice$aspiration]

#Number of Doors: four = 1, two = 0
levels(carprice$doornumber)<-c(1,0)
carprice$doornumber <- as.numeric(levels(carprice$doornumber))[carprice$doornumber]

#Location of Engine: front = 1, rear = 0
levels(carprice$enginelocation)<-c(1,0)
carprice$enginelocation <- as.numeric(levels(carprice$enginelocation))[carprice$enginelocation]

#Creating Dummy variables with more than two categories

dummy_1 <- data.frame(model.matrix(~carbody, data = carprice))
dummy_1 <- dummy_1[,-1]
carprice <- cbind(carprice, dummy_1)
carprice$carbody <- NULL

dummy_2 <- data.frame(model.matrix(~drivewheel, data = carprice))
dummy_2 <- dummy_2[,-1]
carprice <- cbind(carprice, dummy_2)
carprice$drivewheel <- NULL

dummy_3 <- data.frame(model.matrix(~enginetype, data = carprice))
dummy_3 <- dummy_3[,-1]
carprice <- cbind(carprice, dummy_3)
carprice$enginetype <- NULL

dummy_4 <- data.frame(model.matrix(~cylindernumber, data = carprice))
dummy_4 <- dummy_4[,-1]
carprice <- cbind(carprice, dummy_4)
carprice$cylindernumber <- NULL

dummy_5 <- data.frame(model.matrix(~fuelsystem, data = carprice))
dummy_5 <- dummy_5[,-1]
carprice <- cbind(carprice, dummy_5)
carprice$fuelsystem <- NULL

dummy_6 <- data.frame(model.matrix(~brand, data = carprice))
dummy_6 <- dummy_6[,-1]
carprice <- cbind(carprice, dummy_6)
carprice$brand <- NULL

#Setting the seed to 100
set.seed(100)

# separating data into train and test
trainindices <- sample(1:nrow(carprice), 0.7*nrow(carprice))

train <- carprice[trainindices,]

test <- carprice[-trainindices,]

#Creating a Linear regression Model

model_1 <- lm(price~., data = train)
summary(model_1)


#Using stepAIC to eliminate independent variables
step <- stepAIC(model_1, direction = "both")
step

#Using the model
model_2 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + boreratio + stroke + peakrpm + 
                citympg + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelfwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + brandbmw + brandbuick + 
                branddodge + brandhonda + brandjaguar + brandmazda + brandmercury + 
                brandmitsubishi + brandnissan + brandplymouth + brandrenault + 
                brandsaab + brandtoyota + brandvolkswagen, data = train)
              
summary(model_2)

vif(model_2)


#curbweight and enginesize have high VIF. Let's check correlation
cor(train$curbweight, train$enginesize)

#They are highly correlated. Let's drop curbweight as higher p-value

model_3 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                enginesize + boreratio + stroke + peakrpm + 
                citympg + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelfwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + brandbmw + brandbuick + 
                branddodge + brandhonda + brandjaguar + brandmazda + brandmercury + 
                brandmitsubishi + brandnissan + brandplymouth + brandrenault + 
                brandsaab + brandtoyota + brandvolkswagen, data = train)

summary(model_3)

vif(model_3)

# carbodyhatchback and carbodysedan have high VIF. Let's check correlation

cor(train$carbodyhatchback, train$carbodysedan)
#They have high negative correlation. Let's drop carbodysedan as it has higher p-value

model_4 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                enginesize + boreratio + stroke + peakrpm + 
                citympg + carbodyhardtop + carbodyhatchback + 
                carbodywagon + drivewheelfwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + brandbmw + brandbuick + 
                branddodge + brandhonda + brandjaguar + brandmazda + brandmercury + 
                brandmitsubishi + brandnissan + brandplymouth + brandrenault + 
                brandsaab + brandtoyota + brandvolkswagen, data = train)

summary(model_4)

vif(model_4)

#carwidth and enginesize have high VIF. Let's check correlation
cor(train$carwidth, train$enginesize)

#They are highly correlated. Let's drop carwidth as it higher p-value

model_5 <- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + boreratio + stroke + peakrpm + 
                citympg + carbodyhardtop + carbodyhatchback + 
                carbodywagon + drivewheelfwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + brandbmw + brandbuick + 
                branddodge + brandhonda + brandjaguar + brandmazda + brandmercury + 
                brandmitsubishi + brandnissan + brandplymouth + brandrenault + 
                brandsaab + brandtoyota + brandvolkswagen, data = train)

summary(model_5)

vif(model_5)

#citympg has high VIF and high p-value. Let's discard it.

model_6 <- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + boreratio + stroke + peakrpm + 
                carbodyhardtop + carbodyhatchback + 
                carbodywagon + drivewheelfwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + brandbmw + brandbuick + 
                branddodge + brandhonda + brandjaguar + brandmazda + brandmercury + 
                brandmitsubishi + brandnissan + brandplymouth + brandrenault + 
                brandsaab + brandtoyota + brandvolkswagen, data = train)

summary(model_6)

vif(model_6)

#cylindernumberthree has very high p-value. Let's discard it.

model_7 <- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + boreratio + stroke + peakrpm + 
                carbodyhardtop + carbodyhatchback + 
                carbodywagon + drivewheelfwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + 
                fuelsystem2bbl + fuelsystemmpfi + brandbmw + brandbuick + 
                branddodge + brandhonda + brandjaguar + brandmazda + brandmercury + 
                brandmitsubishi + brandnissan + brandplymouth + brandrenault + 
                brandsaab + brandtoyota + brandvolkswagen, data = train)

summary(model_7)

vif(model_7)

#fuelsystem2bbl and fuelsystemmpfi have high VIF. fuelsystem2bbl is insignificant. Let's drop it.

model_8 <- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + boreratio + stroke + peakrpm + 
                carbodyhardtop + carbodyhatchback + 
                carbodywagon + drivewheelfwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + 
                fuelsystemmpfi + brandbmw + brandbuick + 
                branddodge + brandhonda + brandjaguar + brandmazda + brandmercury + 
                brandmitsubishi + brandnissan + brandplymouth + brandrenault + 
                brandsaab + brandtoyota + brandvolkswagen, data = train)

summary(model_8)

vif(model_8)

#boreratio has high VIF and is insignificant. Let's discard it.

model_9 <- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + stroke + peakrpm + 
                carbodyhardtop + carbodyhatchback + 
                carbodywagon + drivewheelfwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + 
                fuelsystemmpfi + brandbmw + brandbuick + 
                branddodge + brandhonda + brandjaguar + brandmazda + brandmercury + 
                brandmitsubishi + brandnissan + brandplymouth + brandrenault + 
                brandsaab + brandtoyota + brandvolkswagen, data = train)

summary(model_9)

vif(model_9)

#carbodywagon has high p-value. Let's drop it.

model_10 <- lm(formula = price ~ aspiration + enginelocation + 
                enginesize + stroke + peakrpm + 
                carbodyhardtop + carbodyhatchback + 
                drivewheelfwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + 
                fuelsystemmpfi + brandbmw + brandbuick + 
                branddodge + brandhonda + brandjaguar + brandmazda + brandmercury + 
                brandmitsubishi + brandnissan + brandplymouth + brandrenault + 
                brandsaab + brandtoyota + brandvolkswagen, data = train)

summary(model_10)

vif(model_10)

#brandsaab has high p-value. let's remove

model_11 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize + stroke + peakrpm + 
                 carbodyhardtop + carbodyhatchback + 
                 drivewheelfwd + enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + 
                 fuelsystemmpfi + brandbmw + brandbuick + 
                 branddodge + brandhonda + brandjaguar + brandmazda + brandmercury + 
                 brandmitsubishi + brandnissan + brandplymouth + brandrenault + 
                 brandtoyota + brandvolkswagen, data = train)

summary(model_11)

vif(model_11)

#Removing brandmecury as it has high p-value

model_12 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize + stroke + peakrpm + 
                 carbodyhardtop + carbodyhatchback + 
                 drivewheelfwd + enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + 
                 fuelsystemmpfi + brandbmw + brandbuick + 
                 branddodge + brandhonda + brandjaguar + brandmazda + 
                 brandmitsubishi + brandnissan + brandplymouth + brandrenault + 
                 brandtoyota + brandvolkswagen, data = train)

summary(model_12)

vif(model_12)

#enginesize still has high VIF. Checking for correlation between enginesize and drivewheelfwd
cor(train$enginesize, train$drivewheelfwd)
#there is moderately high negative correlation
#drivewheelfwd is insignificant as p-value is high. Let's remove it.

model_13 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize + stroke + peakrpm + 
                 carbodyhardtop + carbodyhatchback + 
                 enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + 
                 fuelsystemmpfi + brandbmw + brandbuick + 
                 branddodge + brandhonda + brandjaguar + brandmazda + 
                 brandmitsubishi + brandnissan + brandplymouth + brandrenault + 
                 brandtoyota + brandvolkswagen, data = train)

summary(model_13)

vif(model_13)


#Removing fuelsystemmpfi as it is insignificant

model_14 <- lm(formula = price ~ aspiration + enginelocation + enginesize + stroke + peakrpm + 
                 carbodyhardtop + carbodyhatchback + enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + brandbmw + brandbuick + 
                 branddodge + brandhonda + brandjaguar + brandmazda + 
                 brandmitsubishi + brandnissan + brandplymouth + brandrenault + 
                 brandtoyota + brandvolkswagen, data = train)

summary(model_14)

vif(model_14)

#Removing carbodyhatchback as it is insignificant

model_15 <- lm(formula = price ~ aspiration + enginelocation + enginesize + stroke + peakrpm + 
                 carbodyhardtop + enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + brandbmw + brandbuick + 
                 branddodge + brandhonda + brandjaguar + brandmazda + 
                 brandmitsubishi + brandnissan + brandplymouth + brandrenault + 
                 brandtoyota + brandvolkswagen, data = train)

summary(model_15)

vif(model_15)

#Removing enginetypel as it is insignificant

model_16 <- lm(formula = price ~ aspiration + enginelocation + enginesize + stroke + peakrpm + 
                 carbodyhardtop + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + brandbmw + brandbuick + 
                 branddodge + brandhonda + brandjaguar + brandmazda + 
                 brandmitsubishi + brandnissan + brandplymouth + brandrenault + 
                 brandtoyota + brandvolkswagen, data = train)

summary(model_16)

vif(model_16)

#Removing storke as it is insignificant

model_17 <- lm(formula = price ~ aspiration + enginelocation + enginesize + peakrpm + 
                 carbodyhardtop + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + brandbmw + brandbuick + 
                 branddodge + brandhonda + brandjaguar + brandmazda + 
                 brandmitsubishi + brandnissan + brandplymouth + brandrenault + 
                 brandtoyota + brandvolkswagen, data = train)

summary(model_17)

vif(model_17)

# Removing aspiration as it has only one star

model_18 <- lm(formula = price ~ enginelocation + enginesize + peakrpm + 
                 carbodyhardtop + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + brandbmw + brandbuick + 
                 branddodge + brandhonda + brandjaguar + brandmazda + 
                 brandmitsubishi + brandnissan + brandplymouth + brandrenault + 
                 brandtoyota + brandvolkswagen, data = train)

summary(model_18)

vif(model_18)

#Removing brandjaguar as it has only one star

model_19 <- lm(formula = price ~ enginelocation + enginesize + peakrpm + 
                 carbodyhardtop + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + brandbmw + brandbuick + 
                 branddodge + brandhonda + brandmazda + 
                 brandmitsubishi + brandnissan + brandplymouth + brandrenault + 
                 brandtoyota + brandvolkswagen, data = train)

summary(model_19)

vif(model_19)

#Removing variables with two stars:

#Removing peakrpm as it has two stars

model_20 <- lm(formula = price ~ enginelocation + enginesize + 
                 carbodyhardtop + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + brandbmw + brandbuick + 
                 branddodge + brandhonda + brandmazda + 
                 brandmitsubishi + brandnissan + brandplymouth + brandrenault + 
                 brandtoyota + brandvolkswagen, data = train)

summary(model_20)

vif(model_20)

#Removing carbodyhardtop as it has two stars

model_21 <- lm(formula = price ~ enginelocation + enginesize + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + brandbmw + brandbuick + 
                 branddodge + brandhonda + brandmazda + 
                 brandmitsubishi + brandnissan + brandplymouth + brandrenault + 
                 brandtoyota + brandvolkswagen, data = train)

summary(model_21)

vif(model_21)

#Removing enginetypedohcv as it has two stars

model_22 <- lm(formula = price ~ enginelocation + enginesize + 
                 enginetypeohcf + enginetyperotor + brandbmw + brandbuick + 
                 branddodge + brandhonda + brandmazda + 
                 brandmitsubishi + brandnissan + brandplymouth + brandrenault + 
                 brandtoyota + brandvolkswagen, data = train)

summary(model_22)

vif(model_22)

#All independent variables in model_22 have acceptable VIF and are highly significant (low p-values)
# Hence, we consider model_22 as our final model.

#removing price (dependent variable) from test dataset
test1 <- test[,-which(colnames(test)=="price")]

Predict1 <- predict(model_22, test1)
test$test_price <- Predict1

r <- cor(test$price, test$test_price)
rsquared <- cor(test$price, test$test_price)^2
rsquared

#Predicted price and Actual price is highly correlated. Hence it is a good model.
#R-squared is also high.

#checking Adjusted R-squared
summary(lm(test$price~test$test_price))
#Adjusted R-squared is high. 
#Difference between R-squared and Adjusted R-squared is minimal.
#It is a good model for prediction.


#Creating a variable to show error between actual price and predicted price
test$error <-  test$price - test$test_price


###Assessment using plots

#Plotting Actual Price and Predicted price on the same graph

ggplot(test, aes(x=1:62, y = price)) + geom_line(aes(color="blue")) + 
  geom_line(aes(x=1:62, y=test_price, colour="red"))
#Both plots more or less overlap each other. So, we can say that it is a good model.

#Plotting the errors

ggplot(test, aes(x= 1:62 , y = error)) + geom_point()
#Errors are random. This mean no highly significant variable has been left out.
# Hence it is a good model.

# model_22 can be recommended to Geely Auto
