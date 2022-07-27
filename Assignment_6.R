### Assignment 6

# 1. Import the data into R and change variables Brick and Neighborhood into factor variables.
# Eliminate variable Home ID from the original data frame.
library(tidyverse)
house <- read.csv("assignment1_dataset.csv")

house <- house %>%
  mutate(Brick = factor(Brick),
         Neighborhood = factor(Neighborhood))

house <- house[-1]

# 2. Use aggregate function to inspect the mean of Price grouped by variable Neighborhood.
# What is the average price of houses in North neighborhood?
# What is the average price of houses in West neighborhood? 
#  Hint: aggregate(x=dt[1], by=dt[7], FUN=mean)  
# Or      aggregate(dt[1],dt[7],mean)

aggregate(house[1], house[7], mean)
# average price of houses in North neighborhood is 110154.5
# avarage price of houses in West neighborhood is 159294.9

house %>%
  group_by(Neighborhood) %>%
summarize(AvgPrice = mean(Price))
# can also do this way

# 3. Overwrite variables Brick and Neighborhood into dummy variables.
# If Brick = “Yes”, we recode the variable value into 1, or else we recode it into 0.
# For Neighborhood variable, if neighborhood is west, we recode the variable value into 1, or else we recode it into 0. 
house$Brick <- ifelse(house$Brick == "Yes", 1, 0)
house$Neighborhood <- ifelse(house$Neighborhood == "West", 1, 0)

# 4. Generate a normalization function and apply this function to all variables in the House data frame.
# What are maximal and minimal values of SqFt? Confirm the range is now between zero and one.
MinMaxNormalize <- function(x){
  ifelse(is.numeric(x), return ((x - min(x)) / (max(x) - min(x))), return(x))}

house <- data.frame(lapply(house, MinMaxNormalize))
max(house$SqFt)
min(house$SqFt)
# min of Sqft is now 0, max is now 1

# 5. Randomize the normalized data frame and create training and testing samples on randomized data frame.
# Select the first 70% of houses as training sample (you can round the number using floor() or other functions) and the rest 30% of houses as testing sample.
set.seed(123)
train.index <- sample(1:nrow(house), floor(0.7*nrow(house)), replace = FALSE)

train <- house[train.index,]
test <- house[-train.index,]

# 6. Train a neuralnet model on training sample. Use 5 hidden neurons with Price as dependent variable
# and SqFt, Bedrooms, Bathrooms, Offers, recoded Brick and recoded Neighborhood as independent variables. 
# NOTE: in formula Price ~.  may not work. You may have to write down all columns on the right side, ie Price ~ SqFT + Bedrooms +…
library(neuralnet)
nmodel <- neuralnet(Price ~ SqFt + Bedrooms + Bathrooms + Offers + Brick + Neighborhood, data = train, hidden = 5, linear.output = TRUE)

# 7. Visualize the network topology by plotting the model. How many hidden nodes are included in the model?
# How many steps were used to build the model? What is the error value?
plot(nmodel)
# 5 hidden nodes are included in the model
# 465 steps were used to build the model. Error value is 0.134

# 8. Obtain the model prediction by applying the model on testing sample. Get the predictions of house price
# and examine the correlation between prediction and actual house price from testing sample. What is the correlation between these two variables? 
model_result <- compute(nmodel, test[-1])
predictions <- model_result$net.result

cor(predictions, test$Price)
# correlation between predictions and actual house price is 0.89

# 9. Make a residual plot (i.e. residuals on y-axis and fitted values on x-axis). Do you see any trend? 
#  Hint: residuals<- dt.test$Price - predicted$net.result
residuals <- test$Price - predictions
plot(residuals)
plot(x = predictions, y = residuals)
# the residuals appear to be randomly distributed (i.e., no clear trend to the data). There do appear to be a couple of outliers, though.

# 10.	Now run the model using SVM on training dataset and use kernel='vanilladot'. Use this model to predict Price using testing dataset.
# Examine the correlation between prediction and actual house price from testing sample. What is the correlation between these two variables? 
library(kernlab)
SVM <- ksvm(Price ~., data = train, kernel = 'vanilladot')
predicted <- predict(SVM, test[-1])
cor(predicted, test$Price)
# correlation between predictions and actual house price is 0.90


### Support Vector Machine Example: Car Evaluation

# 1. Import the data into R and change all the variables into factor variables.
# You can directly change the variables into factor variables by setting “stringsAsFactors = TRUE” when you import the data.
# Inspect the data structure of the data. How many vehicles are included in this data set?
cars <- read.csv("assignment2_dataset.csv", stringsAsFactors = T)
str(cars)
# there are 1728 cars in this data set

# 2. How many vehicles have better evaluation than acceptable (not include acceptable)?
# For cars with big luggage, what proportion of “very good” evaluations do these cars have?
# For all the vehicles having “very good” evaluations, what proportion of median safety do these cars have?
# How many vehicles were bought in very high price and have good evaluation?
# How many vehicles have high maintenance price and have very good overall evaluation?
cars %>%
  group_by(class) %>%
  summarize(n = n())
# 134 cars have evaluation > acc

table(cars$class, cars$lug_boot)
# for cars with lug_boot = big, proportion of "very good" evaluation is 6.9%

cars %>%
  group_by(class, safety) %>%
  summarize(n = n())
# vehicles with "very good" evaluation all have high safety. 0% proportion of median safety.

table(cars$class, cars$buying)
# 0 cars bought at very high price with "good" evaluation

table(cars$class, cars$maint)
# 13 cars with high maintenance and very good evaluation

# 3. Randomize the data set and generate training and testing samples on the randomized dataset.
# Select the first 70% of the vehicles as training sample (use floor () function or other functions to round the number)
# and the rest 30% of the vehicles as testing sample. 
set.seed(123)
cars.train.index <- sample(1:nrow(cars), floor(0.7*nrow(cars)), replace = FALSE)

cars.train <- cars[cars.train.index,]
cars.test <- cars[-cars.train.index,]

# 4. Build a Support Vector Machine model on training sample with class variable as outcome variable and the rest attributes as predictors.
# Build this model using kernel = “vanilladot” and other default parameters. Look at the basic information about the model.
# How many support vectors does this model generate? What is the training error of this model?
cars.model <- ksvm(class ~., data = cars.train, kernel="vanilladot")
cars.model
# model generates 385 support vectors. Training error is 0.066

# 5. Evaluate the model. Use the model to predict the class labels of testing sample. Then use table or cross table to evaluate the performance.
# How many vehicles in total have been misclassified? How many vehicles have been classified as unacceptable are actually good? 
cars.predicted <- predict(cars.model, cars.test[-7], type = "response")
library(gmodels)
CrossTable(cars.test$class,cars.predicted)
# 43 total vehicles have been misclassified. 0 vehicles classified as unacceptable are actually "good."

# 6. Evaluate the model in another way. Look only at agreement vs. non-agreement and construct a vector of TRUE/FALSE indicating correct/incorrect predictions.
# How many “TRUE” agreements do the model generate for testing sample? What percentage does the “TRUE” agreements have among all agreements?
agree <- cars.predicted == cars.test$class
table(agree)
# model generates 475 "TRUE" agreements, or 91.7%

# 7. Build a new Support Vector Machine model on training sample with class variables as outcome variable and the rest attributes as predictors.
# Build this model using kernel = "rbfdot" and other default parameters. How many support vectors does this model generate? What is the training error of this model?
cars.model2 <- ksvm(class ~., data = cars.train, kernel="rbfdot")
cars.model2
# model generates 580 support vectors. Training error is 0.033

# 8. Evaluate the new model. Use the model to predict the class labels of testing sample. Then use table or cross table to evaluate the performance.
# How many vehicles in total have been misclassified? How many vehicles have been classified as good are actually unacceptable? 
cars.predicted2 <- predict(cars.model2, cars.test[-7], type = "response")
CrossTable(cars.test$class,cars.predicted2)
# 35 total vehicles have been misclassified. 2 vehicles classified as "good" are actually "unacceptable"
  