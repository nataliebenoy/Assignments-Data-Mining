# Boston Housing dataset is fetched from UCI machine learning repository. It contains 506 observations, 13 continuous attributes and 1 binary-valued attribute. Here is the detailed attribute information:
# 1. CRIM      per capita crime rate by town
# 2. ZN        proportion of residential land zoned for lots over 25,000 sq.ft.
# 3. INDUS     proportion of non-retail business acres per town
# 4. CHAS      Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
# 5. NOX       nitric oxides concentration (parts per 10 million)
# 6. RM        average number of rooms per dwelling
# 7. AGE       proportion of owner-occupied units built prior to 1940
# 8. DIS       weighted distances to five Boston employment centers
# 9. RAD       index of accessibility to radial highways
# 10. TAX      full-value property-tax rate per $10,000
# 11. PTRATIO   pupil-teacher ratio by town
# 12. B        1000(Bk - 0.63)^2 
# 13. LSTAT    % lower status of the population
# 14. MEDV     Median value of owner-occupied homes in $1000's
#  The objective is to explain the Median home value in Boston as a function of its characteristics.


# 1.	Import Boston Housing dataset from UCI machine learning repository: assignment4.csv 
housing <- read.csv("assignment4.csv")

# 2.	Use str() to inspect the type and data frame of variables. How many observations and how many variables in this dataset? Learn the meaning of each variable.
str(housing)
# there are 506 observations with 14 variables.

# 3.	Reform the CHAS to nominal (factor) variable. How many levels does CHAS have? How many instances belong to level 0? 
library(tidyverse)
housing <- housing %>%
  mutate(CHAS = factor(CHAS))

summary(housing$CHAS)
# CHAS has 2 levels, coded 0 & 1. Level 0 has 471 observations.

# 4.	What is the mean value of age variable? Since age is the proportion of homes built before 1940, how do you interpret this mean value? What is the maximum value of MEDV? Build a histogram for MEDV, which value range has the highest frequency?
mean(housing$AGE)
# mean of AGE is 68.57. In the dataset, an average of 68.57 percent of homes were built before 1940.

max(housing$MEDV)
# max value of MEDV is 50.

ggplot(housing, aes(x = MEDV)) + geom_histogram(binwidth = 2)
# MEDV values between 19-21 occur with the highest frequency

# 5.	Check the correlation between all the variables except for CHAS. Which pairs have correlations larger than 0.6? What variables have correlations with MEDV larger than 0.6?
correlation <- cor(housing [-4])

index <- which(abs(correlation) > 0.6 & abs(correlation) < 1,
               arr.ind = T)
(high_correlation <- cbind.data.frame(variable1 = rownames(correlation)[index[,1]],
                 variable2 = colnames(correlation)[index[,2]]))
# 17 pairs have correlations > 0.6.
# RM & LSTAT have correlations with MEDV > 0.6

# 6.	What are your assumptions of the relations between crime and home value, between DIS and home value? Does higher pupil-teacher ratio lead to lower home value? Evaluate your assumptions with the results from correlation. 
correlation[13,1]
# CRIM and MEDV have a correlation of -0.39, which indicates that as crime increases, median home value decreases on average.

correlation[13,7]
# DIS and MEDV have a correlation of 0.25, which indicates that as distance to employement centers increases, home value increases somewhat on average.

correlation[13,10]
# PTRATIO and MEDV have a correlation of -0.51, which indicates that as the pupil-teacher ratio increases, median home value decreases on average.

# 7.	Use pairs.panels() to visualize the relations between "RM", "LSTAT" and "MEDV". What are the histograms depicting on the diagonal? Does more room lead to higher home values? Does higher percentage of lower status of the population cause higher home values?
library(psych)
pairs.panels(housing[c("RM", "LSTAT", "MEDV")])
# histograms on the diagonal depict the spread of each variable on its own, in this case that LSTAT and MEDV are right skewed, which RM is closer to being normally distributed
# more rooms is associated with an increase in the value of MEDV
# higher values of LSTAT are associated with lower values of MEDV. To say higher LSTAT "causes" lower MEDV would be incorrect.

# 8.	Randomize the dataset and use the first 80% of observations for training. Set seed as 1234. 
rows <- nrow(housing)
train.size <- floor(rows * 0.80)

set.seed(1234)
train.index <- sample(1:rows, train.size, replace = F)

train <- housing[train.index,]
test <- housing[-train.index,]

# 9.	Build a linear regression model with MEDV as dependent variable and the rest variables as independent variables. Which variables have insignificant impact on MEDV? 
model <- lm(MEDV ~., data = train)
summary(model)
# you didn't specify significance at what level... Variables that are not significant at the p < .05 level include INDUS and AGE.

# 10.	How to interpret the estimate for variable RM? What is the p-value? Is this variable statistically significant? Why? 

# interpretation of RM is that as the number of rooms increases by 1 unit, MEDV increases by 4.26 on average, all other things equal.
# p-value for RM coefficient is < 2e-16 (basically 0), which indicates this variable is statistically significant at pretty much any level.
# because 2e-16 < 0.001, we say this coefficient is statistically significant at the p < 0.001 level.

# 11.	What are the R-squared value and adjusted R-squared value?
# multiple R-squared is 0.76
# adjusted R-squared is 0.75

# 12.	Try Crim^2 as one of the dependent variable and build a liner regression model include this variable (all other variables included). Does this Crim^2 have significant impact on MEDV? 
# Assuming you mean independent variable and not dependent variable...
train2 <- train %>%
  mutate(CRIM2 = CRIM^2)

model2 <- lm(MEDV ~., data = train2)
summary(model2)
# all other things equal, CRIM^2 does not have a statistically significant impact on MEDV at any significance level that people actually use (i.e p < .05)

# 13.	Also create a new tax2 variable with tax2 = 1 if TAX >=350 and tax2 = 0 if TAX < 350. Build a linear regression model include this tax2 (all other variables included). What is the adjusted R-squared value?
train2 <- train2 %>%
  mutate(tax2 = ifelse(TAX >= 350, 1, 0))

model3 <- lm(MEDV ~., data = train2)
summary(model3)
# adjusted R-squared value is 0.75

# 14.	Create a new simple regression model and estimate MEDV using only CHAS and CRIM. 
simple.model <- lm(MEDV ~ CHAS + CRIM, data = train)
summary(simple.model)

# 15.	How to interpret the estimate for variable CHAS1? What is the p-value? Is this variable statistically significant? Why? 
# interpretation: when value of CHAS goes from 0 (not on river) to 1 (on river), MEDV increases by 5.46 on average (controlling for crime)
# p value of CHAS1 is 0.0007, which is < .001. This variable is considered statistically significant at the p < .001 level.

# 16.	Calculate MAE using the function as discussed in class using training dataset. Also, calculate the correlation between predicted and the actual using training dataset
MAE <- function (v1, v2) mean(abs(v1-v2))

predictions <- predict(model, train)
MAE(train$MEDV, predictions)
# MAE is 3.17 for train set

cor(predictions, train$MEDV)
# correlation between predictions and actual values is 0.87 for train set

# 17.	Calculate MAE and correlation using testing dataset. 
test.predictions <- predict(model, test)
MAE(test$MEDV, test.predictions)
# MAE is 3.46 for test set

cor(test.predictions, test$MEDV)
# correlation between predictions and actual values is 0.80 for test set

# 18.	Estimate MEDV via regression tree using CHAS and CRIM. 
tree <- rpart(MEDV ~ CHAS + CRIM, data = train)
summary(tree)

# a.	Plot the tree using rpart.plot
library(rpart.plot)
rpart.plot(tree)

# b.	What is the most important feature? What is the rule for the first leaf node?  
# the most important feature i.e. the first split is on CRIM < 5.85, so most important feature is CRIM
# rule for the first leaf node is if CRIM < 5.85

# c.	How many examples/records/data points are there in the first leaf node? What is the predicted value for these data points? 
# there are 404 observations in the first leaf node. Predicted value is 22.46

# d.	Calculate the correlation & MAE between predicted and the actual using training dataset
train.tree.predictions <- predict(tree, train)
MAE(train$MEDV, train.tree.predictions)
# MAE is 5.09 for train set

cor(train.tree.predictions, train$MEDV)
# correlation between predictions and actual values is 0.64 for train set

# e.	Calculate the correlation & MAE between predicted and the actual using testing dataset
test.tree.predictions <- predict(tree, test)
MAE(test$MEDV, test.tree.predictions)
# MAE is 6.15 for test set

cor(test.tree.predictions, test$MEDV)
# correlation between predictions and actual values is 0.29 for test set
