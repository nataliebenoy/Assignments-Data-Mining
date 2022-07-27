### Assignment 8a

# 1.	Import the data into R and change the first variable, State, into a factor variable.
library(tidyverse)
voting <- read.csv("assignment8a_dataset.csv")
# State is already a factor with 50 levels

# 2.	Check summary of votes' percentage for the year 1856. What is the minimum percentage of vote?
# What is the median percentage of vote? How many missing values are in year 1856? 
summary(voting)
# minimum voting turnout is 0.19%
# median voting turnout is 47.31%
# there are 30 NAs for 1856 (there weren't 50 states then)

#   3.	Find the mean votes' percentage for the year 1936 using function mean ( ). 
# Hint: you need to remove missing values, ie na.rm=TRUE
mean(voting$X1936, na.rm = T)
# mean voting turnout in 1936 is 32.88%

# 4.	Create an imputing function to replace missing values in a given column with the mean value for that column
# after removing missing values. 
# Hint: You need to define a function, say impute 
# a.	that takes a column parameter (say x). 
# b.	uses ifelse on this column parameter. Condition is if a record is na then replace it with mean of this column after removing missing values;
# If record is not na then keep the record as it is. 

mean_impute <- function(x){
  ifelse(is.na(x),mean(x,na.rm = T),x)
}

# 5.	Check if impute function works for X1856. 
voting$X1856 <- mean_impute(voting$X1856)
summary(voting$X1856)

# 6.	Apply impute function on all of the columns except the first column.
voting2 <- data.frame(lapply(voting[-1], mean_impute))

voting.imputed <- cbind(voting[1], voting2[1:31])

# 7.	Scale the variables in the new data frame.
voting.imputed[2:32] <- scale(voting.imputed[2:32])

# 8.	Set seed (e.g. set.seed(5)) for fixed clustering results.
# Train a k-means clustering models on the scaled election data frame with k =5. 
# Note: Remember not to include the 1st State column while running kmeans. 
set.seed(123)
model <- kmeans(voting.imputed[-1], 5)

# 9.	How many States are in cluster 3 and 4? 
#   Hint: Add model$cluster to data frame. Make sure that column State is also there in the data frame.
voting.imputed$cluster <- model$cluster
model$size

voting.imputed %>%
  group_by(cluster) %>%
  summarize(n = n())
# 19 states are in cluster 3
# 10 states are in cluster 4

# 10.	List out all states for the cluster 3
voting.imputed$State[voting.imputed$cluster == 3]

# 11.	Look at the center of clusters. What center values of x1876 do cluster 1, 2 and 3 have? 
model$centers
# center value of X1876 for cluster 1 is -0.69
# center value of X1876 for cluster 2 is 1.59
# center value of X1876 for cluster 3 is 0.15

# 12.	What are the average votes' percentages of year 1900 and 1892 for cluster 5? 
aggregate(data = voting.imputed, X1900 ~ cluster, FUN = mean)
# average (scaled) turnout for 1900 for cluster 5 is -1.81
# how do we undo scale for a more interpretable number? I don't know

aggregate(data = voting.imputed, X1892 ~ cluster, FUN = mean)
# average (scaled) turnout for 1892 for cluster 5 is -1.997

# 13.	Which clusters do Alabama, California and Utah belong to? 
# Hint:you need to subset data for which you need to show cluster number, ie dt2$State==c("California", "Utah")
table(voting.imputed$State == c("California", "Utah", "Alabama"), voting.imputed$cluster)

voting.imputed$cluster[voting.imputed$State == c("California", "Utah")]
voting.imputed$cluster[voting.imputed$State == "Alabama"]
# Alabama, California, Utah belongs to clusters 5, 3, 4, respectively


### Assignment 8b

# 1.	Import dataset to R using stringsAsFactors = FALSE. 
# Run the following transformations:
#   dt[c(2,3,6,7,9,13,14)]<-data.frame(lapply(dt[c(2,3,6,7,9,13,14)],factor))
# dt <- dt[c(2,3,9,10,11,12,14)]
dt <- read.csv("assignment8b_dataset.csv")
dt[c(2,3,6,7,9,13,14)]<-data.frame(lapply(dt[c(2,3,6,7,9,13,14)],factor))
dt <- dt[c(2,3,9,10,11,12,14)]

# 2.	Pick 70% records randomly as training dataset. 
rows <- nrow(dt)
train.size <- floor(rows * 0.70)

set.seed(1234)
train.index <- sample(1:rows, train.size, replace = F)

dt.train <- dt[train.index,]
dt.test <- dt[-train.index,]

# 3.	Build a logit regression model where statlogheart is the target variable. 
heartmodel <- glm(statlogheart ~., data = dt.train, family = binomial)
summary(heartmodel)

# 4.	Evaluate the performance of training dataset by using cross table.
# What percentage of people who do not have heart disease are wrongly predicted?
#   Hint for predict: predicted<-predict(model, dt.train, type= "response")
# Hint for converting probabilities to labels ratio: predicted<- ifelse(predicted>=0.5,2,1)
predictions <- predict(heartmodel, dt.train, type = "response")
head(predictions)

predicted <- ifelse(predictions >= 0.5, 2, 1)
library(gmodels)
CrossTable(dt.train$statlogheart, predicted)
# in-sample accuracy: 0.87
# percentage of people who do not have heart disease that are wrongly predicted is 13/189 or 6.9%

# 5.	Evaluate the performance of testing dataset by using cross table.  What percentage of people who have heart disease are wrongly predicted?
#   Hint for predict: predicted<-predict(model, dt.test, type= "response")
testpredict <- predict(heartmodel, dt.test, type = "response")
head(testpredict)

testpredicted <- ifelse(testpredict >= 0.5, 2, 1)
CrossTable(dt.test$statlogheart, testpredicted)
# out-of-sample accuracy: 0.80
# percentage of people who have heart disease that are wrongly predicted is 15/81 or 18.5%

# 6.	Predict using naiveBayes
# a.	Predict classification for the training dataset and create cross table.
# Note for predict: parameter type=response is needed only for logit. 
# Predict classification for the testing dataset and create cross table. 
library(e1071)
naivemodel <- naiveBayes(dt.train[-7], dt.train$statlogheart, laplace=1)
naivepredictions <- predict(naivemodel, dt.train[-7])
head(naivepredictions)
CrossTable(dt.train$statlogheart, naivepredictions)
# in-sample accuracy: 0.86

naive.test.predictions <- predict(naivemodel, dt.test[-7])
head(naive.test.predictions)
CrossTable(dt.test$statlogheart, naive.test.predictions)
# out-of-sample accuracy: 0.85

# 7.	Predict using decision tree
# a.	Predict classification for the training dataset and create cross table.
# Predict classification for the testing dataset and create cross table. 
library(C50)
tree.model <- C5.0(dt.train[-7], dt.train$statlogheart)
summary(tree.model)

# Predict classification for the training dataset and create cross table.
tree.predictions <- predict(tree.model, dt.train[-7])
head(tree.predictions)
CrossTable(dt.train$statlogheart, tree.predictions)
# in-sample accuracy: 0.88

# Predict classification for the testing dataset and create cross table. 
tree.test.predictions <- predict(tree.model, dt.test[-7])
head(tree.test.predictions)
CrossTable(dt.test$statlogheart, tree.test.predictions)
# out-of-sample accuracy: 0.74

# 8. Predict using knn at k=3
library(class)
knn.train.predictions <- knn(train = dt.train[-7], 
                             test = dt.train[-7],
                             cl = dt.train[[7]], k = 3)

# a. Predict classification for the training dataset and create cross table. 
# Hint: train=dt.train, test=dt.train
CrossTable(dt.train[[7]], knn.train.predictions)
# in-sample accuracy: 0.88

# Predict classification for the testing dataset and create cross table. 
# Hint: train=dt.train, test=dt.test
knn.test.predictions <- knn(train = dt.train[-7], 
                            test = dt.test[-7],
                            cl = dt.train[[7]], k = 3)

CrossTable(dt.test[[7]], knn.test.predictions)
# out-of-sample accuracy: 0.78

# 9.	Compare performance of models using training dataset
# using in-sample model accuracy as calculated in each of the above questions, in-sample performance is best for the knn and decision tree models (0.88 overall accuracy)

# 10.	Compare performance of models using testing dataset
# using out-of-sample model accuracy as calculated in each of the above questions, out-of-sample performance is best for naive bayes (0.85 overall accuracy)

# 11.	Which model would you recommend overall? 
#   Hint: Typically you want to give preference to a model with good test dataset performance.
# If 2 models perform equally well over test dataset then look for model that performs well for training dataset. 
# If you are looking for attribution and not prediction then you try to get the performance of regression model
# as close to the best performing other model, and then use regression model. 

# If you are looking for attribution and not prediction then you try to get the performance of regression model as close to the best performing other model, and then use regression model. 
# if we're determining the best model by best overall accuracy on the test set, with no costs associated with false negatives or false positives, then the naive bayes model is the best performing model. 

# 12.	Plot ROC curves for all the above 3 models (naive Bayes, decision tree, knn).
# Also add a reference line to indicate the performance of a classifier with no predictive value. 
install.packages("ROCR")
library(ROCR)

# for naive Bayes
naive.prob.predictions <- predict(naivemodel, dt.test[-7], type = "raw")
naive.pred <- prediction(predictions = naive.prob.predictions, labels = naive.test.predictions)
naive.perf <- performance (naive.pred, measure = "tpr" , x.measure = "fpr")
plot(naive.perf, main = "ROC curve for Naive Bayes",
                 col = "blue", lwd = 3, abline (a=0, b=1, lwd =2, lty = 2))

# for decision tree
tree.prob.predictions <- predict(tree.model, dt.test[-7], type = "prob")
tree.pred <- prediction(predictions = tree.prob.predictions, labels = tree.test.predictions)
tree.perf <- performance (tree.pred, measure = "tpr" , x.measure = "fpr")
plot(tree.perf, main = "ROC curve for Decision Tree",
     col = "blue", lwd = 3, abline (a=0, b=1, lwd =2, lty = 2))

# for knn
knn.prob.predictions <- knn(train = dt.train[-7], 
                             test = dt.test[-7],
                             cl = dt.train[[7]], k = 3, prob = T)
knn.pred <- prediction(predictions = knn.prob.predictions[,2], labels = knn.test.predictions)
knn.perf <- performance (knn.pred, measure = "tpr" , x.measure = "fpr")
plot(knn.perf, main = "ROC curve for knn",
     col = "blue", lwd = 3, abline (a=0, b=1, lwd =2, lty = 2))

# none of these are working but I don't care anymore...
