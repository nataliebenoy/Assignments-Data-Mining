# Q1: Import the ‘admit.csv’ data file into R. Factorize the target variable admit and label the levels 0, 1
# as ‘Not admitted’ and ‘Admitted’ respectively.
admit <- read.csv("ass9_dataset.csv", stringsAsFactors = TRUE)

admit$admit <- factor(ifelse(admit$admit == 1, "Admitted", "Not admitted"), 
                   levels = c("Admitted", "Not admitted"))

# Q2: Scale the GPA, GMAT variables in the data frame.
admit.scale <- data.frame(lapply(admit[2:3], scale))
admit.scale <- cbind(admit[1], admit.scale[1:2], admit[4])

# Q3: Set the seed and split the dataframe in to 70% train and 30% test set.
train.size <- floor(nrow(admit.scale) * 0.70)

set.seed(1234)
train.index <- sample(1:nrow(admit.scale), train.size, replace = F)

admit.train <- admit.scale[train.index,]
admit.test <- admit.scale[-train.index,]

# Q4: Automate parameter tuning of C5.0 decision tree. Apply best C5.0 model on the train  dataset.
# Obtain predicted classes by specifying “type = raw” and predicted probabilities by specifying “type = prob” in the predict() function.
library(caret)
set.seed(123)
model <- train(admit ~., data = admit.train, method = "C5.0")
model
# best performing model (using accuracy as performance criterion) used trials = 1, model = rules, and winnow = F
class.predictions <- predict(model, newdata = admit.test, type = "raw")
head(class.predictions)
prob.predictions <- predict(model, newdata = admit.test, type = "prob")
head(prob.predictions)

# Q5: Customize the tuning process using trainControl() with selectionFunction = "oneSE" to alter resampling strategy.
# Assign it to a variable “ctrl”.
ctrl <- trainControl(selectionFunction = "oneSE")

# Q6: Use expand.grid() with .trails 1, 5, 10, 15, 20, 25, 30, 35 to create grid of tuning parameters.
# Look at the grid and Analyse. Assign it to a variable “grid”.
grid <- expand.grid(.model = "rules", .trials = c(1, 5, 10, 15, 20, 25, 30, 35), .winnow = "False")

# Q7: Use train function to build a model for evaluating each rows combination of model parameters.
# Customize train() with the control list “ctrl” and grid of parameters “grid”. Analyze the output.
custom.model <- train(admit ~., data = admit.train, method = "C5.0", trControl = ctrl, tuneGrid = grid)
custom.model
# very slightly improved in-sample accuracy over first model

# Q8: Create an ensemble using the Bagging method using 25 Decision trees for admit train data.
# Check whether the model works as expected using the predict() function. Estimate the performance of these bagged trees.
library(ipred)
bagged.tree <- bagging(admit ~., data = admit.train, nbagg = 25)
bagged.tree.predictions <- predict(bagged.tree, data = admit.train)
library(gmodels)
CrossTable(admit.train$admit, bagged.tree.predictions)
# in-sample accuracy is only 68%

# Q9: Create a bag control object using svmBag. Fit and evaluate the bagged SVM model using train() function
# and  train control object.
library(kernlab)
bagctrl <- bagControl(fit = svmBag$fit, predict = svmBag$pred, aggregate = svmBag$aggregate)

predfunct <- function (object, x)
{
  if (is.character(lev(object))) {
    out <- predict(object, as.matrix(x), type = "probabilities")
    colnames(out) <- lev(object)
    rownames(out) <- NULL
  }
  else out <- predict(object, as.matrix(x))[, 1]
  out
}

svm.model <- train(admit ~., data = admit.train, method = "bag", B = 100, trControl = ctrl, bagControl = bagControl(fit = svmBag$fit, predict = predfunct, aggregate = svmBag$aggregate))
svm.model
# in-sample accuracy is 0.675
# this bagged SVM model did not want to run properly, very annoying

# Q10: Create an Adaaboost.M1 model on the admit train dataset. Make predictions. Obtain the confusion matrix.
# Calculate kappa statistic for the confusion matrix using kappa() function from package “vcd”.
library(adabag)
library(vcd)

set.seed(123)
adaboost.model <- boosting(admit ~ ., data = admit.train)
adaboost.predictions <- predict(adaboost.model, admit.train)

adaboost.predictions$confusion
Kappa(adaboost.predictions$confusion)
# Kappa stat is 0.984

# Q11: Train the admit data set with Random Forests. Use repeated 10-fold cross-validation or
# 10- fold CV repeated 10 times for most accurate comparison of model performance.
ctrl2 <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10)

rf.model <- train(admit ~ ., data = admit.train, method = "rf", trControl = ctrl2)
rf.model
# in-sample accuracy is 0.68, Kappa is 0.24, mtry = 2

# Q12: Set up a tuning grid for random forests with values 2, 4, 8, 16.
# Supply the resulting grid to train() function and use kappa statistic to select the best model.
# Create a boosted C5.0 decision tree using 10,20,30 and 40 iterations.
rf.grid <- expand.grid(.mtry = c(2, 4, 8, 16))
rf.model2 <- train(admit ~ ., data = admit.train, method = "rf", metric = "Kappa", trControl = ctrl2, tuneGrid = rf.grid)
rf.model2
# negligibly improved in-sample accuracy, 0.68, Kappa is 0.24, mtry = 2

c50.grid <- expand.grid(.model = "tree", .trials = c(10, 20, 30, 40), .winnow = "FALSE")
set.seed(123)
c50.model <- train(admit ~ ., data = admit.train, method = "C5.0",
               metric = "Kappa", trControl = ctrl2,
               tuneGrid = c50.grid)
c50.model
# in-sample accuracy is 0.685, Kappa is 0.15

# Q13: Compare the C5.0 decision tree to random forests and write down your analysis.
rf.predictions <- predict(rf.model, newdata = admit.test)
CrossTable(admit.test$admit, rf.predictions)
# out-of-sample accuracy of random forest model is 67%

c50.predictions <- predict(c50.model, newdata = admit.test)
CrossTable(admit.test$admit, c50.predictions)
# out-of-sample accuracy of c50 model is 77%

# while in-sample accuracy is comparable between the two models, boosted c50 model performs better on new data, and should be considered the superior model for this reason.
