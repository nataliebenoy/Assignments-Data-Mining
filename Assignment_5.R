#Ass 5A-- Statlog Heart
# 
# We will use statlog heart dataset again for decision tree model practice. Statlog (Heart) Data Set is downloaded from UCI machine learning repository. It has 13 different attributes and 1 class variable. The detailed feature attributes are presented in the below. You could also find it here: https://archive.ics.uci.edu/ml/datasets/Statlog+%28Heart%29
# The attributes include: 1. Age;  2. Sex (female =0, male =1);  3. chest pain type (4 values); 4. resting blood pressure;  5. serum cholestoral in mg/dl;  6. fasting blood sugar > 120 mg/dl (have =1, don't have =0);  7. resting electrocardiographic results (values 0,1,2);  8. maximum heart rate achieved; 9. exercise induced angina (don't have =0, have =1); 10. oldpeak = ST depression induced by exercise relative to rest; 11. the slope of the peak exercise ST segment; 12. number of major vessels (0-3) colored by flourosopy; 13. thal: 3 = normal; 6 = fixed defect; 7 = reversable defect; 
# The class variable is statlogheart: 1=Absence of heart disease; 2= presence of heart disease;
# Among all attributes, we have real attributes: 1,4,5,8,10,12; ordered attributes: 11; binary attributes: 2,6,9 and nominal attributes: 3, 7, 13;


# 1.	Import dataset to R using stringsAsFactors = FALSE.
heartdata <- read.csv("Assignment_5a_dataset.csv")

# Run the following transformations:
heartdata[c(2,3,6,7,9,13,14)]<-data.frame(lapply(heartdata[c(2,3,6,7,9,13,14)],factor))
heartdata <- heartdata[c(2,3,9,10,11,12,14)]

# 2.	Pick 70% records randomly as training dataset. 
rows <- nrow(heartdata)
train.size <- floor(rows * 0.70)

set.seed(1234)
train.index <- sample(1:rows, train.size, replace = F)

train <- heartdata[train.index,]
test <- heartdata[-train.index,]

# 3.	Build a logit regression model where statlogheart is the target variable. 
heartmodel <- glm(statlogheart ~., data = train, family = binomial)
summary(heartmodel)

# 4.	What is the McFadden R^2 for this model? Evaluate the model by its fitness.
nullmodel <- glm(statlogheart ~ 1, data = train, family = "binomial")
1-logLik(heartmodel)/logLik(nullmodel)
# McFadden R-squared is 0.517

# 5.	Evaluate the performance of training dataset by using cross table.  What percentage of people who do not have heart disease are wrongly predicted?
#   Hint for predict: predicted<-predict(model, dt.train, type= "response")
# Hint for converting logs ratio: predicted<- ifelse(predicted>=0.5,2,1)
predictions <- predict(heartmodel, train, type = "response")
head(predictions)

predicted <- ifelse(predictions >= 0.5, 2, 1)
library(gmodels)
CrossTable(train$statlogheart, predicted)
# in-sample accuracy: 0.87
# percentage of people who do not have heart disease (true value = 1) that are wrongly predicted (model predicts 2) is 13/189, or 6.9%

# 6.	Evaluate the performance of testing dataset by using cross table.  What percentage of people who have heart disease are wrongly predicted?
#   Hint for predict: predicted<-predict(model, dt.test, type= "response")
testpredict <- predict(heartmodel, test, type = "response")
head(testpredict)

testpredicted <- ifelse(testpredict >= 0.5, 2, 1)
CrossTable(test$statlogheart, testpredicted)
# out-of-sample accuracy: 0.80
# percentage of people who have heart disease (true value = 2) that are wrongly predicted (model predicts 1) is 15/81 or 18.5%

# 7.	Predict using naiveBayes
library(e1071)
naivemodel <- naiveBayes(train[-7], train$statlogheart, laplace=1)

# a.	Predict classification for the training dataset and create cross table.
# Note for predict: parameter type=response is needed only for logit. 
naivepredictions <- predict(naivemodel, train[-7])
head(naivepredictions)

CrossTable(train$statlogheart, naivepredictions)
# in-sample accuracy: 0.86

# Predict classification for the testing dataset and create cross table. 
naive.test.predictions <- predict(naivemodel, test[-7])
head(naive.test.predictions)

CrossTable(test$statlogheart, naive.test.predictions)
# out-of-sample accuracy: 0.85

# 8.	Predict using decision tree
library(C50)
tree.model <- C5.0(train[-7], train$statlogheart)
summary(tree.model)

# a.	Predict classification for the training dataset and create cross table.
tree.predictions <- predict(tree.model, train[-7])
head(tree.predictions)

CrossTable(train$statlogheart, tree.predictions)
# in-sample accuracy: 0.88

# Predict classification for the testing dataset and create cross table. 
tree.test.predictions <- predict(tree.model, test[-7])
head(tree.test.predictions)

CrossTable(test$statlogheart, tree.test.predictions)
# out-of-sample accuracy: 0.74

# 9.	Predict using knn at k=3
# a.	Predict classification for the training dataset and create cross table. 
# Hint: train=dt.train, test=dt.train
library(class)
knn.train.predictions <- knn(train = train[-7], 
                           test = train[-7],
                           cl = train[[7]], k=3)

CrossTable(train[[7]], knn.train.predictions)
# in-sample accuracy: 0.88

# b.	 Predict classification for the testing dataset and create cross table. 
# Hint: train=dt.train, test=dt.test
knn.test.predictions <- knn(train = train[-7], 
                             test = test[-7],
                             cl = train[[7]], k=3)

CrossTable(test[[7]], knn.test.predictions)
# out-of-sample accuracy: 0.78

# 10.	Compare performance of models using training dataset

# using overall model accuracy as calculated in each of the above questions, in-sample performance is best for the knn and decision tree models (0.88 overall accuracy)

# 11.	Compare performance of models using testing dataset

# using overall model accuracy as calculated in each of the above questions, out-of-sample performance is best for naive bayes (0.85 overall accuracy)

# 12.	Which model would you recommend overall? 
#   Hint: Typically you want to give preference to a model with good test dataset performance. If 2 models perform equally well over test dataset then look for model that performs well for training dataset. 
# If you are looking for attribution and not prediction then you try to get the performance of regression model as close to the best performing other model, and then use regression model. 

# if we're determining the best model by best overall accuracy on the test set, with no costs associated with false negatives or false positives, then the naive bayes model is the best performing model. 


#### ASS 5-b Bitcoin
# Time Series Exercise: Bitcoin Price
# Bitcoin daily price data was collected from 7/18/2010 to 3/9/2015 (http://www.coindesk.com/price/) and aggregate it into a monthly average Bitcoin price data "MonthlyBiPrice.csv". The first column of the data is the month variable from July-2010 to March-2015, and the second column is monthly average price of Bitcoin. Please follow the instructions and answer the questions.
# 
# 1.	Import the monthly average Bitcoin price data into R. Check the summary of variable Monthly.Average.Price. What is the minimum average price of Bitcoin? What is the maximum average price of Bitcoin?

bitcoin <- read.csv("Assignment_5b_dataset.csv")
summary(bitcoin$Monthly.Average.Price)
# minimum average price: 0.06
# maximum average price: 857.18

#   2.	Install the package "forecast" and create a time series object tprice.
# Select and only include the values from variable Monthly.Average.Price to construct this object.
# Set start month as c(2010, 7) and frequency = 12.
library(forecast)
tprice <- ts(bitcoin$Monthly.Average.Price, start = c(2010, 7), frequency = 12)

# 3.	Check the object tprice and see if you have the correct prices and corresponding months.
# Plot the object tprice. At which year does the price of Bitcoin have a peak?
# What is the start and end month/year of tprice? What is the frequency of tprice?
(tprice)
plot(tprice)
# bitcoin price peaks in 2014
start(tprice)
end(tprice)
# start of tprice is July 2010, end of tprice is March 2015
frequency(tprice)
# frequency of tprice is 12

#   4.	Create a subset of object tprice. Set the start month as July-2013 and end month as Feb-2015. Plot this subset. Hint: Use window method
sub.tprice <- window(tprice, start = c(2013, 7), end = c(2015, 2))
plot(sub.tprice)

# 5.	Try smoothing and plotting and data using function ma( ). Try k=5, 10 and 15 respectively, do you see a smoother plotted curve with increasing k value?
plot(ma(tprice, 5))
plot(ma(tprice, 10))
plot(ma(tprice, 15))
# using k=15 smoothes the data to the point that even where the maximum value occurs changes...

#   6.	Next try seasonal decomposition using stl(). What trend do you see in the data? 
plot(stl(tprice, s.window = "period"))
# there appears to be a definite seasonal trend in the data

#   7.	Visualize the seasonal decomposition by using seasonplot functions on the object tprice.  What are the price trends of Bitcoin in year 2013 and 2014? Do these two years share the same trend?
seasonplot(tprice, year.labels = "TRUE", main = "plot")
# price trend of 2013 is slow growth with near-exponential growth in the last 2 months of the year
# price trend of 2014 is broadly downward, with a modest price increase from May to June
# no, these two years do not share the same trend

#   8.	Build a simple exponential smoothing forecasting model using time series object tprice with model = "ANN".
# What is the value of alpha parameter, ie smoothing parameter for the level? 
forecast.model <- ets(tprice, model = "ANN")
forecast.model
# value of alpha is 0.9999, or very close to 1 (only recent observations matter)

#   9.	Use the forecast() function to predict the time series one step into the future.
# What is the average predicted Bitcoin price for April-2015?
# What is the 95% confidence interval for this prediction value?
# Plot this prediction with "Month" as x label and "Price" as y label. 
forecast(tprice, 1)
# average predicted bitcoin price for April 2015 is 274.38
# 95% CI is -180.31 =< mean predicted price =< 729.06
plot(forecast(tprice, 1), xlab = "Month", ylab = "Price")

# 10.	Check the accuracy of this simple model for time series forecasts. What do RMSE, MAE and MAPE stand for?
# What value of mean absolute percentage error does this model generate?
accuracy(forecast.model)
# RMSE stands for root mean squared error
# MAE stands for mean average error
# MAPE stands for mean absolute percent error
# model generates 40.46% mean absolute percent error

#   11.	Log transform the data and save it as ltprice. Build an exponential smoothing model with Level, Slope,
# and Seasonal components with ltprice and model="ZZZ". Check the model.
# What are the values of smoothing parameters for the level, trend, and seasonal components? 
ltprice <- log(tprice)
ltprice

log.model <- ets(ltprice, model = "ZZZ")
log.model
# the ets function automatically chose ANN, so there are no smoothing parameters for the trend and seasonal components...
# either ask for model = "ZZZ" or ask for level, slope, and seasonal components, and not both?
# value of alpha (smoothing parameter for level) is 0.9999

log.model2 <- ets(ltprice, model = "AAA")
log.model2
# if you do it this way instead, alpha = 0.9997, beta = 0.0075, gamma = 0.0002

#   12.	Use forecast() function to forecast the Bitcoin price for the next 5 months. Plot the prediction with "Month" as x label and "Price" as y label.
# Transform the mean, lower and upper prices of the prediction using exponential function to the actual predictions.
# What is the average predicted Bitcoin price for April-2015? What is the 95% confidence interval of the price?
forecast(ltprice, 5)
plot(forecast(ltprice, 5), xlab = "Month", ylab = "Price")
exp(5.6145) # 274.38 = mean forecast price
exp(3.622233) # 37.42 = lowest 95% CI prediction over the 5-month forecast
exp(7.606767) # 2011.76 = highest 95% CI prediction over the 5-month forecast
# average predicted bitcoin price for April 2015 is still 274.38
exp(4.723460)
exp(6.505540)
# 95% CI is 112.56 =< mean predicted price =< 668.84

#   13.	Check the accuracy of this model for time series forecasts. What value is this model's mean absolute percentage error?
accuracy(log.model)
# MAPE is 36.19

#   14.	Import library tseries. Decide the best d value for object ltprice using ndiffs( ).
# What is the best d value for our time series object? Then do the differencing of the time series object using diff( ).
# Plot the time series object after differencing. Does it look like there is trend in time series after differencing?   
library(tseries)
ndiffs(ltprice)
# ndiffs says best d value is 1
d.ltprice<- diff(ltprice)
plot(d.ltprice)
# though the data is still very irregular... there doesn't seem to be any overarching upward or downward trend after differencing

#   15.	Evaluate the assumption of stationarity using Augmented Dickey-Fuller (ADF) test.
# Do we have a stationarity time series object based on the test results?
adf.test(d.ltprice)
# with a p-value of 0.01, the ADF indicates that we should reject the null hypothesis in favor of the alternative hypothesis (the time series is stationary) at the P < .05 level

#   16.	Fit an ARIMA model with p = 2, d=1 and q=1. What is the AIC value of this model?
# Then check the accuracy of the model. What is the value of MAPE?
arima.model <- arima(ltprice, order = c(2,1,1))
arima.model
# AIC is 55.63

#   17.	Evaluate the model fitness by check the residuals using qqnorm and qqline functions.
# Does the residuals fall along the line? What can we learn if the residuals fall along the line?
# Use box.test() function to check whether autocorrelations are all zero. What can you interpret from the box.test() results?
qqnorm(arima.model$residuals)
qqline(arima.model$residuals)
# the residuals more or less fall along the line
Box.test(arima.model$residuals, type = "Ljung-Box")
# large p-value here suggests residuals are independent, which is what we want

#   18.	Forecast three months Bitcoin prices with this ARIMA model. What is the predicted average Bitcoin price for April-2015? 
forecast(arima.model, 3)
exp(5.795843)
# predicted average bitcoin price for April 2015 is 328.93

#   19.	Use an automated ARIMA forecasting model for the object ltprice. What are the values for p, d and q?
# Compare this model and the one from Q16 based on AIC. Which of the two models is better base on AIC values?
auto.model <- auto.arima(ltprice)
auto.model
# p = 0, d = 1, q = 1
# AIC of auto.model is 51.62 < 55.63(AIC of manual ARIMA model)
# based on AIC the auto.model is the better choice
