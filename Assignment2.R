## Assignment 2A

# Answer 1: 85 students
admission <- read.csv("2a_admission.csv", stringsAsFactors = F)
str(admission)

# Answer 2: 31 students belong to class "admit"
admission$De <- factor(admission$De); levels(admission$De)[2] <- "notadmit"
summary(admission$De)

# Answer 3: Yes, on average students with higher GPA get higher GMAT scores
library(tidyverse)
ggplot(admission, aes(x = GPA, y = GMAT,)) + geom_point()

# Answer 4: Average GPA for admit class is 3.40
x <- aggregate(GPA ~ De, data=admission, FUN=mean)
x[1,]

# Answer 5: Average GMAT for admit class is 561.23
y <- aggregate(GMAT ~ De, data=admission, FUN=mean)
y[1,]

# Answer 6: 36.47% of students belong to class "admit"
table(admission$De)
round(prop.table(table(admission$De)) * 100, digits = 2)

# Answer 7:
head(admission, 10)
set.seed(123)
rows <- sample(nrow(admission))
admission2 <- admission[rows, ]
head(admission2, 10)

# Answer 8: 2.97 GPA
mean(admission$GPA)

# Answer 9: Normalized average GPA is 0.51
Normalize <- function(x){
  ifelse(is.numeric(x), return ((x - min(x)) / (max(x) - min(x))), return(x))}

admission.normal <- data.frame(lapply(admission2, Normalize))
mean(admission.normal$GPA)

# Answer 10: 34 students in test set
nrows <- nrow(admission.normal)
train.size <- floor(nrows * 0.6)

set.seed(124)
train.index <- sample(1:nrows, train.size, replace = F)
train <- admission.normal[train.index,]
str(train)

test <- admission.normal[-train.index,]

# Answer 11: 2 misclassified students in total. Model predicted 2 students as "notadmit" when they actually should have been classified as "admit"
library(class)
library(gmodels)
predicted.test <- knn(train = train[-3], test = test[-3], cl = train$De, k = 5)
CrossTable(x = test$De, y = predicted.test, dnn = c("Actual", "Predicted"))

# Answer 12: average GPA is now this meaningless number: -2.994184e-16
admission.scale <- admission2 %>%
  mutate_at(c("GPA", "GMAT"), ~(scale(.) %>% as.vector))
mean(admission.scale$GPA)

# Answer 13: Now there are still only 2 misclassified students
train2 <- admission.scale[train.index,]
test2 <- admission.scale[-train.index,]

predicted.test2 <- knn(train = train2[-3], test = test2[-3], cl = train2$De, k = 5)
CrossTable(x = test2$De, y = predicted.test2, dnn = c("Actual", "Predicted"))

# Answer 14: Bayesian model out-of-sample accuracy is the same as the KNN model. 2 misclassified students in total. Model predicted 2 students as "notadmit" when they actually should have been classified as "admit"
library(e1071)
model <- naiveBayes(x = train[-3], train$De, laplace = 1)

predicted.test.naive <- predict(model, test[-3])
CrossTable(x = test$De, y = predicted.test.naive, dnn = c("Actual", "Predicted"))

## Assignment 2B

# Answer 1: 2201 observations
delay <- read.csv("2b_FlightDelays.csv", stringsAsFactors = F)
str(delay)

# Answer 2: 208 flights scheduled during either "9" or "10
delay <- delay %>%
  mutate(sched = floor((schedtime/100)))

delay %>%
  group_by(sched) %>%
  summarize(n = n())

# Answer 3: 503 flights on the weekend
delay <- delay %>%
  mutate(dayweek = factor(ifelse(dayweek > 5, 1, 0)))

delay %>%
  group_by(dayweek) %>%
  summarize(n = n())

# Answer 4: 
delay <- delay[-c(1, 3, 5, 6, 7, 11, 12)]

# Answer 5: when weather is good, 396 flights were delayed
delay %>%
  group_by(weather, delay) %>%
  summarize(n = n())

# Answer 6: DCA originates most flights. LGA manages the most destinations. BWI has the highest delay rate. DH has the most flights traveling.
delay <- delay %>%
  mutate(carrier = factor(carrier),
         dest = factor(dest),
         origin = factor(origin),
         weather = factor(weather),
         delay = factor(delay))

summary(delay)

delay %>%
  group_by(origin, delay) %>%
  summarize(n = n())

delay %>%
  group_by(carrier) %>%
  summarize(n = n())

# Answer 7:
delay <- delay %>%
  mutate(origin = ifelse(origin == "DCA", 1, 0),
         dest = ifelse(dest == "LGA", 1, 0),
         carrier = ifelse(carrier == "DH", 1,0))

# Answer 8: I guess if you leave sched as a numeric variable, it would need to be normalized because it's not on a 0-1 scale like everything else is. Or you could just make it a factor...
set.seed(125)
delay.rows <- sample(nrow(delay))
delay.random <- delay[delay.rows, ]

delay.normal <- data.frame(lapply(delay.random, Normalize))

# Answer 9: 881 observations in delay.test set
delay.nrows <- nrow(delay.normal)
delay.train.size <- floor(delay.nrows * 0.6)

set.seed(126)
delay.train.index <- sample(1:delay.nrows, delay.train.size, replace = F)
delay.train <- delay.normal[delay.train.index,]
str(delay.train)

delay.test <- delay.normal[-delay.train.index,]
str(delay.test)

# Answer 10: 18 flights were predicted as delayed that were actually on time.
delay.predicted.test <- knn(train = delay.train[-6], test = delay.test[-6], cl = delay.train$delay, k = 10)
CrossTable(x = delay.test$delay, y = delay.predicted.test, dnn = c("Actual", "Predicted"))

# Answer 11: 151 flights were predicted as on time, but are actually delayed
delay.model <- naiveBayes(x = delay.train[-6], delay.train$delay, laplace = 1)

predicted.delay.naive <- predict(delay.model, delay.test[-6])
CrossTable(x = delay.test$delay, y = predicted.delay.naive, dnn = c("Actual", "Predicted"))
