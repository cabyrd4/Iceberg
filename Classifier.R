set.seed(4)

library(rpart)
library(mlbench)
library(caret)
library(dplyr)
library(e1071)

# Import Files
train = read.csv("train.csv")
test = read.csv("test.csv")

# Understand Data
sapply(train, class)
dim(train)
names(train)
str(train)
summary(train)
glimpse(train)


# Data Manipulation
dataset = train %>%
  select(-PassengerId, -Name, -Ticket)

glimpse(dataset)
dim(dataset)

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(dataset[,2:9], dataset[,1], sizes=c(1:8), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

