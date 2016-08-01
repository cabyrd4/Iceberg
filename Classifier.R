set.seed(4)

library(rpart)
library(mlbench)
library(caret)
library(dplyr)

# Import Files
train = read.csv("train.csv")
test = read.csv("test.csv")

# Understand Data
class(train)
dim(train)
names(train)
str(train)
summary(train)
glimpse(train)

class(test)
dim(test)
names(test)
str(test)
summary(test)

# Data Manipulation
train$Male = ifelse(train$Sex == 'male', 1, 0)

# Train dataset w/o text
train.nontext = select(train, Survived, PassengerId, Pclass, Male, Age:Parch, Fare)

# Feature Selection
correlationMatrix <- cor(train.nontext[,])
print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
print(highlyCorrelated)

# Decision Tree
tree = rpart(Survived ~ ., train, method = "class")
