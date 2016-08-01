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
train$male = train %>%
  mutate(male = ifelse(Sex == "female",0,1))

train.nontext = select(train, Survived, PassengerId, Pclass, Age)

# Feature Selection
correlationMatrix <- cor(train.nontext[,])
print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
print(highlyCorrelated)

# Decision Tree
tree = rpart(Survived ~ ., train, method = "class")
