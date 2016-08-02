set.seed(4)

library(rpart)
library(mlbench)
library(caret)
library(stringr)
library(dplyr)
library(e1071)
library(randomForest)
library(ggplot2)

# Import Files
train = read.csv("train.csv", stringsAsFactors=FALSE)
test = read.csv("test.csv", stringsAsFactors=FALSE)

# Understand Data
sapply(train, class)
dim(train)
names(train)
str(train)
summary(train)
glimpse(train)
head(train)

# Grab Features
grabFeatures <- function(data) {
  features <- c("Pclass",
                "Age",
                "Sex",
                "Parch",
                "SibSp",
                "Fare",
                "Embarked",
                "title",
                "total_family",
                "deck")
  data$title <- ifelse(str_detect(data$Name, "Mr.") == TRUE, "Mr.",
                 ifelse(str_detect(data$Name, "Mrs.") == TRUE, "Mrs.",
                 ifelse(str_detect(data$Name, "Miss.") == TRUE, "Miss.",
                 ifelse(str_detect(data$Name, "Master.") == TRUE, "Master.",
                 ifelse(str_detect(data$Name, "Don.") == TRUE, "Don.",
                 ifelse(str_detect(data$Name, "Dr.") == TRUE, "Dr.",
                 ifelse(str_detect(data$Name, "Rev.") == TRUE, "Rev.",
                 ifelse(str_detect(data$Name, "Col.") == TRUE, "Col.",
                 ifelse(str_detect(data$Name, "Countess.") == TRUE, "Countess.",
                 ifelse(str_detect(data$Name, "Capt.") == TRUE, "Capt.",
                 ifelse(str_detect(data$Name, "Major.") == TRUE, "Major.",
                 ifelse(str_detect(data$Name, "Mme.") == TRUE, "Mme.",
                 ifelse(str_detect(data$Name, "Ms.") == TRUE, "Ms.",
                 ifelse(str_detect(data$Name, "Mlle.") == TRUE, "Mlle.",
                 ifelse(str_detect(data$Name, "Jonkheer.") == TRUE, "Jonkheer.","None")))))))))))))))
  data$title <- as.factor(data$title)
  data$total_family <- data$SibSp + data$Parch
  data$deck <- ifelse(str_detect(data$Cabin, "A") == TRUE, "A",
                ifelse(str_detect(data$Cabin, "B") == TRUE, "B",
                ifelse(str_detect(data$Cabin, "C") == TRUE, "C",
                ifelse(str_detect(data$Cabin, "D") == TRUE, "D",
                ifelse(str_detect(data$Cabin, "E") == TRUE, "E",
                ifelse(str_detect(data$Cabin, "F") == TRUE, "F",
                ifelse(str_detect(data$Cabin, "G") == TRUE, "G","")))))))
  data$deck <- as.factor(data$deck)
  fea <- data[,features]
  fea$Age[is.na(fea$Age)] <- -1
  fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm=TRUE)
  fea$Embarked[fea$Embarked==""] = "S"
  fea$Sex      <- as.factor(fea$Sex)
  fea$Pclass <- as.factor(fea$Pclass)
  fea$Embarked <- as.factor(fea$Embarked)
  return(fea)
}

head(grabFeatures(test))
glimpse(grabFeatures(test))
glimpse(grabFeatures(train))

summary(grabFeatures(test))

# Random Forest
rf <- randomForest(grabFeatures(train), as.factor(train$Survived), ntree=100, importance=TRUE)

submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- predict(rf, grabFeatures(test))
write.csv(submission, file = "1_random_forest_r_submission.csv", row.names=FALSE)

imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

ggsave("2_feature_importance.png", p)
