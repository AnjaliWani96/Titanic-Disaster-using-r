setwd("C:/SPB_Data/Kaggle")

titanic.test <- read.csv("C:/Users/Anjali/Downloads/test.csv",stringsAsFactors = FALSE)
View(test)
titanic.train <- read.csv("C:/Users/Anjali/Downloads/train.csv",stringsAsFactors = FALSE)
View(train)
median(titanic.train$Age)
median(titanic.train$Age, na.rm=TRUE)
median(titanic.test$Age, na.rm=TRUE)

titanic.train$IsTrainSet<-TRUE
tail(titanic.train$IsTrainSet)
titanic.test$IsTrainSet<-FALSE
ncol(titanic.train)
ncol(titanic.test)
names(titanic.train)
names(titanic.test)

titanic.test$Survived<-NA

names(titanic.test)

titanic.full<-rbind(titanic.train, titanic.test)
tail(titanic.full)
table(titanic.full$IsTrainSet)
table(titanic.full$Embarked)

titanic.full$Embarked==''
titanic.full[titanic.full$Embarked=='',"Embarked"]<-'S'

table(titanic.full$Embarked)

table(is.na(titanic.full$Age))

median(titanic.full$Age, na.rm=TRUE)

age.median<-median(titanic.full$Age, na.rm=TRUE)
titanic.full[is.na(titanic.full$Age),"Age"]<-age.median
table(is.na(titanic.full$Fare))

#clean missing values of fare
fare.median<-median(titanic.full$Fare, na.rm = TRUE)
fare.median

titanic.full[is.na(titanic.full$Fare),"Fare"]<-fare.median
table(is.na(titanic.full$Fare))

#categorical casting
table(titanic.full$Survived)
titanic.full$Survived
str(titanic.full)


titanic.full$Pclass<-as.factor(titanic.full$Pclass)
titanic.full$Sex<-as.factor(titanic.full$Sex)
titanic.full$Embarked<-as.factor(titanic.full$Embarked)

#split dataset back out into train and test
titanic.train<-titanic.full[titanic.full$IsTrainSet==TRUE,]
tail(titanic.train)

titanic.test<-titanic.full[titanic.full$IsTrainSet==FALSE,]

str(titanic.train)
str(titanic.test)
str(titanic.full)

titanic.train$Survived<- as.factor(titanic.train$Survived)

str(titanic.train)

survived.equation<- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula<- as.formula(survived.equation)

install.packages("randomForest")
library(randomForest)
titanic.model<- randomForest(formula = survived.formula, data = titanic.train, ntree=500, mtry=3, nodesize = 0.01 * nrow(titanic.test))

features.equation<- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived<-predict(titanic.model, newdata = titanic.test)

Survived

PassengerId<- titanic.test$PassengerId

output.df<-as.data.frame(PassengerId)
output.df$Survived<-Survived

write.csv(output.df, file="kaggle_submission1.csv", row.names = FALSE)

###########################################################

#How to clean data using predictive model in R

setwd("C:/SPB_Data/Kaggle")

titanic.test <- read.csv("C:/Users/Anjali/Downloads/test.csv",stringsAsFactors = FALSE)
View(test)
titanic.train <- read.csv("C:/Users/Anjali/Downloads/train.csv",stringsAsFactors = FALSE)
View(train)
median(titanic.train$Age)
median(titanic.train$Age, na.rm=TRUE)
median(titanic.test$Age, na.rm=TRUE)

titanic.train$IsTrainSet<-TRUE
tail(titanic.train$IsTrainSet)
titanic.test$IsTrainSet<-FALSE
ncol(titanic.train)
ncol(titanic.test)
names(titanic.train)
names(titanic.test)

titanic.test$Survived<-NA

names(titanic.test)

titanic.full<-rbind(titanic.train, titanic.test)
tail(titanic.full)
table(titanic.full$IsTrainSet)
table(titanic.full$Embarked)

titanic.full$Embarked==''
titanic.full[titanic.full$Embarked=='',"Embarked"]<-'S'

table(titanic.full$Embarked)

table(is.na(titanic.full$Age))

median(titanic.full$Age, na.rm=TRUE)

age.median<-median(titanic.full$Age, na.rm=TRUE)
titanic.full[is.na(titanic.full$Age),"Age"]<-age.median
table(is.na(titanic.full$Fare))

#clean missing values of fare

#fare.median<-median(titanic.full$Fare, na.rm = TRUE)
#fare.median

#titanic.full[is.na(titanic.full$Fare),"Fare"]<-fare.median
#table(is.na(titanic.full$Fare))

boxplot(titanic.full$Fare)
boxplot.stats(titanic.full$Fare)

boxplot.stats(titanic.full$Fare)$stats
boxplot.stats(titanic.full$Fare)$stats[5]
#Linear Regression Model

upper.whisker <-boxplot.stats(titanic.full$Fare)$stats[5]
outlier.filter<- titanic.full$Fare < upper.whisker
titanic.full[outlier.filter,]

fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model<-lm(
  formula = fare.equation,
  data = titanic.full[outlier.filter,]
)

fare.row<-titanic.full[
  is.na(titanic.full$Fare),
  c("Pclass","Sex","Age","SibSp","Parch","Embarked")
  ]

predict(fare.model, newdata = fare.row)
fare.predictions<-predict(fare.model, newdata= fare.row)
titanic.full[is.na(titanic.full$Fare), "Fare"]<-fare.predictions

#categorical casting
table(titanic.full$Survived)
titanic.full$Survived
str(titanic.full)


titanic.full$Pclass<-as.factor(titanic.full$Pclass)
titanic.full$Sex<-as.factor(titanic.full$Sex)
titanic.full$Embarked<-as.factor(titanic.full$Embarked)

#split dataset back out into train and test
titanic.train<-titanic.full[titanic.full$IsTrainSet==TRUE,]
tail(titanic.train)

titanic.test<-titanic.full[titanic.full$IsTrainSet==FALSE,]

str(titanic.train)
str(titanic.test)
str(titanic.full)

titanic.train$Survived<- as.factor(titanic.train$Survived)

str(titanic.train)

survived.equation<- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula<- as.formula(survived.equation)

install.packages("randomForest")
library(randomForest)
titanic.model<- randomForest(formula = survived.formula, data = titanic.train, ntree=500, mtry=3, nodesize = 0.01 * nrow(titanic.test))

features.equation<- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived<-predict(titanic.model, newdata = titanic.test)

Survived

PassengerId<- titanic.test$PassengerId

output.df<-as.data.frame(PassengerId)
output.df$Survived<-Survived

write.csv(output.df, file="kaggle_submission1.csv", row.names = FALSE)
