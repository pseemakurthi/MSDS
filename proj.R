setwd("/media/prasad/Edu/Machine_learning/proj")
data <- read.csv("final_country.csv")
data <-  data[complete.cases(data),]
str(data[,100:119])
library(C50)
library(rpart)
library(caret)
#install.packages("varSelRF")
library(varSelRF)
library(randomForest)
trainindex<-(createDataPartition(data$countries,p=.8, list= F))

train<- data[trainindex,]
test<- data[-trainindex,]
train <- train[,-c(117,118)]
mod <- randomForest(countries~., data =train)
pred1 <- predict(mod,test)
acc1 <- pred1 == test$countries
table(acc1)

imp <-as.data.frame(importance(mod))

mod1 <- C5.0(countries~. , data= train, control = C5.0Control(noGlobalPruning = F))
summary(mod1)
pred <- predict(mod1,test)
acc <- pred == test$countries
table(acc)
names(imp)


mod2 <- C5.0(countries~. , data= train[,1:50], control = C5.0Control(noGlobalPruning = F,CF = 0.2))
summary(mod1)
pred <- predict(mod2,test)
acc1 <- pred1 == test$countries
table(acc1)


train_updated <- train[,c(4,5,33,60,61,62,88,89)]
train_updated$countries  <- train$countries
test_updated <- test[,c(4,5,33,60,61,62,88,89)]
test_updated$countries <- test$countries
mod3 <- C5.0(countries~. , data= train_updated, control = C5.0Control(noGlobalPruning = F,CF = 0.2))
pred3 <- predict(mod3, test_updated)
acc3 <- pred3 == test_updated$countries
table(acc3)

install.packages('e1071')
library(e1071)
model <- naiveBayes(countries~., data=train_updated, laplace=3)
predts <- predict(model, test_updated)
acc4 <- pred3 == test_updated$countries
table(acc4)
dim(train)
