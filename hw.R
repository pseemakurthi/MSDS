setwd("/media/prasad/Edu/Machine_learning/vertebral_column_data")
col2 <- read.table("column_2C.dat")

# Explain what the data represents.  Lump all data not classified as “normal”
# into another category “abnormal,” making this a binary problem.
install.packages("adabag")
library(adabag)
library(caret)
library(C50)
library(rpart)
names(col2)
table(col2$V7)
##Creates a stratified sample
trainindex<-(createDataPartition(col2$V7,p=.8, list= F))
train<- col2[trainindex,]
test<- col2[-trainindex,]
table(train$V7)
table(test$V7)

#Model with out prunning
mod1 <- C5.0(V7~., data = train)
C5.0Control(noGlobalPruning = T)
summary(mod1)
pred1 <- table(data.frame(pred = predict(mod1,test), org = test$V7))

pred1[4]
# Sensitivity: A/(A+C) × 100
# Specificity: D/(D+B) × 100
# positive Predictive Value: A/(A+B) × 100
# Negative Predictive Value: D/(D+C) × 100

sens1 <- pred1[1]/(pred1[1]+pred1[2]) #0.9047619
spec1 <- pred1[4]/(pred1[4]+pred1[3]) #0.7
acc1 <- (pred1[1]+pred1[4])/(sum(pred1[1:4])) #0.8387097

mod1_prune <-C5.0(V7~., data = train,control = C5.0Control(noGlobalPruning = T))
pred1_prune <- table(data.frame(pred = predict(mod1_prune,test), org = test$V7))
sens1_prune <- pred1_prune[1]/(pred1_prune[1]+pred1_prune[2]) #1
spec1_prune <- pred1_prune[4]/(pred1_prune[4]+pred1_prune[3]) #0.65
acc1_prune <- (pred1_prune[1]+pred1_prune[4])/(sum(pred1_prune[1:4])) #0.89


mod2 <- rpart(V7~., data= train)
summary(mod2)
plot(mod2, compress= T)
text(mod2,use.n = T)
mod2_prune <- prune(mod2, cp=0.2 )
plot(mod2_prune)
text(mod2_prune, use.n =T) 
pred2 <- table(data.frame(pred = predict(mod2,test,type=c("class")), org = test$V7))
sens2 <- pred2[1]/(pred2[1]+pred2[2]) #1
spec2 <- pred2[4]/(pred2[4]+pred2[3]) #0.55
acc2 <- (pred2[1]+pred2[4])/(sum(pred2[1:4])) #0.85

pred2_prune <- table(data.frame(pred = predict(mod2_prune,test,type=c("class")), org = test$V7))
sens2_prune <- pred2_prune[1]/(pred2_prune[1]+pred2_prune[2]) #0.71
spec2_prune <- pred2_prune[4]/(pred2_prune[4]+pred2_prune[3]) #0.1
acc2_prune <- (pred2_prune[1]+pred2_prune[4])/(sum(pred2_prune[1:4])) #0.806

ibrary(ipred)
install.packages("ipred")
library(ipred)

bag_model <- bagging(V7 ~ ., data = train, nbagg = 25)
bag_pred <- predict(bag_model, test)
pred3 <- table(bag_pred, test$V7)
summary(pred3)
sens3 <- pred3[1]/(pred3[1]+pred3[2]) #0.97
spec3 <- pred3[4]/(pred3[4]+pred3[3]) #0.7
acc3 <- (pred3[1]+pred3[4])/(sum(pred3[1:4])) #0.88

install.packages(randomForest)
library(randomForest
mod4 <- randomForest(train[,-7], train[,7], ntree = 500, mtry = sqrt(6))
p <- predict(mod4, test, type = "response")
pred4 <- table(p, test$V7)
importance(mod4)
# MeanDecreaseGini
# V1         13.83723
# V2         13.28958
# V3         12.11372
# V4         13.47697
# V5         17.16137
# V6         38.12263

sens4 <- pred4[1]/(pred4[1]+pred4[2]) #0.976
spec4 <- pred4[4]/(pred4[4]+pred4[3]) #0.65
acc4 <- (pred4[1]+pred4[4])/(sum(pred4[1:4])) #0.87

