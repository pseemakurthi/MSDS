####
#### This is classification problem 
## The error cost of defining background as signal is more than viceversa
## The Data set is a clean one. Need not check for missing values etc
install.packages("caret")
install.packages("C50")
library(caret)
library(nnet)
library(C50)
library(psych)
library(e1071)
setwd("H:/Machine_learning/Midterm")
data  <- read.table("magic04.data.txt", sep= ",", header = T)
noramlize <-function(x){
              (max(x)-x)/(max(x)-min(x))
          }

###Scaling the data
data_scale <- as.data.frame(lapply(data[,1:10],noramlize))
summary(res$fConc1)

nms <- c("fLength","fWidth","fSize","fConc","fConc1","fAsym","fM3Long","fM3Trans",
                              "fAlpha","fDist")
head(data_scale)
data_scale$class <- data$class
trainIndex <- createDataPartition(data_scale$class, p =0.8, list = F, times = 1)
head(trainIndex)
train <- data_scale[trainIndex,]
table(train$class)
test  <- data_scale[-trainIndex,]
table(test$class)
#### class distribution is almost same so need not to randomize the data
## Checking the distributions of the split
summary(train$fConc1)
summary(data$fConc1)
### Ideal way is to go with logistic regression with binomial classifier 
cls <- class.ind(train$class)

#building a model trainging
nn <- nnet(train[,1:10],cls, size = 5, softmax = T)

# # weights:  67
# initial  value 19870.367009 
# iter  10 value 7041.158736
# iter  20 value 6103.059360
# iter  30 value 5544.532122
# iter  40 value 5384.524813
# iter  50 value 5308.133902
# iter  60 value 5227.366288
# iter  70 value 5033.028073
# iter  80 value 4948.781463
# iter  90 value 4926.000238
# iter 100 value 4893.878720
# final  value 4893.878720 
# stopped after 100 iterations
#predicting a model
test$pred <- predict(nn,test[,1:10],type = "class")
table(test$pred)

### Model diagnostics
###Accuracy
sum(test$pred== test$class)/length(test$pred) ##0.8679989
## Confusion Matrix
xtab <- confusionMatrix(test$pred, test$class)
## AS per the problem statement we need to minimize the missclassification of H as G
diagdata <-test[test$class == "h",]
sum(diagdata$pred=="g")/length(diagdata$pred) 
#0.252266268% miss classified.

nn$class <- ifelse(test$class == "g", 1,0)
nn$pred <- ifelse(test$pred == "g", 1,0)

pred <- prediction(nn$pred,nn$class)
perf <- performance(pred,"tpr","fpr")
##Plotting ROC Curve
plot(perf,col="black",lty=3, lwd=3)


##########################Decision Tree#################################################
### discretizing the data for C50trees is not required as Rinternally handles that.
dTrain <-data[trainIndex,]
dTest <- data[-trainIndex,]

dt1 <- C5.0(dTrain[-11], dTrain$class)

summary(dt1)

dTest$pred <- predict(dt1,dTest[-11])
## Confusion Matrix
xtab1 <- confusionMatrix(dTest$pred, dTest$class)
diagdata1 <-dTest[dTest$class == "h",]
sum(diagdata1$pred=="g")/length(diagdata1$pred)  #0.2692595

# Evaluation on training data (15217 cases):
#   
#   Decision Tree   
# ----------------  
#   Size      Errors  
# 
# 260 1402( 9.2%)   <<
#   
#   
#   (a)   (b)    <-classified as
# ----  ----
#   9435   431    (a): class g
# 971  4380    (b): class h
# 
# 
# Attribute usage:
#   
#   100.00%	fLength
# 100.00%	fAlpha
# 86.96%	fSize
# 86.88%	fWidth
# 70.97%	fM3Long
# 57.77%	fDist
# 42.33%	fConc
# 31.23%	fConc1
# 15.78%	fAsym
# 2.28%	fM3Trans
# 
# 
# Time: 2.1 secs
# 
###################################################################################################
###################################################################################################
###################################################################################################

## Variable Selection
### From the output of decission tree using attribute Usage we can select variables
### I'm considering 30% as threshold and dropping all variables whose usage is less than threshold
### Dropping FAsyn and FConc1 

nn1 <- nnet(train[,1:8],cls, size = 5, softmax = T)
test$pred1 <- predict(nn,test[,1:8],type = "class")
table(test$pred1)
xtab <- confusionMatrix(test$pred1, test$class)

## AS per the problem statement we need to minimize the missclassification of H as G

diagdata1 <-test[test$class == "h",]
sum(diagdata1$pred1=="g")/length(diagdata1$pred1)  #0.002991772625 miss classified. 

dTrainUpdated<- (dTrain[-c(9,10)])
dTestUpdated<- (dTest[-c(9,10)])
dt2 <- C5.0(dTrainUpdated[-9], dTrainUpdated$class)

summary(dt2)
dTestUpdated$pred1 <- predict(dt2,dTestUpdated[-9])
diagdata4 <-dTestUpdated[dTestUpdated$class == "h",]
sum(diagdata4$pred1=="g")/length(diagdata4$pred1)  #0.391 ## error has increased

class <- ifelse(dTestUpdated$class == "g", 1,0)
pred <- ifelse(dTestUpdated$pred1 == "g", 1,0)

pred <- prediction(pred,class)
perf <- performance(pred,"tpr","fpr")
##Plotting ROC Curve
plot(perf,col="black",lty=3, lwd=3)


#####################################################################################################
#####################################################################################################
#########################################Princial Component Analysis#################################
pTest <- data_scale[trainIndex,]
pTrain <- data_scale[-trainIndex,][-11]

pca <- prcomp(pTest[-11])
summary(pca)
# Importance of components:
#   PC1    PC2     PC3     PC4     PC5     PC6     PC7     PC8     PC9    PC10
# Standard deviation     0.3344 0.2675 0.14305 0.10242 0.07942 0.06103 0.05433 0.04973 0.03699 0.02586
# Proportion of Variance 0.4823 0.3088 0.08826 0.04525 0.02720 0.01606 0.01273 0.01067 0.00590 0.00288
# Cumulative Proportion  0.4823 0.7910 0.87930 0.92455 0.95175 0.96782 0.98055 0.99122 0.99712 1.00000
pca$rotation
# 

#PC1 in terms of actual components
# Flength * -0.21 - fwidth*-0.106 - fsize * -.32 *fconc1 *.4 + Fasym*.017 + fm3Long*-.044 - fm3Trans*0.00088
# +falpha *0.58 -fdist*0.21

##First 3 PC account for more than 89% of variaance
##Transformed data
uTrain <- as.data.frame(scale(pTrain,pca$center,pca$scale) %*% pca$rotation[,1:3])

uTest <- as.data.frame(scale(pTest[-11],pca$center,pca$scale) %*% pca$rotation[,1:3])


nn <- nnet(uTest,cls, size = 3, softmax = T)
# # weights:  20
# initial  value 10005.654003 
# iter  10 value 7209.896110
# iter  20 value 6551.638613
# iter  30 value 6489.469241
# iter  40 value 6474.142597
# iter  50 value 6472.490651
# iter  60 value 6459.189520
# iter  70 value 6458.271966
# iter  80 value 6456.558695
# iter  90 value 6456.157052
# iter 100 value 6456.135393
# final  value 6456.135393 
# stopped after 100 iterations
# 
# 
uTest$pred <- predict(nn,uTest,type = "class")
pred <- data.frame(uTest$pred)

xtab <- confusionMatrix(pred, test$class)

diagdata5 <-uTest[uTest$class == "h",]
sum(diagdata5$pred=="g")/length(diagdata5$pred)  #0.0022345 miss classified neural Network on PCA data. 
dt2 <- C5.0(uTrain, train$class)
uTest$pred <- predict(dt2,uTest[-11])
## Confusion Matrix
xtab6 <- confusionMatrix(uTest$pred, dTest$class)
diagdata6 <-uTest[dTest$class == "h",]
sum(diagdata6$pred=="g")/length(diagdata6$pred)  #0.15892


# Conclusion: There is a slight improvement after doing a PCA