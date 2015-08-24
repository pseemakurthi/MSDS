setwd("/media/prasad/Edu/Machine_learning/proj")
data <- read.csv("final_country.csv")

daadata <-  data[complete.cases(data),]
set.seed(1234)
str(data[,100:119])
library(C50)
library(rpart)
library(caret)
#install.packages("varSelRF")
library(varSelRF)
library(randomForest)
install.packages("dplyr")
library(dplyr)
library(reshape)

trainindex<-(createDataPartition(data$countries,p=.8, list= F))

train<- data[trainindex,]
test<- data[-trainindex,]
train_lat <- train[,-c(118,119)]
test_lat <- test[,-c(118,119)]
d_cor <- as.matrix(cor(train_lat))
zz<- function(x){if (x<.75){x}else{0}}
d_cor[] <- vapply(d_cor, zz, numeric(1))
write.csv(d_cor,"d_cor.csv")
train_update <- train_lat[,-c(19:30,47:56,77:87,105:116)]
lm1 <- lm(V117~., data = train_update)
summary(lm1)
train_lm <- train_update[,c("V4","V7","V16","V32","V33","V37","V38","V45","V61","V63","V90","V91","V93","V95","V96","V103","V117")]
test_lm <- test_update[,c("V4","V7","V16","V32","V33","V37","V38","V45","V61","V63","V90","V91","V93","V95","V96","V103","V117")]
lm2 <- lm(V117~., data = train_lm) 
pred_lm2 <- predict(lm2,test_lm)
rmse1 <- sqrt( sum( (pred_lm2-test_lm$V117)^2 , na.rm = TRUE ) / nrow(test_lm) )



##############for lontitudes##########################################################3
train_lon <- train[,-c(117,119)]
test_lon <- test[,-c(117,119)]
d_cor_lon <- as.matrix(cor(train_lon))
# zz<- function(x){if (x<.75){x}else{0}}
# d_cor_lon[] <- vapply(d_cor_lon, zz, numeric(1))
# write.csv(d_cor,"d_cor_lon.csv")
train_update_lon <- train_lon[,-c(19:30,47:56,77:87,105:116)]
test_update_lon <- test_lon[,-c(19:30,47:56,77:87,105:116)]
lon_lm1 <- lm(V118~., data = train_update)
summary(lm1)

train_lon_lm <- train_update_lon[,c("V3","V5","V8","V9","V11","V32","V34","V39","V63","V76","V88","V96","V97","V118")]
test_lon_lm <-test_update_lon[,c("V3","V5","V8","V9","V11","V32","V34","V39","V63","V76","V88","V96","V97","V118")]
lon_lm2 <- lm(V118~., data = train_lon_lm) 
pred_lon_lm2 <- predict(lon_lm2,test_lon_lm)
rmse1 <- sqrt( sum( (pred_lm2-test_lm$V117)^2 , na.rm = TRUE ) / nrow(test_lm) )
latlon <- data.frame("lat"= pred_lm2,"lon"= pred_lon_lm2)
write.csv(latlon,"cord.csv")


train_m <-train[,-c(117,118)] 

mm <- mlogit(countries~., data= train_m)

mmod <- multinom(countries ~ ., train_m)
