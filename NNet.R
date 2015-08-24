setwd("/media/prasad/Edu/Machine_learning/nnet")
library(nnet)
library(ggplot2)
set.seed(12345)

data <- read.table(file = "concrete.txt",sep = ",", header =T )
names(data)
summary(data$strength)
concrete_rand <- sample(data,9)

normalize <- function(x){
              return ((x-min(x))/(max(x)-min(x)))
              }
concrete_norm <- as.data.frame(lapply(concrete_rand,normalize))
summary(concrete_norm$strength)
lt
length(concrete_norm$strength)
train <- concrete_norm[1:772,]
test <- concrete_norm[773:1030,]

summary(train$strength)
summary(test$strength)

m <- nnet(strength ~ ., data=train, size=1) 
plot(m)







vars <- setdiff(colnames(training), list('rgroup','strength'))

formauls <- as.formula(paste('strength== "strength"',paste(vars,collapse = '+'), sep = ","))
