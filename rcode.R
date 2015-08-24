main <- rm(list=ls(all = T))

# install.packages("lubridate")
library(lubridate)
library(glmnet)
# install.packages("psych")
library(psych)
setwd("/media/prasad/Edu/Machine_learning/Rest")
data <- read.csv("train.csv", header = T)
data1 <- read.csv("test.csv", header = T)
str(data)


final_d <- rbind(data[,-43],data1)
data <- final_d
data$date <- mdy(data$Open.Date)
data <- data[,-2]
data$interval <-  now() - data$date 



##herarchiacl clustering
data_hclust <- data[,-c(1,3,42)]
data_hclust$interval <- as.numeric(data_hclust$interval)
clust_data <- scale(data_hclust[,-c(1)])
pcenter <- attr(clust_data, "scaled:center")
pscale <- attr(clust_data, "scaled:scale")

d <- dist(clust_data, method="euclidean")
pfit <- hclust(d, method="ward.D2") ## need to research about the method "Ward"
plot(pfit, labels=data$City)

rect.hclust(pfit, k=5)
groups <- cutree(pfit, k=5) ### converting cities to various grous

data$city <- groups ## adding new groups to city
#rm(data_hclust,clust_data)

###Data for modelling 

mod_data <- data[,-c(1:5,43)]

pairs.panels(mod_data)

y <- mod_data$revenue
x <- mod_data[,-c(37,38)]
library(glmnet)
mode_enet <- glmnet(x,y) 
coef.glmnet(mode_enet)



test <-read.csv("test.csv",header = T)


test$date <- mdy(test$Open.Date)
test$interval <-  now() - test$date
test_updated <- test[,-c(1,2,3,4,5,9,10,13,17,18,19,22,28,30,39,41,43)]



test_updated$interval <- as.numeric(test_updated$interval)
test_clust_data <- scale(test_clust_data)
pcenter <- attr(test_clust_data, "scaled:center")
pscale <- attr(test_clust_data, "scaled:scale")

d <- dist(test_clust_data, method="euclidean")
pfit <- hclust(d, method="ward.D2") ## need to research about the method "Ward"
plot(pfit, labels=data$City)



rect.hclust(pfit, k=5)
groups <- cutree(pfit, k=5) ##


library(cluster)





z <- clara(test_clust_data,3,metric= "euclidean")
values <- z[4]

write.csv(values,"values.csv")
table(values)
