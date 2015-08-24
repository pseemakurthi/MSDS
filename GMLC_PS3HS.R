main <- rm(list=ls(all=T))
setwd("H:/DS6001")
library(ggplot2)
data <- read.csv("proj.csv",header=T)
data <- data[,1:3]
names(data) <- c("Sno","length","width")
data$ratio <- data$length/data$width
leaf <- data[c(1:30,41:70),2:4]
test <- data[c(31:40,71:80),2:4]
leaf$type <-as.factor( c(rep(1,30),rep(2,30)))


#Scatterplot the data, coloring by class.
plot(leaf$length,width,pch=21,col=c("red","blue")[type])
qplot(length,width,colour=type,data=leaf)

# Subset the data by type.
t1 <- subset(leaf, type == "1")
t2 <- subset(leaf, type == "2")


# Compute the mean of length, width, ratio for each type of leaf
# Mean of length
t1lm <- mean(t1[,1])
t2lm <- mean(t2[,1])
# Mean of width
t1wm <- mean(t1[,2])
t2wm <- mean(t2[,2])
# Mean of ratio
t1rm <- mean(t1[,3])
t2rm <- mean(t2[,3])

# Compute the standard deviation of length, width, ratio for each type of leaf
# sd of length
t1ls <- sd(t1[,1])
t2ls <- sd(t2[,1])
# sd of width
t1ws <- sd(t1[,2])
t2ws <- sd(t2[,2])
# sd of ratio
t1rs <- sd(t1[,3])
t2rs <- sd(t2[,3])

######################
### Single feature ###
######################
# For the single feature, compute the z-score for the threshold and expected % accuracy for the threshold
# Single feature: length
# xl is the number of standard deviation to the threshold, which is z-score
xl <- (t1lm-t2lm)/(t1ls+t2ls)
xl
# thl is the threshold
thl <- t1lm-xl*t1ls
# compute the % accuracy
acl <- (sum(test$length[1:10]>thl) + sum(test$length[11:20]<thl))/20
acl

# Single feature: width (same method applied)
xw <- (t2wm-t1wm)/(t1ws+t2ws)
xw
# thw is the threshold
thw <- t2wm-xw*t2ws
# Compute the % accuracy
acw <- (sum(test$width[11:20]>thw) + sum(test$width[1:10]<thw))/20
acw

# Single feature: ratio (same method applied)
xr <- (t1rm-t2rm)/(t1rs+t2rs)
xr
# thr is the threshold
thr <- t1rm-xr*t1rs
# compute the % accuracy
acr <- (sum(test$ratio[1:10]>thr) + sum(test$ratio[11:20]<thr))/20
acr


#
# Out length width and ratio ratio is the one which gives highest accuracy of 95%
#


####################
# Length and width #
####################
# Compute the Mahalanobis distance from each type of leaf, only including length and width
t1lwm = colMeans(t1[,1:2])
t2lwm = colMeans(t2[,1:2])
# Compute the covariance of each type of leaf.
Ct1lw <- cov(t1[1:2])
Ct2lw <- cov(t2[1:2])

Mahalt1lw=c()
Mahalt2lw=c()
for (i in 1:length(leaf[,1])){
  Mahalt1lw[i] = mahalanobis(leaf[i,1:2], t1lwm, Ct1lw)
}
for (i in 1:length(leaf[,1])){
  Mahalt2lw[i] = mahalanobis(leaf[i,1:2], t2lwm, Ct2lw)
}

# Plot the Mahalanobis distance for each type
plot(Mahalt1lw, col="blue")
points(Mahalt2lw, col="green")

color=c()
color[Mahalt1lw-Mahalt2lw < 0] = "blue"
color[Mahalt1lw-Mahalt2lw > 0] = "green"
plot(Mahalt1lw-Mahalt2lw, type="p", col=color)
title("GMLC results using length and width")

# Compute the accuracy
lw.accuracy <- (sum(Mahalt1lw[31:60]-Mahalt2lw[31:60] > 0)+sum(Mahalt1lw[1:30]-Mahalt2lw[1:30]< 0))/60
lw.accuracy

#### Test data using length and width #####
# Compute the Mahalanobis distance from each type of leaf, only including length and width
test1lwm = colMeans(test[1:10,1:2])
test2lwm = colMeans(test[11:20,1:2])
# Compute the covariance of each type of leaf.
Ctest1lw <- cov(test[1:10,1:2])
Ctest2lw <- cov(test[11:20,1:2])

Mahaltest1lw=c()
Mahaltest2lw=c()
for (i in 1:length(leaf[,1])){
  Mahaltest1lw[i] = mahalanobis(test[i,1:2], test1lwm, Ctest1lw)
}
for (i in 1:length(test[,1])){
  Mahaltest2lw[i] = mahalanobis(leaf[i,1:2], test2lwm, Ctest2lw)
}

# Plot the Mahalanobis distance for each type
plot(Mahaltest1lw, col="red")
points(Mahaltest2lw, col="blue")

color=c()
color[Mahaltest1lw-Mahaltest2lw < 0] = "red"
color[Mahaltest1lw-Mahaltest2lw > 0] = "blue"
plot(Mahaltest1lw-Mahaltest2lw, type="p", col=color)
title("GMLC results using length and width for test data")

# Compute the accuracy
lwtest.accuracy <- (sum(Mahaltest1lw[11:20]-Mahaltest2lw[11:20] > 0)+sum(Mahaltest1lw[1:10]-Mahaltest2lw[1:10]< 0))/20
lwtest.accuracy


##################
# Length and ratio
##################
# Compute the Mahalanobis distance from each type of leaf, only including length and ratio
t1lrm = colMeans(t1[,c(1,3)])
t2lrm = colMeans(t2[,c(1,3)])
# Compute the covariance of each type of leaf.
Ct1lr <- cov(t1[c(1,3)])
Ct2lr <- cov(t2[c(1,3)])

Mahalt1lr=c()
Mahalt2lr=c()
for (i in 1:length(leaf[,1])){
  Mahalt1lr[i] = mahalanobis(leaf[i,c(1,3)], t1lrm, Ct1lr)
}
for (i in 1:length(leaf[,1])){
  Mahalt2lr[i] = mahalanobis(leaf[i,c(1,3)], t2lrm, Ct2lr)
}

# Plot the Mahalanobis distance for each type
plot(Mahalt1lr, col="blue")
points(Mahalt2lr, col="green")

color=c()
 color[Mahalt1lr-Mahalt2lr < 0] = "blue"
color[Mahalt1lr-Mahalt2lr > 0] = "green"
plot(Mahalt1lr-Mahalt2lr, type="p", col=color)
title("GMLC results using length and ratio ")

# Compute the accuracy
lr.accuracy <- (sum(Mahalt1lr[31:60]-Mahalt2lr[31:60] > 0)+sum(Mahalt1lr[1:30]-Mahalt2lr[1:30]< 0))/60
lr.accuracy

### test data for length and ratio ###

# Compute the Mahalanobis distance from each type of leaf, only including length and ratio
test1lrm = colMeans(test[c(1:10),c(1,3)])
test2lrm = colMeans(test[c(1:10),c(1,3)])
# Compute the covariance of each type of leaf.
Ctest1lr <- cov(test[1:10,c(1,3)])
Ctest2lr <- cov(test[11:20,c(1,3)])

Mahaltest1lr=c()
Mahaltest2lr=c()
for (i in 1:length(test[,1])){
  Mahaltest1lr[i] = mahalanobis(test[i,c(1,3)], test1lrm, Ctest1lr)
}
for (i in 1:length(test[,1])){
  Mahaltest2lr[i] = mahalanobis(leaf[i,c(1,3)], test2lrm, Ctest2lr)
}

# Plot the Mahalanobis distance for each type
plot(Mahaltest1lr, col="blue")
points(Mahaltest2lr, col="green")

color=c()
color[Mahaltest1lr-Mahaltest2lr < 0] = "blue"
color[Mahaltest1lr-Mahaltest2lr > 0] = "green"
plot(Mahaltest1lr-Mahaltest2lr, type="p", col=color)
title("GMLC results using length and ratio for test data")

# Compute the accuracy
lrtest.accuracy <- (sum(Mahaltest1lr[11:20]-Mahaltest2lr[11:20] > 0)+sum(Mahaltest1lr[1:10]-Mahaltest2lr[1:10]< 0))/20
lrtest.accuracy

### 
###100 % accuraccy 

##################
# width and ratio
##################
# Compute the Mahalanobis distance from each type of leaf, only including width and ratio
t1wrm = colMeans(t1[,c(2,3)])
t2wrm = colMeans(t2[,c(2,3)])
# Compute the covariance of each type of leaf.
Ct1wr <- cov(t1[c(2,3)])
Ct2wr <- cov(t2[c(2,3)])

Mahalt1wr=c()
Mahalt2wr=c()
for (i in 1:length(leaf[,1])){
  Mahalt1wr[i] = mahalanobis(leaf[i,c(2,3)], t1wrm, Ct1wr)
}
for (i in 1:length(leaf[,1])){
  Mahalt2wr[i] = mahalanobis(leaf[i,c(2,3)], t2wrm, Ct2wr)
}

# Plot the Mahalanobis distance for each type
plot(Mahalt1wr, col="blue")
points(Mahalt2wr, col="green")

color=c()
color[Mahalt1wr-Mahalt2wr < 0] = "blue"
color[Mahalt1wr-Mahalt2wr > 0] = "green"
plot(Mahalt1wr-Mahalt2wr, type="p", col=color)
title("GMLC results using width and ratio ")

# Compute the accuracy
wr.accuracy <- (sum(Mahalt1wr[31:60]-Mahalt2wr[31:60] > 0)+sum(Mahalt1wr[1:30]-Mahalt2wr[1:30]< 0))/60
wr.accuracy
## 96 % accuraccy

### test data for width and ratio ###

# Compute the Mahalanobis distance from each type of leaf, only including width and ratio
test1wrm = colMeans(test[c(1:10),c(2,3)])
test2wrm = colMeans(test[c(11:20),c(2,3)])
# Compute the covariance of each type of leaf.
Ctest1wr <- cov(test[c(1:10),c(2,3)])
Ctest2wr <- cov(test[c(11:20),c(2,3)])

Mahaltest1wr=c()
Mahaltest2wr=c()
for (i in 1:length(test[,1])){
  Mahaltest1wr[i] = mahalanobis(test[i,c(2,3)], test1wrm, Ctest1wr)
}
for (i in 1:length(test[,1])){
  Mahaltest2wr[i] = mahalanobis(test[i,c(2,3)], test2wrm, Ctest2wr)
}

# Plot the Mahalanobis distance for each type
plot(Mahaltest1wr, col="blue")
points(Mahaltest2wr, col="green")

color=c()
color[Mahaltest1wr-Mahaltest2wr < 0] = "blue"
color[Mahaltest1wr-Mahaltest2wr > 0] = "green"
plot(Mahaltest1wr-Mahaltest2wr, type="p", col=color)
title("GMLC results using width and ratio ")

# Compute the accuracy
wrtest.accuracy <- (sum(Mahaltest1wr[11:20]-Mahaltest2wr[11:20] > 0)+sum(Mahaltest1wr[1:10]-Mahaltest2wr[1:10]< 0))/20
wrtest.accuracy

### 100%accuracy

############################
# length, width and ratio #
############################
# Compute the Mahalanobis distance from each type of leaf, only including length, width and ratio
t1lwrm = colMeans(t1[,1:3])
t2lwrm = colMeans(t2[,1:3])
# Compute the covariance of each type of leaf.
Ct1lwr <- cov(t1[1:3])
Ct2lwr <- cov(t2[1:3])

Mahalt1lwr=c()
Mahalt2lwr=c()
for (i in 1:length(leaf[,1])){
  Mahalt1lwr[i] = mahalanobis(leaf[i,1:3], t1lwrm, Ct1lwr)
}
for (i in 1:length(leaf[,1])){
  Mahalt2lwr[i] = mahalanobis(leaf[i,1:3], t2lwrm, Ct2lwr)
}

# Plot the Mahalanobis distance for each type
plot(Mahalt1lwr, col="blue")
points(Mahalt2lwr, col="green")

color=c()
color[Mahalt1lwr-Mahalt2lwr < 0] = "blue"
color[Mahalt1lwr-Mahalt2lwr > 0] = "green"
plot(Mahalt1lwr-Mahalt2lwr, type="p", col=color)
title("GMLC results using length, width and ratio ")

# Compute the accuracy
lwr.accuracy <- (sum(Mahalt1lwr[31:60]-Mahalt2lwr[31:60] > 0)+sum(Mahalt1lwr[1:30]-Mahalt2lwr[1:30]< 0))/60
lwr.accuracy

#### test data for length, width and ratio ###
# Compute the Mahalanobis distance from each type of leaf, only including length, width and ratio
test1lwrm = colMeans(test[1:10,1:3])
test2lwrm = colMeans(test[11:20,1:3])
# Compute the covariance of each type of leaf.
Ctest1lwr <- cov(test[1:10,1:3])
Ctest2lwr <- cov(test[11:20,1:3])

Mahaltest1lwr=c()
Mahaltest2lwr=c()
for (i in 1:length(test[,1])){
  Mahaltest1lwr[i] = mahalanobis(test[i,1:3], test1lwrm, Ctest1lwr)
}
for (i in 1:length(test[,1])){
  Mahaltest2lwr[i] = mahalanobis(test[i,1:3], test2lwrm, Ctest2lwr)
}

# Plot the Mahalanobis distance for each type
plot(Mahaltest1lwr, col="blue")
points(Mahaltest2lwr, col="green")

color=c()
color[Mahaltest1lwr-Mahaltest2lwr < 0] = "blue"
color[Mahaltest1lwr-Mahaltest2lwr > 0] = "green"
plot(Mahaltest1lwr-Mahaltest2lwr, type="p", col=color)
title("GMLC results using length, width and ratio for test data")

# Compute the accuracy
lwrtest.accuracy <- (sum(Mahaltest1lwr[11:20]-Mahaltest2lwr[11:20] > 0)+sum(Mahaltest1lwr[1:10]-Mahaltest2lwr[1:10]< 0))/20
lwrtest.accuracy
#100 % accurate



