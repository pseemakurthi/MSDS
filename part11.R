# Friday, September 19
#
# Interactions and qualitative variables
#
library(VIF)
main<-rm(list=ls(all=T))
setwd("/media/prasad/Edu/Stat 6021/part11")
s1 = read.csv("bodyfat.csv", header = TRUE)
names(s1) <- c("y","x1","x2","x3")
#
plot(s1,pch=20,cex=.2)
s1.lm <- lm(y~x1+x2+x3, data=s1)
summary(s1.lm)
vif(s1.lm)
#
# Next add in interaction terms
# This expands the data frame
#
x12 <- s1$x1*s1$x2
x13 <- s1$x1*s1$x3
x23 <- s1$x2*s1$x3
s1i <- data.frame(s1,x12,x13,x23)

s1i.lm <- lm(y~x1+x2+x3+x12+x13+x23,data=s1i)
summary(s1i.lm)

# A shortcut to specify all second
# order interactions

s1i.lm <- lm(y~(x1+x2+x3)^2,data=s1i)
summary(s1i.lm)

# If we want just x1*x3

s1i.lm <- lm(y~x1+x2+x3+x1:x3,data=s1i)
summary(s1i.lm)

# Correlations between variables is
# often a problem when introducting
# interacting variables

cor(s1i)

# "Centering" the data can help reduce
# this problem

x1c <- s1$x1-mean(s1$x1)
x2c <- s1$x2-mean(s1$x2)
x3c <- s1$x3-mean(s1$x3)

x12 <- x1c*x2c
x13 <- x1c*x3c
x23 <- x2c*x3c
s1c <- data.frame(s1$y,x1c,x2c,x3c,x12,x13,x23)
cor(s1c)

s1c.lm <- lm(s1.y~x1c+x2c+x3c+x12+x13+x23,data=s1c)
summary(s1c.lm)

# Activity: Import "icecream.csv" and perform
# regression analysis, trying out interaction
# terms to determine if they make a difference


s2 = read.csv("insurance.csv", header = TRUE)
plot(s2,pch=20,cex=.2)

s2.lm <- lm(time~size+type, data=s2)
summary(s2.lm)
vif(s1.lm)

# Does interaction matter?

s2i.lm <- lm(time~size*type, data=s2)
summary(s2i.lm)

