setwd("/media/prasad/Edu/Stat 6021/part5")
# Monday, September 8
#
# Simple linear regression, dignostics and 
# transformations
#
# Here we look at a number of examples of data
# where we proform some diagnostics and 
# transformations
#
data01 = read.csv("simple01.csv", header = TRUE)
summary(data01)
#
# Start with a plot of the data
#
plot(y~x, data=data01,pch=20,cex=.2)
# 
# We want to check two things:
# 1. Does a line seem like the appropriate model?
# 2. Are the residuals normal with constant variance?
#
data01.lm <- lm(y ~ x, data=data01)
#
# A summary of the values generated by lm:
#
summary(data01.lm)
#
x <- seq(20,60,.01)
plot(y~x, data=data01,pch=20,cex=.2)
lines(x, 1.947*x+15.688, type="l", col="red")

# The plot function provides a number of
# diagnostic plots, including the residual plot
#
plot(data01.lm,pch=20,cex=.2,which=1)
#
# The "qreference" plot function 
# provides comparisons for qq-plots.
# Note: Requires the DAAG package
#
qreference(residuals(data01.lm),nrep=8,nrow=2)
#
#
#
# Let's look at a second simulated data set
#
data02 = read.csv("simple02.csv", header = TRUE)
#
# A plot of the data
#
plot(y~x, data=data02,pch=20,cex=.2)
#
# A line doesn't appear to be an appropriate 
# model.  Let's look at (x, log(y))
# 
plot(log(y)~x,data=data02,pch=20,cex=.2)
# Let's check the residuals for the 
# log(y) model
#
data02.lm <- lm(log(y) ~ x, data=data02)
#
plot(data02.lm,pch=20,cex=.2,which=1)
#
summary(data02.lm)
#
plot(log(y)~x,data=data02,pch=20,cex=.2)
lines(x, -2.235+.114*x, type="l", col="red")
#
# Reversing the transformation
#
plot(y~x, data=data02,pch=20,cex=.2)
lines(x, exp(-2.235+.114*x), type="l", col="red")

#
# One more data set
#

data03 = read.csv("simple03.csv", header = TRUE)
#
# A plot of the data
#
plot(y~x, data=data03,pch=20,cex=.2)
#
# And a plot with a line
#
data03.lm <- lm(y ~ x, data=data03)
summary(data03.lm)
#
plot(y~x, data=data03,pch=20,cex=.2)
lines(x, 301.74+3.67*x, type="l", col="red")
#
# The residuals
#
plot(data03.lm,pch=20,cex=.2,which=1)
#
# The residuals appear to be increasing with
# x, suggesting another model might be better
# The (x, log(y)) model
#
plot(log(y)~x, data=data03,pch=20,cex=.2)
#
# Maybe a little better.  The residuals:
#
data03logy.lm <- lm(log(y) ~ x, data=data03)
plot(data03logy.lm,pch=20,cex=.2,which=1)
#
# One more model: (log(x), log(y))
#
plot(log(y)~log(x), data=data03,pch=20,cex=.2)
#
# Maybe a little better.  The residuals:
#
data03logxy.lm <- lm(log(y) ~ log(x), data=data03)
plot(data03logxy.lm,pch=20,cex=.2,which=1)
#
# A bit better.  The model:
#
summary(data03logxy.lm)
#
plot(y~x, data=data03,pch=20,cex=.2)
lines(x, exp(4.94)*x^.32, type="l", col="red")

