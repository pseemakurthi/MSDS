

install.packages("graphics")
library(graphics)
install.packages("ggplot2")
library(ggplot2)
install.packages("outliers")
library(outliers)
main <- rm(list=ls(all=T))
setwd("/media/prasad/Edu/Stat 6021/HW1")
copier <- read.table("copier.txt",header = T)
gpa <- as.data.frame(read.table("gpa.txt",header = T))

summary(copier)

# Make a scatter plot of the data, and perform appropriate diagnostics to evaluate the appropriate-ness 
# of the linear regression mode for this data, and to identify any potential outliers. 
# (Regardles of your conclusions, do the remaining parts below.) Summarize your observations.

#Scatter plot
gpa.lm <- lm(GPA~ACT, data=gpa)
plot(gpa.lm , las =1)
#Outliers
out <- gpa[c(2,9,115),]

gpa$pred <- predict(gpa.lm,gpa)
ggplot(data=gpa,aes(x=pred,y=GPA)) +
  geom_point(alpha=0.2,color="black") +
  geom_smooth(aes(x=pred,y=GPA),color="black") +
  geom_line(aes(x=GPA,y=GPA),color="blue",linetype=2) +
  scale_x_continuous(limits=c(0,4)) +
  scale_y_continuous(limits=c(0,4))

attach(gpa)
plot(x= GPA, col = "red" , pch =16)
par(new = T)
plot(x = pred,col = "blue", pch =16)


ggplot(data=gpa,aes(x=pred, y=GPA)) +
  geom_point(alpha=0.2,color="red") +
  geom_smooth(aes(x=pred, y=GPA), color="blue",method = "loess")
              

# Predicted Vs Residuals 
ggplot(data=gpa,aes(x=pred,y=pred-GPA)) +
geom_point(alpha=0.5,color="blue") +
geom_smooth(aes(x=pred,y=pred-GPA),color="red",method ="loess")


attach(gpa)
plot(x=ACT,y=GPA, pch =16 , col = "red")
abline (gpa.lm, col = "blue")

#b)Obtain the least-squares estimates of β 0 and β 1 , and state the regression equation
b1 <- sum( (ACT-mean(ACT))*(pred-mean(pred)) ) /
  + sum( (ACT-mean(ACT))^2 )

b0 <- mean(pred)-b1*mean(ACT)

#the above can be directly derived from the coefficents(gpa.lm)
coefficients(gpa.lm)

# Regression equation = GPA = 0.03882713 *ACT + 2.11404

# Plot the estimated regression line on the same graph as the data. Does the function appear to be
# a good fit
plot(x=ACT,y=GPA)
abline(gpa.lm)
#from the above plot the fit is not so good 


# d)Obtain a point and 99% interval estimate for the mean GPA for students with ACT test score of
# 30
newvalues = data.frame(ACT = c(30))
predict(gpa.lm, newvalues, interval="conf",level = 0.99)

# Give a point and 99% interval estimate for the change in the mean GPA corresponding to a one
# point increase in ACT score.
point = 0.03882713
confint(gpa.lm,parm = "ACT",level = 0.99)

# (Intercept)         ACT 
# 2.11404929  0.03882713 

# Is there evidence to conclude that there is a linear association between ACT scores freshman year
# GPA? Justify your conclusion.
plot(x=ACT,y=GPA)


# q2)
# (a) Make a scatter plot of the data, and perform appropriate diagnostics to evaluate the appropriate-
#   ness of the linear regression model for this data, and to identify any potential outliers. (Regardless 
#   of your conclusions, do the remaining parts below.) Summarize your observations.


plot(x= Copiers , y= Time)
cp.lm <- lm(Time~Copiers,data = copier)
summary(cp.lm)
cp1.lm <- lm(Time~Copiers+0,data = copier)
summary(cp1.lm)
abline(cp.lm,col="1")
abline(cp1.lm,col="red")

# (b) Obtain the estimated regression equation.
# Time = 14.9472 * Copiers

# (c) Plot the regression equation together with the data. Does the function appear to be a good fit?
plot(x= Copiers , y= Time)
abline(cp1.lm,col="red")
# ##
#e Give a point and 98% interval estimate for the predicted amount of time required for a call to
# service 4 copiers.

newvaluesCopier1 = data.frame(Copiers = c(4))
predict(cp1.lm, newvaluesCopier1, interval="pred",level = 0.98)

# (f) Give a point estimate for σ 2 .
# Residual standard error gives the standard deviation of the sample so σ2: 8.816*8.816
 

# (g) Give the coefficient of determination.
#Multiple R-squared:   0.99


# summa
# 3. Suppose that you perform a simple linear regression on a set of 20 data points. The lm function reports
# that b 1 = 2.32 and that the standard error for this estimate is 0.65. The researcher you are assisting
# is interested in whether the slope of the regression is equal to 1. Carry out an appropriate hypothesis
# test, determining the value of the test statistic and the corresponding p-value.

# H0 : the slope of the regression line = 1
# Ha : The slope of the line is not equal 1

b1 <- 2.32
se <- 0.65
t <- (b1-1)/se #=  2.030769

# The two-tailed P value equals 0.0573  
# taking 95% confidence interval level we are failed to reject H0
# with more data we could estimate more precisely . 

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
summary(copier)

copier.lm <- lm(Time~Copiers,data = copier)
copier1.lm <- lm(Time~Copiers+0,data = copier)
plot(copier.lm ,las =1)
attach(copier)
plot(x= Copiers ,y = Time)
abline(copier.lm, col = "blue")

# 4. For the copier data above, the first 21 customer visits come in the one week, and remaining visits come
# in the following week. Is there evidence that there is a difference between the data from the different
# weeks? Carry out an analysis, and explain your reasoning.


copier_1st <- copier[c(1:21),]
copier_next <- copier[c(22:45),]
summary(copier_1st)

combined <- rbind(data.frame(copier_1st,period="1"),
                  data.frame(copier_next,period="2"))

lm1 <- lm(Time~Copiers,data = copier_1st)
lm2 <- lm(Time~Copiers, data = copier_next)
with(combined, plot(Copiers, Time, col=period))
abline(lm1,col="black")
abline(lm2,col="red")



