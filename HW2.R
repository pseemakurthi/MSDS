main <- rm(list = ls(all =T))
setwd("/media/prasad/Edu/Stat 6021/Hw2")

#Reading the file and converting X3 as factor
gro <- as.data.frame(read.table("grocery.txt", header = T,colClasses = c(rep("numeric",3),"factor")))


names(gro) <- c("tlh","cs","ic","hol")
#Summary 
summary(gro)
str(gro)
boxplot(gro$cs)
boxplot(gro$ic)
#labeling outliers
source("http://www.r-statistics.com/wp-content/uploads/2011/01/boxplot-with-outlier-label-r.txt")
y_lab<- seq(1,52,1)
boxplot.with.outlier.label(gro$cs~gro$hol,y_lab)
boxplot.with.outlier.label(gro$ic~gro$hol,y_lab)


#Scatetter plot 
#For x1
plot(gro$cs, pch=20,cex=1, col= "red")
# For x2
plot (gro$ic, pch=20,cex=1, col= "blue")




# c) The 52 records are for consecutive weeks. Make a time plot (a plot of week number 1, 2, 3, ...,
#  52 against the variable) for each of the explanatory variables. Do the plots show anything of
#  interest?

gro$week <- as.factor(seq(1,52,1))
tsx1 <- ts(gro$cs)
plot.ts(tsx1)

tsx2 <- ts(gro$ic)
plot.ts(tsx2)

tsx3 <- ts(gro$hol)
plot.ts(tsx3)





# (d) Obtain a regression model using the three explanatory variables. Give an interpretation for each
# of b 1 , b 2 , and b 3 .
model <- lm(tlh~cs+ic+hol,data = gro)
coefficients(model)
# tlh  = 4149.89 + 0.000787*cs  − 13.166*ic + 623.554*hol



#leaps( x=gro[,2:4], y=gro[,1], names=names(gro)[2:4], method="Cp")

# (e) Is there evidence of a difference between holiday weeks and non-holiday weeks?
  #YES looking the cofficient of hol its for holiday =1 there is an extra effort 643.55

# f) Obtain the residuals, and provide separate plots of the residuals against each of Yˆ , X1, X2, and
# X3. Obtain a normal probability (qq) plot for the residuals. Do the plots show anything of
# interest?

#extracting residuals
res <-  resid(model)
gro$pred <- predict(model, gro)
plot(gro$pred,res,
     ylab="Residuals", xlab="Predicted values", 
       main="Pred vs Residual", pch =19,cex =0.5, col ="red") 
abline(0,0, col = "blue")

plot(model, which =1)

#Variance of error terms may not be equal
#assumptions of linerity may ot be true as the residuals forms a group 
#there could be few outliers

plot(gro$cs,res,
     ylab="Residuals", xlab="cases", 
     main="Cases vs Residual", pch =19,cex =0.5, col ="red") 
abline(0,0, col = "blue")

#points are not randomly distributed the relationship may not be linear 
plot(gro$ic,res,
     ylab="Residuals", xlab="percentage", 
     main="percentage vs Residual", pch =19,cex =0.5, col ="red") 
abline(0,0, col = "blue")

#the points are randomly distributed the realatoinship may be linear with few outliers
plot(as.numeric(gro$hol),res,
     ylab="Residuals", xlab="percentage", 
     main="percentage vs Residual", pch =19,cex =0.5, col ="red") 
abline(0,0, col = "blue")
#


qqnorm(res, 
            ylab="Standardized Residuals", 
            xlab="Normal", 
            main="QQ plot" , pch =19,cex =0.5, col = "red") 
 qqline(res)

#from plot we can infer that the error term may not be normally distributed.

# (g) Find a 95% confidence interval for the regression coefficients.
  confint(model,level = 0.95)
#                 2.5 %       97.5 %
#   (Intercept)  3.756677e+03 4.543098e+03
# cs           5.409544e-05 1.520065e-03
# ic          -5.959506e+01 3.326302e+01
# hol1         4.976064e+02 7.495025e+02

# (h) Give 95% interval estimates for the average and predicted values associated with explanatory
# variables X1 = 300, 000, X2 = 5.9, and X3 = 1.
newdata <- data.frame(cs=300000,ic=5.9, hol=as.factor(1))
conf <- predict(model, newdata ,  interval="conf", level=0.95)
predict <- predict(model, newdata , interval="predict", level=0.95)
# 
# $fit
# fit      lwr      upr
# 1 4719.375 4474.986 4963.763
# 
# $se.fit
# [1] 121.5481
# 
# $df
# [1] 48
# 
# $residual.scale
# [1] 143.2895

# (i) Would a model with fewer variables make sense? Justify your answer, including appropriate test
# results.
Grocery <- as.data.frame(read.table("grocery.txt", header = T))
leaps( x=Grocery[,2:4], y=Grocery[,1], names=names(Grocery)[2:4],method = "Cp")

leaps( x=Grocery[,2:4], y=Grocery[,1], names=names(Grocery)[2:4],method = "r2")
# $which
# x1    x2    x3
# 1 FALSE FALSE  TRUE
# 1  TRUE FALSE FALSE
# 1 FALSE  TRUE FALSE
# 2  TRUE FALSE  TRUE
# 2 FALSE  TRUE  TRUE
# 2  TRUE  TRUE FALSE
# 3  TRUE  TRUE  TRUE
# 
# $label
# [1] "(Intercept)" "x1"          "x2"          "x3"         
# 
# $size
# [1] 2 2 2 3 3 3 4
# 
# $r2
# [1] 0.657038957 0.043124725 0.003603553 0.686223376 0.658067646 0.044935502 0.688334161
# 

# The best possible submodel is to include x1 and x3  which can be validated byy both R2 and CP


#2(a) Obtain the estimated regression equation, and explain the meaning of each coefficient.

data <- data.frame(read.table("copier.txt", header =T))
dat2 <-read.table("copier2.txt", header =T)
data$type <- as.factor(dat2$Type)

cp.lm <- lm(Time~., data = data)
coefficients(cp.lm)

# (Intercept)     Copiers       type1 
# -0.9224729  15.0461435   0.7587218

#b_0 is the time required to service 0 copiers.
# This isn't meaningful for this application, and
# the p-value on b_0 is large enough so that
# beta_0 = 0 is plausible.



2b)
  confint(cp.lm)
# type1       -4.851254  6.368698

# Type1 : its signifies the amount of extra time required to if the copier is big

#copiers : its the amount of time required to process one copier 


# Give a point and 90% interval estimate for the mean amount of time 
#required for a call to service# 3 large copiers
newdf <- data.frame(Copiers= 3, type =as.factor(1))

predict(cp.lm, newdf, interval="conf", level=.90)

#     fit      lwr      upr
# 1 44.97468 29.30695 60.64241

# (d) Find SSR, SSE, and SST. What is the change in SSR when copier type is included in the model
# versus when it is not?
 
  data$pred <- predict(object = cp.lm,newdata = data)
  SSE <- sum((data$Time- data$pred)^2) 
  SSR <- sum((data$pred - mean(data$Time))^2)
  SST <- sum((mean(data$Time)-data$Time)^2)
#SST = SSR + SSE

# What is the change in SSR when copier type is included in the model
# # versus when it is not?
data_u <- data.frame(read.table("copier.txt", header =T))
cp1.lm <- lm(Time~Copiers, data = data_u)
data_u$pred <- predict(cp1.lm,data_u)
SSR_U <- sum((data_u$pred - mean(data_u$Time))^2)
change = SSR- SSR_U

# (e) Make a scatter plot of number of copiers serviced versus time, with the type of copier coded in
# different colors. Decide if this suggests a difference between the two groups, and if so then give a
# revised model.,

ds<- data[data$type == 0,]
db <- data[data$type == 1,]
ds.lm <- lm(Time ~ Copiers, data =ds)
db.lm <- lm(Time ~ Copiers+type, data =db)
plot(data$Time~data$Copiers,col=data$type, pch =19, cex =.5,jitter=0.2)
abline(ds.lm,col = "black")
abline(db.lm,col = "red")

mod_new <- lm(Time~Copiers , data = data)
