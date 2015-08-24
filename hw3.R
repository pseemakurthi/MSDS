main <- rm(list=ls(all=T))
setwd("/media/prasad/Edu/Stat 6021/Hw3/")
install.packages("psych")
library(psych)
install.packages("lmtest")
library("lmtest")
# Given the sequential nature of the data, it may be that the error terms are autocorrelated. Carry out
# the appropriate test to determine if this is the case. If so develop an appropriate model that addresses
# the autocorrelation problem.

grocery <- read.table("grocery.txt", header = T) 

model_grocery <- lm(y~., data= grocery)
summary(model_grocery)

#durbin watson test for detecting autocorrelation
dwtest(model_grocery)
# p-value = 0.8283 
# we are ignoring alternate hypothesis in favour of null there is no autocorrelation
-------------------------------------------------------------------------------------------------------------

# The file flu.txt contains 159 patient records, and includes age (x1), health awareness index (x2),
# gender (x3, coded as 0 for female, 1 for male), and if a flu shot had been received (1 = yes). The
# records were randomly selected from among a large number of patients who received information
# encouraging them to get the vaccine, with the goal of increasing vaccination rates.

flu <- read.table("flu.txt",header = T ,colClasses =c("factor","numeric","numeric","factor")) 
                  
colnames(flu) <- c("shot","age","index","gender")
summary(flu)

pairs.panels(flu)
#as index level goes up people who takes shots are going down 
xtabs(~shot  + index, data = flu)

xtabs(~shot + age, data = flu)

#(a) Obtain estimates for the coefficients ??0, ??1, ??2, and ??3.

mod <- glm(shot~ age + index + gender, data = flu ,family="binomial")

summary(mod)
#   (Intercept) -1.17716    2.98242  -0.395  0.69307   
#   age          0.07279    0.03038   2.396  0.01658 * 
#   index       -0.09899    0.03348  -2.957  0.00311 **
#   gender1      0.43397    0.52179   0.832  0.40558 

# shot = -1.17716 + 0.07279 * age + -0.09899 *index +  0.43397 *gender
-------------------------------------------------------------------------------------------------------------
# (b) What proportion of 55 year old male patients with a health awareness index of 60 can be expected
# to get a flu shot?
 new_data <- data.frame(age=55,index = 60, gender = as.factor(1) )
 predict_value <- predict(mod,  new_data, type = "response")
 # 0.06422197 6.4% 
-------------------------------------------------------------------------------------------------------------
# (c) Among 40 year old female patients, how big an increase in vaccination rates will be yielded by a
# 10-point increase in health awareness index?
new_data_1 <- data.frame(age=40,index = 45, gender = as.factor(0))
new_data_2 <- data.frame(age=40,index = 55, gender = as.factor(0))
predict(mod,  new_data_1, type = "response") - predict(mod,  new_data_2, type = "response")

#there is a decrease of 3.79 % 
-------------------------------------------------------------------------------------------------------------
# (d) Does it appear that any of the variables can be removed from the model? Explain your reasoning.
#Gender can be removed from the model

# based of summary of the model   we can remove Gender and intercept value as their p-values are insignificant

-------------------------------------------------------------------------------------------------------------
# (e) Determine if any of the following transformed variables appear to improve the model: the square
# of age; the square of health index; the interaction of gender and health index; the interaction of
# gender and age.
 mod_update1 <- glm(shot ~age + index + gender + I(age*age), data= flu ,  family = "binomial")
 summary(mod_update1)
  # no effect of transforming age and infact its making age variable insignificant
mod_update2 <- glm(shot ~age + index + gender + I(index*index), data= flu ,  family = "binomial")
summary(mod_update2)
# no effect of transforming index and infact its making index variable insignificant
mod_update3 <- glm(shot ~age + index + gender + gender:index, data= flu ,  family = "binomial")
summary(mod_update3)
# no improvement of using interaction on gender and age.

mod_update <- glm(shot~ age + I(age*age) + index + I(index*index)+ gender,+ gender:index,  data = flu ,family="binomial")
summary(mod_update)
# Using all transformation there is no improvement in the model.
---------------------------------------------------------------------------------------------------------------
# 3. The file advert.csv contains monthly data on billings (y), staff expenses (x1), and overhead expenses
# (x2), starting with January 2013 and continuing through August 2014.

adver <- read.csv("advert.csv",header = T)
colnames(adver) <- c("bill","sexp", "oexp")
summary(adver)
str(adver)
# (a) Fit a linear regression model to the data, and obtain estimates for the model coefficients.
fit <- lm(bill~.,data = adver)
# (Intercept)     sexp         oexp  
# 94.54514      0.85650     -0.02429  
bill = 94.54514  +  0.85650 *sexp - -0.02429 * oexp
---------------------------------------------------------------------------------------------------------------

# (b) Plot the residuals, and comment on whether there appears to be autocorrelation in the data.
  plot(fit, which =1)
 #there appears to be some kind of auto correlation.
-------------------------------------------------------------------------------------------------------------
# (c) Carry out a formal test for autocorrelation.
dwtest(fit)
plot(fit$residual,pch=20,cex=.2)
#this says there is some autocorrelation as p-value is lower we can ignore the null hypothesis
-------------------------------------------------------------------------------------------------------------
# (d) Based on the preceding results, develop an appropriate model. Use your model to predict the
# billings for September 2014, given staff expenses of 230 and overhead expenses of 120.

res <- as.vector(fit$residuals)
r <- sum(res[2:20]*res[1:19])/sum(res[1:19]^2)
r

# Removing the autocorrelation factor
sexpt <- adver$sexp[2:20] - r*adver$sexp[1:19]
oxept <- adver$oexp[2:20] - r*adver$oexp[1:19]
billt <- adver$bill[2:20] - r*adver$bill[1:19]



## Now developing a linear model
fit.lm2 <- lm(billt~ sexpt+oxept)
summary(fit.lm2)
dwtest(fit.lm2)

b0 <- 59.59077/(1-r) #intercept
# 96.49895
b1<- 0.85115   #sexpt
b2 <- -0.02573 #oxept


#forecasting
#removing the august term to get the error
error <- adver$bill[20] - (b0 + b1*adver$sexp[20] + b2*adver$oexp[20] )
bill_sept <- b0 + b1*230 + error*r + b2*120 
#288.7899
-------------------------------------------------------------------------------------------------------------
# (e) Returning to the data, determine if any of the variables might be removed from the model. If so,
# then do so and repeat the preceding parts. Does the prediction from above change much?

#from the summary we can remove other exp variable 

fit.lm3 <- lm(bill~sexp, data = adver)
summary(fit.lm3)
dwtest(fit.lm3)




res_new <- as.vector(fit.lm3$residuals)
r_new <- sum(res_new[2:20]*res_new[1:19])/sum(res_new[1:19]^2)
r_new

sexpt_update <- adver$sexp[2:20] - r_new*adver$sexp[1:19]
billt_update <- adver$bill[2:20] - r_new*adver$bill[1:19]

fit.lm4 <- lm(billt_update~sexpt_update) 

b0_new = 63.383984/(1-r_new)
b1_new <- 0.842449 

error_new <- adver$bill[20] - (b0_new + b1_new*adver$sexp[20])
bill_sept_new <- b0_new + b1_new*230 + error_new*r_new  

#there is no big change in the prediction level.
#288.8843

