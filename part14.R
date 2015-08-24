# Monday, September 29
#
# Logistic regression
#
# Data on success on a programming task.
# x = months of experience, y = 1 if
# successful in completing task.
s1 = read.csv("programming.csv", header = TRUE)

# "glm" can be used for various types of
# models. Below we call logistic regression
s1.lg <- glm(success~months, data = s1, 
             family = "binomial")
summary(s1.lg)
fit1<-predict(s1.lg, type="response")
as.vector(fit1)

plot(as.vector(fit1)~s1$months,pch=20,cex=.2)

# confint can provide confidence intervals
# for the coefficients
confint(s1.lg, level=0.96)

# We can have R predict model values
# from a list of inputs
newdata = data.frame(months=c(10,15,20))
p1<-predict(s1.lg, newdata, type="response")
p1

# The "(link="probit")" option gives
# the probit model.
s2.lg <- glm(success~months, data = s1, 
         family = "binomial"(link = "probit"))
summary(s2.lg)

# The estimated responses
fit2<-predict(s2.lg, type="response")
as.vector(fit2)
plot(as.vector(fit2)~s1$months,pch=20,cex=.2)
# Compare logit to probit -- they
# produce similar results
plot(as.vector(fit1)~as.vector(fit2),
     pch=20,cex=.2)