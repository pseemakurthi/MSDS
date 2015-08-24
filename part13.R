# Friday, September 26
#
# Partial F-tests
#
# setwd("/media/prasad/Edu/Stat 6021/part13")
setwd("H:/Stat 6021/part13")
s1 = read.csv("icecream2.csv", header = TRUE)

f1.lm <- lm(cons~temp+inc+price+pop, data=s1)
summary(f1.lm)

# It appears that "price" and "pop" might
# not contribute to the model.  Define
# a reduced model that does not include
# them

p1.lm <- lm(cons~temp+inc, data=s1)
summary(p1.lm)

# The "anova" function will compute
# SSR for both models, and give the
# difference along with the F test

anova(p1.lm,f1.lm)

# A different example

s2 = read.csv("steel.csv", header = TRUE)
names(s2)<-c("y","x1","x2","x3","x4")

f2.lm <- lm(y~x1+x2+x3+x4, data=s2)
summary(f2.lm)

# Only x3 appears to be significant,
# let's remove the rest and compare
# to the full model

p2.lm <- lm(y~x3, data=s2)
summary(p2.lm)
anova(p2.lm,f2.lm)

# It seems at least one of the other
# variables is significant.  Let's 
# add x2 back in, as it had the
# next smallest p-value in the full
# model

p2b.lm <- lm(y~x2+x3, data=s2)
summary(p2b.lm)
anova(p2b.lm,f2.lm)

# Even with x2 and x3 present, 
# either x1 or x4 is significant.
# Also x2 is not significant, so
# let's replace it with x4

p2c.lm <- lm(y~x3+x4, data=s2)
summary(p2c.lm)
anova(p2c.lm,f2.lm)
# The bodyfat data (original version)

s3 = read.csv("bodyfat2.csv", header = TRUE)

f3.lm <- lm(y~x1+x2+x3, data=s3)
summary(f3.lm)

# None appear to be significant, but
# R^2 is pretty good.  This could be
# multicollinearity -- check VIFs
install.packages("VIF")
library(VIF)
vif(f3.lm)

# x1 has the smallest p-value in the
# original model.  Let's try that alone.

p3.lm <- lm(y~x1, data=s3)
summary(p3.lm)

# Now x1 is significant, and R^2 did
# not change too much.  Let's compare
# with the full model.

anova(p3.lm,f3.lm)

# The p-value for the F test is right
# in the boundary for significance.
# Let's try a different variable in 
# place of x1.

p3b.lm <- lm(y~x3, data=s3)
summary(p3b.lm)

# Nope -- maybe x2

p3c.lm <- lm(y~x2, data=s3)
summary(p3c.lm)

# Maybe a model with x1 and x2

p3d.lm <- lm(y~x1+x2, data=s3)
summary(p3d.lm)

# Hmm, multicollinearity?
vif(p3d.lm)

# Verify x3 should be dropped.
anova(p3d.lm,f3.lm)

# Let's compare the 3 possible
# models again:

summary(lm(y~x1, data=s3))
summary(lm(y~x2, data=s3))
summary(lm(y~x1+x2, data=s3))

# Possibly x2 alone is best.

p3c.lm <- lm(y~x2, data=s3)
anova(p3c.lm,f3.lm)

