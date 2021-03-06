# Wednesday, September 17
#
# Follow-up, a couple more outlier functions,
# and multicollinearity
#
install.packages("VIF")
library(VIF)
steel = read.csv("steel.csv", header = TRUE)
#
plot(steel,pch=20,cex=.2)
st.lm <- lm(units~width+temp+density+tensile, data=steel)
summary(st.lm)
#
# The "leaps" function (discussed in Part09)
# is a wrapper for "regsubsets" (both require
# the "leaps" package.)
#
st.sub <- regsubsets(x=steel[,2:5], y=steel[,1],nbest=2)
summary(st.sub)$which
#
# The diagnostic values are hidden with the summary
# of st.sub.  Here is R^2 for the selected models
summary(st.sub)$rsq
#
# We can put it all into a table for viewing
rsq <- summary(st.sub)$rsq
adjr2 <- summary(st.sub)$adjr2
cp <- summary(st.sub)$cp
bic <- summary(st.sub)$bic
cbind(summary(st.sub)$which,rsq, adjr2,cp,bic)
#
# There is also a nice graphical way of
# viewing the results
plot(st.sub, scale="adjr2")
plot(st.sub, scale="bic")

# Revisting outliers
#
# Some simple regression data
#
setwd("/media/prasad/Edu/Stat 6021/part10")
s2 = read.csv("simple02.csv", header = TRUE)
plot(s2,pch=20,cex=.2)
s2.lm <- lm(y~x, data=s2)
#
# The "dffits" function computes new values
# of y-hat when each data value is removed.
dffits(s2.lm)

#
# The "dfbetas" function computes new values
# for the regression coefficients when each
# data value is removed.
#
dfbetas(s2.lm)
#
# Multicollinearity
#
col = read.csv("collinear.csv", header = TRUE)
#
plot(col,pch=20,cex=.2)
#
col.lm <- lm(y~x1+x2+x3+x4, data=col)
#
# A summary of the values generated by lm:
#
summary(col.lm)
# 
# The "variance inflation factors" can be
# computed with the vif function
vif(col.lm)

#
# Activity: Select a model for the collinear
# data




data(housingexp)
summary(housingexp)
data(syn)
vif.sel <- vif(syn$y, syn$x, trace = FALSE);
vif.sel$select;
syn$true;
