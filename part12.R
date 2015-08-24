# Wednesday, September 24
#
# Qualitative variables
#

s1 = read.csv("insurance1.csv", header = TRUE)

# qplot requires ggplot2 package
qplot(size,time,color = type,data = s1)

s1.lm <- lm(time~size+type, data=s1)
summary(s1.lm)

# Does interaction matter?

s1i.lm <- lm(time~size*type, data=s1)
summary(s1i.lm)

# A different version of the insurance data
s2 = read.csv("insurance2.csv", header = TRUE)

qplot(size,time,color = type,data = s2)

s2.lm <- lm(time~size+type, data=s2)
summary(s2.lm)

# Does interaction matter this time?

s2i.lm <- lm(time~size*type, data=s2)
summary(s2i.lm)

# Categorical data with more than two levels

# A different version of the insurance data
s3 = read.csv("insurance3.csv", header = TRUE)

qplot(size,time,color = type,data = s3)

s3.lm <- lm(time~size+type, data=s3)
summary(s3.lm)

# The above model isn't right, because the
# categorical variable "type" is treated
# like it is quantitative.

# The "factor" function will change the 
# variable into a factor so that R will
# treat is as categorical in the lm function

s3$type <- factor(s3$type)
is.factor(s3$type)

s3.lm <- lm(time~size+type, data=s3)
summary(s3.lm)

# The file "insurance4.csv" has the data
# coded correctly, so we can compare to 
# the output from s3.lm

s4 = read.csv("insurance4.csv", header = TRUE)
s4.lm <- lm(time~size+type1+type2, data=s4)
summary(s4.lm)

# Does interaction matter?
s3i.lm <- lm(time~size*type, data=s3)
summary(s3i.lm)
