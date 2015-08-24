# Friday, August 29
#
# Simulation examples
#

# Generating random values from a uniform
# distribution

runif(10, min=0, max=10)

x <- runif(1000, min=0, max=10) # Bigger sample!
hist(x, breaks=20, xlim=c(0,10))

mean(runif(4,min=0, max=10)) # Mean of 4

# "replicate" allows you to repeat the same
# code a specified number of times.

replicate(10,{mean(runif(4,min=0, max=10))})

# 1000 means for samples of size 4
y <- replicate(1000,{mean(runif(4,min=0, max=10))})
hist(y, breaks=20, xlim=c(0,10))

# 1000 means for samples of size 10
y <- replicate(1000,{mean(runif(10,min=0, max=10))})
hist(y, breaks=20, xlim=c(0,10))

# 1000 means for samples of size 25
y <- replicate(1000,{mean(runif(25,min=0, max=10))})
hist(y, breaks=20, xlim=c(0,10))

# "rt" generates random values from t-distributions
rt(10, df=1.5)

# 1. Plot a histogram of 1000 random t-values, df=1.5
# 2. Plot a histogram of 1000 means of samples of size
#    4 of random t-values, df=1.5
# 3. Repeat 2., but with a sample of size 16.
# 4. Repeat 2., but with a sample of size 36.
# 5. Comment on histogram shape.








x <- rt(1000,df=1.5)
hist(x, breaks=200)

y <- replicate(1000,{mean(rt(4,df=1.5))})
hist(y, breaks=200)

y <- replicate(1000,{mean(rt(16,df=1.5))})
hist(y, breaks=200)

y <- replicate(1000,{mean(rt(36,df=1.5))})
hist(y, breaks=200)


