# Monday, September 1
#
# Simulation, confidence intervals, hypothesis
# tests
#
# Sampling from a set of data
#
x <- c(2,4,4,4,5,5,7,9) # Define a small set

sample(x, size=4)  # Gives a subset of size 4

# The function "sd" gives the sample standard 
# deviation s.  To get the population standard
# deviation sigma, we multiply by sqrt(7/8)

sd(x)*sqrt(7/8)

#    Suppose that we take a large number of independent
#    samples of size 4, and compute the mean of each.
#
# 1. What do we expect the standard deviation of these
#    sample means to equal?
#
# 2. Simulate: Generate 1000 independent samples of size
#    4, and find the (population) standard deviation for
#    the sampling distribution.  What do you get?


sampmeans <- replicate(1000,{mean(sample(x, size=4))})
sd(sampmeans)*sqrt(999/1000)

# Why didn't we get 1?

# Answer: Because sampling by default is without 
# replacement, but the formula sigma/sqrt(n) assumes
# with replacement sampling.  

sampmeans <- replicate(1000,
{mean(sample(x, size=4,replace=TRUE))}
)
sd(sampmeans)*sqrt(999/1000)

# There is a version of the formula for without
# replacement sampling, which includes the "finite
# population correction factor" given by
# sqrt((N-n)/(N-1))
#
# For a sample of size n=4 and our population of size
# N=8, we expect the sampling distribution to have
# standard deviation:

(2/sqrt(4))*sqrt((8-4)/(8-1))

# Simulate one more time

sampmeans <- replicate(1000,
{mean(sample(x, size=4))}
)
sd(sampmeans)*sqrt(999/1000)

#
# Confidence intervals
#
# 
qnorm(.975, mean=0, sd=1) # Upper crit val for 95% conf int
#
# Select random sample of size n=4 from N(60,10) population
#
x <- rnorm(4, mean=60, sd=10)
x
# Standardize -- Z score;
(mean(x)-60)/(10/sqrt(4)) # Usually in [-1.96,1.96]
#
# Let's replicate 50,000 times, and count proportion
# that are in [-1.96,1.96].  
#
n <- 50000
y <- replicate(n,
{x <- rnorm(4, mean=60, sd=10)
(mean(x)-60)/(10/sqrt(4))})

z <- rnorm(4,40,10)

sum(abs(y)<1.96)/n  # This should be about .95.

# In most cases, we do not know the standard deviation
# of the population, so we use the sample standard 
# deviation in place of sigma.  Let's simulate again.

n <- 50000
y <- replicate(n,
{x <- rnorm(4, mean=60, sd=10)
 (mean(x)-60)/(sd(x)/sqrt(4))})

sum(abs(y)<1.96)/n

# The qt function will give us the upper crit. value
qt(.975,df=3)
# 
# Let's simulate again, this time changing the cut-off
#
n <- 50000
y <- replicate(n,
{x <- rnorm(4, mean=60, sd=10)
 (mean(x)-60)/(sd(x)/sqrt(4))})

sum(abs(y)<3.182)/n

