# Wednesday, September 3
#
# Hypothesis tests and an intro to bootstrap methods
# 
#
# Sample: If t=2.03 (df=20), then the p-value for a 
# 2-sided test is:

2*(1-pt(2.03,df=20))

# 1. Draw a random sample of size 200000 from a normal
#    distribution with mean 60.1 and standard deviation
#    10.
# 2. Compute the p-value corresponding to testing the
#    null hypothesis mu=60 vs mu not= 60.
#

###################################

################################33






x <- rnorm(200000, mean=60.1, sd=10)
(mean(x)-60)/(sd(x)/sqrt(200000))

2*(1-pt(XXX,df=999999))

##
## Bootstrap examples
##

# Generate a random sample from a normal distribution
s <- rnorm(10, mean=60, sd=10)

# Use sample to create usual t-confidence interval

tcrit <- qt(.975,df=9)
ucl <- mean(s)+tcrit*(sd(s)/sqrt(10))
lcl <- mean(s)-tcrit*(sd(s)/sqrt(10))
c(lcl,ucl) 

# Now we try bootstrap, percentile method

y <- replicate(999,{mean(sample(s,size=10,replace=TRUE))})
ci <- sort(y)[c(25,975)]  # This gives the middle 95% of means
ci

samp
# What fraction of the time do the resulting intervals contain
# the true mean 60?  We generate different samples s and see 
# what happens.

# First the usual t-confidence interval

s <- rnorm(10, mean=60, sd=10)
ucl <- mean(s)+tcrit*(sd(s)/sqrt(10))
lcl <- mean(s)-tcrit*(sd(s)/sqrt(10))
lcl <= 60 & ucl >= 60   # TRUE if 60 in CI, FALSE if not

# Now let's try this 1000 times, and find the proportion 
# times the interval contains 60
z <- replicate(1000,{
  s <- rnorm(10, mean=60, sd=10)
  ucl <- mean(s)+tcrit*(sd(s)/sqrt(10))
  lcl <- mean(s)-tcrit*(sd(s)/sqrt(10))
  lcl <= 60 & ucl >= 60
})
sum(z)/1000

# Now the bootstrap method
# 

s <- rnorm(10, mean=60, sd=10)
y <- replicate(999,{mean(sample(s,size=10,replace=TRUE))})
ci <- sort(y)[c(25,975)]
ci[1] <= 60 & ci[2] >= 60   # TRUE if 60 in CI, FALSE if not

# Now let's see what happens with 1000 distinct samples
#
z <- replicate(1000,{
  s <- rnorm(10, mean=60, sd=10)
  y <- replicate(999,{mean(sample(s,size=10,replace=TRUE))})
  ci <- sort(y)[c(25,975)]
  ci[1]<= 60 & ci[2] >= 60
})
sum(z)/1000

# 1. Select a random sample of size 10 from an exponential
#    distribution with rate = 0.05 (mean = 20).  The 
#    function required is rexp(n, rate= )
#
# 2. Use the bootstrap with 999 replications to generate
#    a 95% confidence interval for the mean.

#############################################3

s <- rexp(10,rate = 0.05)
y <- replicate(999, {mean(sample(s,10,replace = T))})
z<- sort(y)[c(25,975)]
################################################


#
# The "boot" package (start by loading)
#
install.packages("boot")
library(boot)
# Define special mean function that takes the sample and
# a vector d of bootstrap indices as input
#
samplemean <- function(x, d) {
  return(mean(x[d]))
}
#
# The "boot" function requires the original sample, the
# special function, and the number of resamples.  It 
# the creates an object that can be called
#
  my.boot <- boot(s, samplemean, R=999)
#
# We use "boot.ci" to generate the confidence interval.
# Type "perc" uses the percentile method.
#
boot.ci(my.boot, type="perc")
#
# You can extract the confidence limits with "my.boot$t"
# which is a vector of each resampled statistic value
#
sort(my.boot$t)[c(25,975)]
#
# One can use the bootstrap method to estimate other
# statistics.  The distribution isn't very important
#
# The sample median
#
samplemedian <- function(x, d) {
  return(median(x[d]))
}
#
my.boot <- boot(s, samplemedian, R=999)
#
boot.ci(my.boot, type="perc")
#
#
# There are variants of the percentile method.  A common
# one uses the sorted residuals 
#
s <- rnorm(10, mean=60, sd=10)
y <- replicate(999,{mean(sample(s,size=10,replace=TRUE))})
sortedres <- sort(y - mean(s))
ci <- mean(s) - sortedres[c(975,25)]
ci

# Applying "boot" with type="basic" (residuals)

my.boot <- boot(s, samplemean, R=999)
boot.ci(my.boot, type="basic")

# Check data directly
sortedres <- sort(my.boot$t - mean(s))
mean(s) - sortedres[c(975,25)]
