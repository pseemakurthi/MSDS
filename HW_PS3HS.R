

# Homework Questions
#
# 1. Develop a linear model with "Price" as the
#    response variable.  Do not use any transformations
#    on the data, just develop a model with variables
#    removed as appropriate

  bulls_model1 = lm(Price~., data=bulls)
  summary(bulls_model1)
 # from summary we can remove PctFFB SaleHt SaleWt
  
  bulls_model2 = lm(Price~YrHgt+FtFrBody+BkFat, data=bulls)
  summary(bulls_model2)


# 2. Find the principal components for the explanatory
#    variables.  (All of them, not just those in your
#    model from #1.)
#    a) Decide how many PC's are needed to explain
#       most of the variation in your data.

bulls_pc = princomp(bulls[,2:7], cor=T)
bulls_pc

bulls_pc$loadings
bulls_pc$sdev^2
summary(bulls_pc)
plot(bulls_pc, type="lines",pch=20,cex=1)
# we can chosse 4 principal components as they were able to explain more than 95% of variation
#in the data


#    b) With the PC's from (a), fit a linear model
#       with Price as response.  How does it compare
#       to the linear model from #1?

bulls_final = lm(Price~bulls_pc$scores[,1:4], data = bulls)
summary(bulls_final) #Adjusted R-squared:  0.4188
summary(bulls_model2) #Adjusted R-squared:  0.4138


# The model with principal components is a decent one as the Adjusted r2 and r2 for nearly same
# as the number of feature vectors in bulls_pc and bulls_model2 are same PCA could be a (overhead)
# unnecessary considering the data set. 
