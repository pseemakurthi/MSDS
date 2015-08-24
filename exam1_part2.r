main <- rm(list=ls(all=T))
setwd("/media/prasad/Edu/Stat 6021/exam")
install.packages("car","gvlma")
install.packages("psych")
library(gvlma)
library(psych)
library(car)

#Reading the data 

data <- read.table("exam01.txt",header = T,nrows = 45,colClasses = c("factor",rep("numeric",6)))

names(data)
eval <- read.table("exam01.txt",header = F,skip = 47,colClasses = c("factor",rep("numeric",5)))
names(eval) <- c("Region",  "Pop",     "SATV",    "SATM",    "PerTake", "PerNoHS")
#Splitting the data to test and train 
set.seed(12343)
rows<-seq(1:45)
set.seed(11)
trainRows=sample(rows,38)
remainingRows<-rows[-(trainRows)]
set.seed(12)
testRows=sample(remainingRows, 7)
train_data = data[trainRows,] 
test=data[testRows,] 




# 5 - point statistics of the data
str(train_data)
summary(train_data)



#Looking at the dixtribution of Region 
table(train_data$Region)
# The number of values for Region3 is less comapred to remaining two regions.


##Finding the corelation between numeric variables (Testing for multicollinearity).
#Region is ignored as its a categorical value.  

cor(train_data[c(2:7)]) 

## test for assumptions of multi-collinearity of independent variables.
# 
# Pop        SATV        SATM    PerTake     PerNoHS        Pay
# Pop      1.0000000 -0.28382380 -0.16461193  0.1800286  0.13618759  0.3144773
# SATV    -0.2838238  1.00000000  0.96588384 -0.8707232  0.06342497 -0.4927864
# SATM    -0.1646119  0.96588384  1.00000000 -0.8451120 -0.04437536 -0.4249569
# PerTake  0.1800286 -0.87072325 -0.84511197  1.0000000 -0.23249416  0.6214656
# PerNoHS  0.1361876  0.06342497 -0.04437536 -0.2324942  1.00000000 -0.3422650
# Pay      0.3144773 -0.49278636 -0.42495691  0.6214656 -0.34226496  1.0000000



pairs.panels(train_data[c("Pay","Region","Pop","SATV","PerNoHS")])
plot(train_data$Region,train_data$PerTake, col = train_data$Region)


scatterplotMatrix(train_data[c(2:7)],spread = F , lty =2, main = "Scatter Plot Matrix" )


model <- lm(Pay~., data = train_data)
#finding outliers
influencePlot(model, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook’s distance")

#outlier could be removed 
cutoff <- 4/(nrow(train_data)-length(model$coefficients)-2)
plot(model, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

summary(model)
vif(model) 
sqrt(vif(model)) > 2 ## We can remove either SATV


model1 <- lm(Pay~Region+Pop+SATM+PerTake+PerNoHS, data = train_data)
summary(model1)
vif(model1)
sqrt(vif(model1)) > 2 

anova(model1,model)
#can drop model
AIC(model1,model2)

model2 <- lm(Pay~Region+Pop+SATM+PerNoHS, data = train_data)
summary(model2)
vif(model2)
sqrt(vif(model2)) > 2 

anova(model2, model1)
#model1 can be dropped 

install.packages("MASS")
library(MASS)
stepAIC(model, direction="both")
#final model which gives min AIC and max Adjusted R-squared values.
mod <- lm(formula = Pay ~ Pop + PerTake + PerNoHS, data = train_data)
train_data$pred <- predict(mod,train_data)

vif(mod)
summary(mod)

#model with interactions
#On general percent of high school students taking SAT is assocaited with Population of sate, used
#interaction to get the combined effect of both

mod_int <- lm(formula = Pay ~ Pop*PerTake + PerTake + PerNoHS, data = train_data)
summary(mod_int)
#the model satastistic say insignificant
percent of high school students taking SAT)
#Similarly 
mod_int1 <- lm(formula = Pay ~ Pop*PerNoHS + PerTake + PerNoHS, data = train_data)
summary(mod_int1)

#though the above interaction gives improved Adjusted R-squared but the improvements are not significant 


# 2. Would any transformations significantly improve the model? Explain briefly which transformations
# you tried, and if there are significant improvements, give the improved model.

# Transforamtions are done generally when model's assumptions such as normality, linearity, or homoscedasticity
# are not met

plot(mod , which =2)
#from the plot we can see that the data is normally distributed
residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}

residplot(mod)

#Independence of error we can use Durbin-watson.

durbinWatsonTest(mod)

# p-value is = 0.572  which we can asume the the dependent variables are independent of each other
#i.e no correlation between explanatory variables.

# Linearity 
crPlots(mod)
#the plots are all linear

#HOMOSCEDASTICITY
spreadLevelPlot(mod)
# No transformation is required

mod_trans <- lm(formula = Pay ~ Pop + I(Pop^2) + PerTake + I(PerTake^2) + PerNoHS, data = train_data)

  summary(mod_trans)

mod_log <- lm(formula = log(Pay) ~ log(Pop) + log(PerTake) + log(PerNoHS), data = train_data)
summary(mod_log)

# Below function from Cars package will asses if there is any need for transformation 
 boxTidwell(Pay ~ Pop + PerTake + PerNoHS, data = train_data)


# Score Statistic   p-value MLE of lambda
# Pop          -0.3205234 0.7485716      0.921698
# PerTake       0.1302789 0.8963458      3.113614
# PerNoHS       1.8017224 0.0715891     -7.695435

# As we could see the p-values for all the independent variables are high which suggest that there is no need
# of transformation

# 3. Use your model from 1. to predict teacher salaries for the 6 states that do not include that information
# and were not used in creating the model.

eval$pay <- predict(mod,eval)





