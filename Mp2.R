# install.packages("stringr")
library(stringr)
library(ggplot2)
install.packages("psych")
library(psych)
# main <- rm(list=ls(all=T))
#setwd("/media/prasad/Edu/Stat 6021/MP2")

#Reading the data 
school_data <- read.csv("SchoolData.csv",header=T)

#adding an extra column type to identify the type of school (University or liberal college)
school_data$type <- c(rep("uni",52),c(rep("col",53)))

#Find the basic stats about the data
summary(school_data)
str(school_data)

# univ <- school_data[1:52,]
# 
# as.dat
# table(univ$academicCal)
# table(univ$state)
# as.data.frame(table(univ$city))

## this is not a normal curve 
ggplot(data = school_data , aes(x= acceptRate)) + geom_density()

scatterplot = ggplot(data = school_data, aes( x = studFacRatio, y = acceptRate, color = as.factor(type)))
scatterplot + geom_point()

univ <- school_data[1:52,1:20]
coll <- school_data[53:83,1:20]

# trying to understand the distribution of studFacRatio
ggplot(coll , aes(x= studFacRatio)) + geom_density()
#This is multi modal with  right skewed 
ggplot(univ , aes(x= studFacRatio)) + geom_density()
#Unimodal data with most student faculty ratio as 6


#Low ranked schools has lesser Student to faculty ratio compared to the high ranked ones 
plot_sfr <- ggplot(data = school_data, aes(x = studFacRatio,y=acceptRate , size = 1/rank, color = type))
plot_sfr + geom_point() +geom_jitter() + ggtitle("StudentFaculty Ratio Vs Acceptance rate")


#liberal colleges has similar tuition irresspective of acceptance rate
#highly selective national universites has higher tuition fee (40K  to 50K)
plot_tuition <- ggplot(data = school_data, aes(x = tuition,y=acceptRate,size = 1/rank, color = type))
plot_tuition + geom_point() +geom_jitter() + ggtitle("Tuition vs Acceptance")


#plotting enrollment vs Acceptance
# For liberal colleges enrollment doesnt have any effect on acceptance rate
#For national universities acceptance increases as enrollment rate increases
plot_enrollment <- ggplot(data = school_data, aes(x = enrollment,y=acceptRate, size = 1/rank, color = type))
plot_enrollment + geom_point() +geom_jitter() + ggtitle("enrollment vs Acceptance") #+
# geom_smooth(method = 'lm', se = FALSE)


#plotting retentionRate vs Acceptance
#Schools with high retetion rates are more selective in general 
plot_retentionRate <- ggplot(data = school_data, aes(x = retentionRate,y=acceptRate,size = 1/rank, color = type))
plot_retentionRate + geom_point() +geom_jitter() + ggtitle("retentionRate vs Acceptance") 


#Highly selective schools have more percentage of classes whose strength is less than 20
plot_classLT20 <- ggplot(data = school_data, aes(x = classLT20,y=acceptRate,size = 1/rank, color = type))
plot_classLT20 + geom_point() +geom_jitter() + ggtitle("classLT20 vs Acceptance") 

#Highly selective schools have fewer percentage of classes whose strength is greaterthan 50
plot_classGT50 <- ggplot(data = school_data, aes(x = classGT50,y=acceptRate,size = 1/rank, color = type))
plot_classGT50 + geom_point() +geom_jitter() + ggtitle("classGT50 vs Acceptance") 



# plot_studFacRatio <- ggplot(data = school_data, aes(x = studFacRatio,y=acceptRate,size = 1/rank, color = type))
# plot_studFacRatio + geom_point() +geom_jitter() + ggtitle("classLT20 vs Acceptance") 
# geom_smooth(method = 'lm', se = FALSE)


#Finding the correlation between all independent variables 
#taking threshold as .75 we are dorpping the indicator varaiable which are highly correlated 
pairs.panels(school_data[3:20])

train_data  <- school_data[c(1:41,53:84),]
#Intial model after removing the highly correlated indicator variables. 
lm0 <- lm(acceptRate~studFacRatio+schoolType+setting+academicCal+enrollment+yearFounded+gradRate+classLT20
          +counselorScore+perFemale+endowment+type, data = train_data)
summary(lm0)
plot(lm0, which = 4)
plot(lm0, which = 2)
#from cooks distance and leverage we can remove the outliers which are obs 29,60 and 84


#Removing outliers 
trainData <- train_data[-c(29,60,84),]
trainData <- trainData[c(1:70),]  

#finding the model after removing the outliers 
lm1 <- lm(acceptRate~studFacRatio+schoolType+setting+academicCal+enrollment+yearFounded+gradRate+classLT20
          +counselorScore+perFemale+endowment+type, data = trainData)
summary(lm1)
plot(lm1, which = 2)

#Removing type category as its not contributing much to the model(p > 0.05)

lm2 <- lm(acceptRate~studFacRatio+schoolType+setting+academicCal+enrollment+yearFounded+gradRate+classLT20
          +counselorScore+perFemale+endowment, data = trainData)
summary(lm2)

#Removing academicCal category and endowment as its not contributing much to the model(p > 0.05)
lm3 <- lm(acceptRate~studFacRatio+setting+enrollment+yearFounded+gradRate+classLT20
          +counselorScore+perFemale, data = trainData)
summary(lm3)

#Removing studFacRatio studFacRatio setting enrollment yearFounded as its not
#contributing much to the model(p > 0.05) and roped in the academicCal variable
lm4 <- lm(acceptRate~gradRate+classLT20
          +counselorScore+perFemale+academicCal, data = trainData)
summary(lm4)


testdata <- school_data[c(42:46,85:89),]
values <- cbind(predict(lm4,testdata, int = "conf"),actualAcceptanceRate = testdata$acceptRate)
values
#   fit         lwr       upr             actualAcceptanceRate
# 42 38.38213 36.51322 40.25104                36.90
# 43 36.36966 34.23471 38.50461                32.30
# 44 32.60631 35.96041 45.25221                41.20
# 45 38.02205 34.82318 41.22093                41.10
# 46 40.74358 37.98141 43.50574                62.40
# 85 28.31286 26.65002 29.97570                29.50
# 86 31.91449 29.01515 34.81384                33.00
# 87 35.35349 32.82913 37.87785                34.10
# 88 36.69391 34.27376 39.11407                14.50
# 89 35.19984 32.66352 37.73617                44.50

#results inferences 
# In university testdata out of 5 4 are in 95% confidence interval range and 5th whose actual acceptance rate is 
# 62.4% (> max(acceptRate i.e 43.1))that requires extrapolation as a result its not giving appropriate predicitions 

#For liberal colleges out of 5 the model predics 3 correctly with in the ranges of 95% confidence interval levels
# And 5th observation in test data(44.5 acceptance rate) is greater than max(acceptRate) in trainig dataset.

write.table(school_data,file = "data.txt")
