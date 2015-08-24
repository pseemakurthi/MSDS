install.packages("infotheo")
install.packages("DMwR")
library(DMwR)
library(infotheo)
library(C50)

main <- rm(list =ls(all = T))
setwd("H:/Stat 6021/Final_project")
load("NatalRiskData.rData")
univ <- sdata[,-c(13)]
univ$atRisk <- sdata[,13]
PWGT <- discretize(sdata$PWGT, disc="equalfreq", nbins=5)$X
UPREVIS <- discretize(sdata$UPREVIS, disc="equalfreq", nbins=5)$X
DBWT <- discretize(sdata$DBWT, disc="equalfreq", nbins=5)$X

univTemp <- data.frame(PWGT,UPREVIS,DBWT)
univ1 <- cbind(univTemp,univ[,-c(1,2,13,14)])

class <- function(x){
  x <- as.factor(x)
  return(x)
}

univ1 <- data.frame(apply(univ1,2,class))
summary(univ1)


rows<-seq(1:26313)
set.seed(11)
trainRows=sample(rows,20313)
remainingRows<-rows[-(trainRows)]
set.seed(12)
testRows=sample(remainingRows, 4000)
evalRows=rows[-c(trainRows,testRows)]


train = univ1[trainRows,] 
test=univ1[testRows,] 
eval=univ1[evalRows,]

rm(rows, trainRows, testRows, evalRows, remainingRows,univ1)

summary(train)
summary(test)
summary(eval)

trainS = SMOTE(atRisk ~ ., 
               train, 
               perc.over = 10000, 
               k=5)

dtC50= C5.0(atRisk ~ ., 
            data = trainS[], 
            rules=TRUE)

summary(dtC50)
C5imp(dtC50, pct=TRUE)
a=table(train$atRisk, 
        predict(dtC50, 
                newdata=train, 
                type="class"))
rcTrain=(a[2,2])/(a[2,1]+a[2,2])*100
a=table(test$atRisk, predict(dtC50, newdata=test, type="class"))
rcTest=(a[2,2])/(a[2,1]+a[2,2])*100

dtC50= C5.0(atRisk ~ ., 
            data = trainS, 
            rules=TRUE)
summary(dtC50)
C5imp(dtC50, pct=TRUE)

# a=table(trainS$loan, 
#         predict(dtC50, 
#                 newdata=trainS, 
#                 type="class"))
# rcTrain=(a[2,2])/(a[2,1]+a[2,2])*100
# a=table(test$atRisk, predict(dtC50, newdata=test, type="class"))
# rcTest=(a[2,2])/(a[2,1]+a[2,2])*100
dtCart=rpart(atRisk ~.,data=trainS, 
             method="anova")    
plot(dtCart,main="Classification Tree for atRisk",margin=0.15,uniform=TRUE)
text(dtCart,use.n=T)



