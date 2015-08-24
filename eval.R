main <- rm(list = ls(all =T))
setwd("H:/Machine_learning/HW1")
install.packages("RWeka","C50")
library(RWeka)
library(infotheo)
library(C50)
library(caret)
library(e1071)
library(rpart)
mail_d <- read.arff(file = "spambase.arff")
table(mail_d$word_freq_make)

#building a data dictionary 
data_dict <- as.data.frame(cbind(sapply(mail_d, typeof))) ## all the columns are numeric except class

####Converting all cloumns to categroical for Desicion tree to work.
mail <- mail_d[,-58]

### Converting classes to bins 
## reason for no choosing many bins? 
##Choosing large bins may not be ideal we might inflate the randomness in that particular variable 
## and the algorithm might choose the variable with more number of bins as root by default

mail <- as.data.frame(sapply(mail, function(x) as.factor(discretize(x, disc="equalfreq", nbins=5)$X ))) 
mail$class <- mail_d$class

str(mail)
summary(mail)

table(mail$class) ## the class distribution is 61: 39 in the actual data. 

trainindex<-(createDataPartition(mail$class,p=.8, list= F))
train<- mail[trainindex,]
test<- mail[-trainindex,]

t1= C5.0(class ~ ., 
            data = train[,c(1:5,58)], 
            rules=F)

t2= C5.0(class ~ ., 
         data = train[,c(1:7,58)], 
         rules=F)

t3= C5.0(class ~ ., 
         data = train[,c(1:9,58)], 
         rules=F)

t4= C5.0(class ~ ., 
         data = train[,c(1:10,58)], 
         rules=F)

t5= C5.0(class ~ ., 
         data = train[,c(1:11,58)], 
         rules=F)


t6= C5.0(class ~ ., 
         data = train[,c(1:13,58)], 
         rules=F)

t6= C5.0(class ~ ., 
         data = train[,c(1:15,58)], 
         rules=F)


t7= C5.0(class ~ ., 
         data = train[,c(1:17,58)], 
         rules=F)

t8= C5.0(class ~ ., 
         data = train[,c(1:19,58)], 
         rules=F)

t9= C5.0(class ~ ., 
         data = train[,c(1:21,58)], 
         rules=F)

t10= C5.0(class ~ ., 
         data = train[,c(1:22,58)], 
         rules=F)

t11= C5.0(class ~ ., 
         data = train[,c(1:23,58)], 
         rules=F)

t12= C5.0(class ~ ., 
          data = train[,c(1:25,58)], 
          rules=F)

t13= C5.0(class ~ ., 
          data = train[,c(1:27,58)], 
          rules=F)

t14= C5.0(class ~ ., 
          data = train[,c(1:29,58)], 
          rules=F)

t15= C5.0(class ~ ., 
          data = train[,c(1:31,58)], 
          rules=F)


t16= C5.0(class ~ ., 
          data = train[,c(1:33,58)], 
          rules=F)

t17= C5.0(class ~ ., 
          data = train[,c(1:35,58)], 
          rules=F)

t18= C5.0(class ~ ., 
          data = train[,c(1:37,58)], 
          rules=F)

t19= C5.0(class ~ ., 
          data = train[,c(1:39,58)], 
          rules=F)

t20= C5.0(class ~ ., 
          data = train[,c(1:41,58)], 
          rules=F)

t21= C5.0(class ~ ., 
          data = train[,c(1:42,58)], 
          rules=F)



m1=table(train$class, 
         predict(t1, 
                 newdata=train, 
                 type="class"))

m2=table(train$class, 
         predict(t2, 
                 newdata=train, 
                 type="class"))
m3=table(train$class, 
         predict(t3, 
                 newdata=train, 
                 type="class"))
m4=table(train$class, 
         predict(t4, 
                 newdata=train, 
                 type="class"))
m5=table(train$class, 
         predict(t5, 
                 newdata=train, 
                 type="class"))
m6=table(train$class, 
         predict(t6, 
                 newdata=train, 
                 type="class"))
m7=table(train$class, 
         predict(t7, 
                 newdata=train, 
                 type="class"))
m8=table(train$class, 
         predict(t8, 
                 newdata=train, 
                 type="class"))
m9=table(train$class, 
         predict(t9, 
                 newdata=train, 
                 type="class"))
m10=table(train$class, 
         predict(t10, 
                 newdata=train, 
                 type="class"))
m11=table(train$class, 
         predict(t11, 
                 newdata=train, 
                 type="class"))
m12=table(train$class, 
         predict(t12, 
                 newdata=train, 
                 type="class"))

m13=table(train$class, 
         predict(t13, 
                 newdata=train, 
                 type="class"))
m14=table(train$class, 
         predict(t14, 
                 newdata=train, 
                 type="class"))
m15=table(train$class, 
         predict(t15, 
                 newdata=train, 
                 type="class"))
m16=table(train$class, 
         predict(t16, 
                 newdata=train, 
                 type="class"))
m17=table(train$class, 
         predict(t17, 
                 newdata=train, 
                 type="class"))
m18=table(train$class, 
         predict(t18, 
                 newdata=train, 
                 type="class"))
m19=table(train$class, 
          predict(t19, 
                  newdata=train, 
                  type="class"))
m20=table(train$class, 
          predict(t20, 
                  newdata=train, 
                  type="class"))
m21=table(train$class, 
          predict(t21, 
                  newdata=train, 
                  type="class"))





ptrain <- c()
ptrain[1] =(m1[2,1])/(m1[2,1]+m1[1,1])*100
ptrain[2]=(m2[2,1])/(m2[2,1]+m3[1,1])*100
ptrain[3] =(m3[2,1])/(m3[2,1]+m3[1,1])*100
ptrain[4]=(m4[2,1])/(m4[2,1]+m4[1,1])*100
ptrain[5] =(m5[2,1])/(m5[2,1]+m5[1,1])*100
ptrain[6]=(m6[2,1])/(m6[2,1]+m6[1,1])*100
ptrain[7] =(m7[2,1])/(m7[2,1]+m7[1,1])*100
ptrain[8]=(m8[2,1])/(m8[2,1]+m8[1,1])*100
ptrain[9] =(m9[2,1])/(m9[2,1]+m9[1,1])*100
ptrain[10]=(m10[2,1])/(m10[2,1]+m10[1,1])*100
ptrain[11] =(m11[2,1])/(m11[2,1]+m11[1,1])*100
ptrain[12]=(m12[2,1])/(m12[2,1]+m12[1,1])*100
ptrain[13]=(m13[2,1])/(m13[2,1]+m13[1,1])*100
ptrain[14]=(m4[2,1])/(m14[2,1]+m14[1,1])*100
ptrain[15] =(m5[2,1])/(m15[2,1]+m15[1,1])*100
ptrain[16]=(m6[2,1])/(m16[2,1]+m16[1,1])*100
ptrain[17] =(m7[2,1])/(m17[2,1]+m17[1,1])*100
ptrain[18]=(m8[2,1])/(m18[2,1]+m18[1,1])*100
ptrain[19] =(m9[2,1])/(m19[2,1]+m19[1,1])*100
ptrain[20]=(m10[2,1])/(m20[2,1]+m20[1,1])*100
ptrain[21] =(m11[2,1])/(m21[2,1]+m21[1,1])*100
ptrain

atrain<- c()
atrain[1] =(m1[2,2]+m1[1,1])/length(train$class)
atrain[2] =(m2[2,2]+m2[1,1])/length(train$class)
atrain[3] =(m3[2,2]+m3[1,1])/length(train$class)
atrain[4] =(m4[2,2]+m4[1,1])/length(train$class)
atrain[5] =(m5[2,2]+m5[1,1])/length(train$class)
atrain[6] =(m6[2,2]+m6[1,1])/length(train$class)
atrain[7] =(m7[2,2]+m7[1,1])/length(train$class)
atrain[8] =(m8[2,2]+m8[1,1])/length(train$class)
atrain[9] =(m9[2,2]+m9[1,1])/length(train$class)
atrain[10] =(m10[2,2]+m10[1,1])/length(train$class)
atrain[11] =(m11[2,2]+m11[1,1])/length(train$class)
atrain[12] =(m12[2,2]+m12[1,1])/length(train$class)
atrain[13] =(m13[2,2]+m13[1,1])/length(train$class)
atrain[14] =(m14[2,2]+m14[1,1])/length(train$class)
atrain[15] =(m15[2,2]+m15[1,1])/length(train$class)
atrain[16] =(m16[2,2]+m16[1,1])/length(train$class)
atrain[17] =(m17[2,2]+m17[1,1])/length(train$class)
atrain[18] =(m18[2,2]+m18[1,1])/length(train$class)
atrain[19] =(m19[2,2]+m19[1,1])/length(train$class)
atrain[20] =(m20[2,2]+m20[1,1])/length(train$class)
atrain[21] =(m21[2,2]+m21[1,1])/length(train$class)


###############################################################################################
tt1= C5.0(class ~ ., 
          data = test[,c(1:5,58)], 
          rules=F)

tt2= C5.0(class ~ ., 
          data = test[,c(1:7,58)], 
          rules=F)

tt3= C5.0(class ~ ., 
          data = test[,c(1:9,58)], 
          rules=F)

tt4= C5.0(class ~ ., 
          data = test[,c(1:10,58)], 
          rules=F)

tt5= C5.0(class ~ ., 
          data = test[,c(1:11,58)], 
          rules=F)


tt6= C5.0(class ~ ., 
          data = test[,c(1:13,58)], 
          rules=F)

tt7= C5.0(class ~ ., 
          data = test[,c(1:15,58)], 
          rules=F)


tt8= C5.0(class ~ ., 
          data = test[,c(1:17,58)], 
          rules=F)

tt9= C5.0(class ~ ., 
          data = test[,c(1:19,58)], 
          rules=F)

tt10= C5.0(class ~ ., 
           data = test[,c(1:21,58)], 
           rules=F)

tt11= C5.0(class ~ ., 
           data = test[,c(1:22,58)], 
           rules=F)

tt12= C5.0(class ~ ., 
           data = test[,c(1:23,58)], 
           rules=F)

tt13= C5.0(class ~ ., 
           data = test[,c(1:25,58)], 
           rules=F)

tt14= C5.0(class ~ ., 
           data = test[,c(1:27,58)], 
           rules=F)

tt15= C5.0(class ~ ., 
           data = test[,c(1:29,58)], 
           rules=F)

tt16= C5.0(class ~ ., 
           data = test[,c(1:31,58)], 
           rules=F)


tt17= C5.0(class ~ ., 
           data = test[,c(1:33,58)], 
           rules=F)

tt18= C5.0(class ~ ., 
           data = test[,c(1:35,58)], 
           rules=F)

tt19= C5.0(class ~ ., 
           data = test[,c(1:37,58)], 
           rules=F)

tt20= C5.0(class ~ ., 
           data = test[,c(1:39,58)], 
           rules=F)

tt21= C5.0(class ~ ., 
           data = test[,c(1:41,58)], 
           rules=F)

tt22= C5.0(class ~ ., 
           data = test[,c(1:42,58)], 
           rules=F)



mt1=table(test$class, 
          predict(tt1, 
                  newdata=test, 
                  type="class"))

mt2=table(test$class, 
          predict(tt2, 
                  newdata=test, 
                  type="class"))
mt3=table(test$class, 
          predict(tt3, 
                  newdata=test, 
                  type="class"))
mt4=table(test$class, 
          predict(tt4, 
                  newdata=test, 
                  type="class"))
mt5=table(test$class, 
          predict(tt5, 
                  newdata=test, 
                  type="class"))
mt6=table(test$class, 
          predict(tt6, 
                  newdata=test, 
                  type="class"))
mt7=table(test$class, 
          predict(tt7, 
                  newdata=test, 
                  type="class"))
mt8=table(test$class, 
          predict(tt8, 
                  newdata=test, 
                  type="class"))
mt9=table(test$class, 
          predict(tt9, 
                  newdata=test, 
                  type="class"))
mt10=table(test$class, 
           predict(tt10, 
                   newdata=test, 
                   type="class"))
mt11=table(test$class, 
           predict(tt11, 
                   newdata=test, 
                   type="class"))
mt12=table(test$class, 
           predict(tt12, 
                   newdata=test, 
                   type="class"))

mt13=table(test$class, 
           predict(tt13, 
                   newdata=test, 
                   type="class"))
mt14=table(test$class, 
           predict(tt14, 
                   newdata=test, 
                   type="class"))
mt15=table(test$class, 
           predict(tt15, 
                   newdata=test, 
                   type="class"))
mt16=table(test$class, 
           predict(tt16, 
                   newdata=test, 
                   type="class"))
mt17=table(test$class, 
           predict(tt17, 
                   newdata=test, 
                   type="class"))
mt18=table(test$class, 
           predict(tt18, 
                   newdata=test, 
                   type="class"))
mt19=table(test$class, 
           predict(tt19, 
                   newdata=test, 
                   type="class"))
mt20=table(test$class, 
           predict(tt20, 
                   newdata=test, 
                   type="class"))
mt21=table(test$class, 
           predict(tt21, 
                   newdata=test, 
                   type="class"))

mt22=table(test$class, 
           predict(tt22, 
                   newdata=test, 
                   type="class"))




ptest <- c()
ptest[1] =(mt1[2,1])/(mt1[2,1]+mt1[2,2])*100
ptest[2]=(mt2[2,1])/(mt2[2,1]+mt3[2,2])*100
ptest[3] =(mt3[2,1])/(mt3[2,1]+mt3[2,2])*100
ptest[4]=(mt4[2,1])/(mt4[2,1]+mt4[2,2])*100
ptest[5] =(mt5[2,1])/(mt5[2,1]+mt5[2,2])*100
ptest[6]=(mt6[2,1])/(mt6[2,1]+mt6[2,2])*100
ptest[7] =(mt7[2,1])/(mt7[2,1]+mt7[2,2])*100
ptest[8]=(mt8[2,1])/(mt8[2,1]+mt8[2,2])*100
ptest[9] =(mt9[2,1])/(mt9[2,1]+mt9[2,2])*100
ptest[10]=(mt10[2,1])/(mt10[2,1]+mt10[2,2])*100
ptest[11] =(mt11[2,1])/(mt11[2,1]+mt11[2,2])*100
ptest[12]=(mt12[2,1])/(mt12[2,1]+mt12[2,2])*100
ptest[13]=(mt13[2,1])/(mt13[2,1]+mt13[2,2])*100
ptest[14]=(mt4[2,1])/(mt14[2,1]+mt14[2,2])*100
ptest[15] =(mt5[2,1])/(mt15[2,1]+mt15[2,2])*100
ptest[16]=(mt6[2,1])/(mt16[2,1]+mt16[2,2])*100
ptest[17] =(mt7[2,1])/(mt17[2,1]+mt17[2,2])*100
ptest[18]=(mt8[2,1])/(mt18[2,1]+mt18[2,2])*100
ptest[19] =(mt9[2,1])/(mt19[2,1]+mt19[2,2])*100
ptest[20]=(mt10[2,1])/(mt20[2,1]+mt20[2,2])*100
ptest[21] =(mt11[2,1])/(mt21[2,1]+mt21[2,2])*100

ptest

atest
atest<- c()
atest[1] =(mt1[2,2]+mt1[1,1])/length(test$class)
atest[2] =(mt2[2,2]+mt2[1,1])/length(test$class)
atest[3] =(mt3[2,2]+mt3[1,1])/length(test$class)
atest[4] =(mt4[2,2]+mt4[1,1])/length(test$class)
atest[5] =(mt5[2,2]+mt5[1,1])/length(test$class)
atest[6] =(mt6[2,2]+mt6[1,1])/length(test$class)
atest[7] =(mt7[2,2]+mt7[1,1])/length(test$class)
atest[8] =(mt8[2,2]+mt8[1,1])/length(test$class)
atest[9] =(mt9[2,2]+mt9[1,1])/length(test$class)
atest[10] =(mt10[2,2]+mt10[1,1])/length(test$class)
atest[11] =(mt11[2,2]+mt11[1,1])/length(test$class)
atest[12] =(mt12[2,2]+mt12[1,1])/length(test$class)
atest[13] =(mt13[2,2]+mt13[1,1])/length(test$class)
atest[14] =(mt14[2,2]+mt14[1,1])/length(test$class)
atest[15] =(mt15[2,2]+mt15[1,1])/length(test$class)
atest[16] =(mt16[2,2]+mt16[1,1])/length(test$class)
atest[17] =(mt17[2,2]+mt17[1,1])/length(test$class)
atest[18] =(mt18[2,2]+mt18[1,1])/length(test$class)
atest[19] =(mt19[2,2]+mt19[1,1])/length(test$class)
atest[20] =(mt20[2,2]+mt20[1,1])/length(test$class)
atest[21] =(mt21[2,2]+mt21[1,1])/length(test$class)

