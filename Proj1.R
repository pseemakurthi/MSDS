  main <- rm(list=ls(all=T))
  setwd("H:/DS6001")
  install.packages("ggplot2")
  install.packages("xlsx")
  library(ggplot2)
  library(xlsx)
  #reading data from disk
  dat <- read.xlsx(file = "HW.xlsx" , 1)
  test <- read.csv(file = "leaf.csv",skip = 31 )
  
  
  #renaming the cols
  names(dat) <- c("Sno","LengthA","WidthA","LengthB","WidthB")
  names(test) <- c("Sno","LengthA","WidthA","LengthB","WidthB")
  
  #subsetting dataleaf
  leaf <- dat[1:30,2:5]
  
  leaf$ratioA <- leaf$LengthA/leaf$WidthA 
  
  leaf$ratioB <- leaf$LengthB/leaf$WidthB
  
  
  meanLengthA <- mean(leaf$LengthA)
  meanWidthA  <- mean(leaf$WidthA)
  
  sdLengthA <- sd(leaf$LengthA)
  sdWidthA <- sd(leaf$WidthA)
  
  
  
  meanLengthB <- mean(leaf$LengthB)
  meanWidthB  <- mean(leaf$WidthB)
  
  sdLengthB <- sd(leaf$LengthB)
  sdWidthB <- sd(leaf$WidthB)

summary(leaf)

##plotting graphs 
attach(leaf)
plot(x=leaf$LengthA, y=leaf$WidthA,xlab="Length ", ylab="width ", pch=19,
                                        xlim=c(8,15),ylim=c(3,10))
par(new=TRUE)   
plot(x=leaf$LengthB, y=leaf$WidthB,col = "red",xlab="Length ", ylab="width ", pch=19,xlim=c(8,15),ylim=c(3,10))

attach(leaf)
hist(leaf$LengthA,breaks =20,freq = T,col=rgb(1,0,0,0.5),main="Overlapping lengths",xlab = "Lengths")
hist(leaf$LengthB, col=rgb(0,0,1,0.5),breaks =20,freq = T,add=T)

attach(leaf)
hist(leaf$WidthA,freq = F,col="yellow",main="Overlapping breadth",xlab = "Breadths",breaks = 12, xlim=c(2,6))
hist(leaf$WidthB*0.8, col="green",breaks =18,freq = F,add=T)

ggplot(leaf,aes(LengthA,WidthA))+geom_boxplot()+
  geom_point(aes(color=ratioA),position=position_dodge(width=0.5)) +ggtitle("Leaf A")

ggplot(leaf,aes(LengthB,WidthB))+geom_boxplot()+
  geom_point(aes(color=ratioB),position=position_dodge(width=0.5))+ggtitle("Leaf B")

   
###GMLC
  
  gmlc <- function(m1,s1,m2,s2, vec){
      class <- vector()
      class <- ifelse(((m1-vec)/s1 - (m2-vec)/s2) >0 , "b","a")
    
  }
  
lma <- mean(leaf$LengthA)
lmb <- mean(leaf$LengthB)
csa <- cov(leaf[,1:2])
csb <- cov(leaf[,3:4])  
  
  MahalA=c()
  MahalB=c()
  MahalVi=c()
   len1 <- test[,1]
   len2 <- test[,3]  
    <- cbind(len1,len2)
  for (i in 1:length(test[,1])){
    Mahala[i] = mahalanobis(iris[i,1:4], Sm, CS)
  }
  for (i in 1:length(iris[,1])){
    MahalV[i] = mahalanobis(iris[i,1:4], Vm, CV)
  }
  
