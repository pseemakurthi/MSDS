PD <- function(D, targetIndex, NonTargetIndex, T){
  # returns PD for a given detector and indices and threshold
  numTargets = length(targetIndex) # determine the number of targets
  return(sum(D[targetIndex] >= T)/numTargets) # compute and return the PD
}



PFA <- function(D, targetIndex, NonTargetIndex, T){
  # returns average PD for a given detector and indices and threshold
  numNontargets = length(NonTargetIndex) # determine the number of nonTargets
  return(sum(D[NonTargetIndex] >= T)/numNontargets) # compute and return the PFA
}



AFAR <- function(D, targetIndex, NonTargetIndex){
  # returns average false alarm rate (AFAR) for a given detector and indices

  numTargets = length(targetIndex) # determine the number of targets
  numNontargets = length(NonTargetIndex) # determine the number of nonTargets
  FAR = 0
  for (i in targetIndex){
    # compute the False Alarm Rate (aka PFA) for the 
    # threshold set to the detector value of the ith targets, 
    # and add this to our sum of the FARs
    FAR = FAR + sum(D[NonTargetIndex] >= D[i])/numNontargets
  }
  AFAR = FAR/numTargets # divide by the number of targets to get the average FAR
  
  return(AFAR)
}





ROC <- function(D, targetIndex, NonTargetIndex){
  # returns average false alarm rate (AFAR) for a given detector and indices
  # and plots the ROC curve
  numTargets = length(targetIndex)
  numNontargets = length(NonTargetIndex)
  FAR = c()
  PD = c()
  index = 1
  for (i in targetIndex){
    FAR[index] = sum(D[NonTargetIndex] >= D[i])/numNontargets
    PD[index] = sum(D[targetIndex] >= D[i])/numTargets
    index = index + 1
  }
  AFAR = sum(FAR)/numTargets
#  plot(FAR,PD)
    
  s = sort.int(D[targetIndex], index.return=TRUE)# sort by detection score
  name <- deparse(substitute(D))# Determine the name of the detector algorithm
  plot(FAR[s$ix],PD[s$ix], type="l", col="blue",
       xlab="False Alarm Rate", ylab="Probability of Detection",
       main=paste("ROC curve for ",name), 
       sub=paste("AFAR = ",AFAR),
       xlim=c(0,1), ylim=c(0,1))
   df <- data.frame(x= FAR[s$ix], y= PD[s$ix] )
   return(df)
}





ROC_optimal <- function(D, targetIndex, NonTargetIndex, FA_cost, MT_cost){
  # returns average false alarm rate (AFAR) for a given detector and indices
  # and plots the ROC curve
  numTargets = length(targetIndex)
  numNontargets = length(NonTargetIndex)
  FAR = c()
  PD = c()
  Cost = c()
  index = 1
  for (i in targetIndex){
    FAR[index] = sum(D[NonTargetIndex] >= D[i])/numNontargets
    PD[index] = sum(D[targetIndex] >= D[i])/numTargets
    index = index + 1
  
    Cost[index] = FA_cost*sum(D[NonTargetIndex] >= D[i]) + MT_cost*sum(D[targetIndex] < D[i])
  
  }
  AFAR = sum(FAR)/numTargets
  #  plot(FAR,PD)
  
  s = sort.int(D[targetIndex], index.return=TRUE)# sort by detection score
  name <- deparse(substitute(D))# Determine the name of the detector algorithm
  plot(FAR[s$ix],PD[s$ix], type="l", col="blue",
       xlab="False Alarm Rate", ylab="Probability of Detection",
       main=paste("ROC curve for ",name), 
       sub=paste("AFAR = ",AFAR),
       xlim=c(0,1), ylim=c(0,1))

  
  #plot(D[s$ix],Cost[s$ix])  
  return(AFAR)
}


target <- c(481:504)
Nt <- c(1:480)

z <- ROC(LDA_detection,target,Nt)
y <- ROC(Mahali-Mahalp,target,Nt)
a <- ROC(ACE,target,Nt)
p <- ROC(probability[,1],target,Nt)
m <- ROC(MF,target,Nt)

plot(z$x, z$y, type="l",color= "red")
lines(y$x, y$y, lty="l")
plot(y$x,y$y, type = "l")

 xlim <- c(0,1)
 ylim <- c(0,1)
 plot(z$x, z$y, type="l", xlim=xlim, ylim=ylim)
 lines(y$x, z$y,col= "red")
 lines(a$x,a$y,col="blue")
 lines(p$x,p$y,col="green")
 lines(m$x,m$y,col="steelblue")

