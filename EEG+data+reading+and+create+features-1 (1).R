#install.packages("R.matlab")
require(R.matlab)
require(stats)


bands <- c(0.1,2,4,6,8,10,12,18,30,50,70,125,180)
#bands <- c(0.1,4,8,12,30,70,180)
band.centers = (bands[1:(length(bands)-1)] + bands[2:length(bands)])/2
band.widths = bands[2:length(bands)] - bands[1:(length(bands)-1)]
bands.num = length(band.centers)
sensor.Idx = 1
sensors.num = 16

## data.method determine how the data will be acquired
# 0 for reading matlab files and computing pd
# 1 for reading an R file with pd computed
data.method = 1

if (data.method == 0){
  directory = "C:/Users/William/Documents/Kaggle/EEG Seizure Prediction/Data/Dog_1"
  files <- list.files(directory, pattern="*.mat", recursive="True")
  files <- paste(directory,"/",files, sep="")
  files.num = length(files)
  fileIdx = 0
  #pd <- array(0, dim=c(files.num,sensors.num*bands.num))
  
  for (fname in files) {
    print(fname)
    if (grepl("inter", fname)) print("inter")
    if (grepl("pre", fname)) print("pre")
    if (grepl("test", fname)) print("test")
    fileIdx = fileIdx + 1
    if (min(pd[fileIdx,]) == 0) {
    
    for (sensor.Idx in 1:16) {
      
      # Read and organize data
      EEG <- readMat(fname)
      EEG.reading <- as.data.frame(t(EEG[[1]][[1]]))
      data.length.sec <- EEG[[1]][[2]]
      sampling.frequency <- EEG[[1]][[3]]
      
      # Compute the change in time (dt) and the sequence of time values (t)
      dt <- 1/sampling.frequency
      t <- seq(0,data.length.sec, by=dt) 
      
      start.Idx = 1
      end.Idx = 2^(floor(log2(length(EEG.reading[,1]))))#5000
      Idx.Vector = start.Idx:end.Idx
      
      # Compute the piece of the EEG reading for computation of the frequencies
      t.segment <- t[Idx.Vector]
      EEG.segment <- EEG.reading[Idx.Vector,sensor.Idx]
      T.segment = t[end.Idx]-t[start.Idx]
      plot(t.segment, EEG.segment, type="l", col=sensor.Idx)
        
      # Create a frequency array
      f <- 1:length(t.segment)/T.segment
      
      # Compute FFT
      Y <- fft(EEG.segment)
      mag <- sqrt(Re(Y)^2+Im(Y)^2)*2/length(EEG.segment)
      phase <- atan(Im(Y)/Re(Y))
      Yr <- Re(Y)
      Yi <- Im(Y)
      
      # Plot the EEG reading and the frequencies
      layout(matrix(c(1,2,3), 3, 1, byrow = TRUE))
      plot(t.segment,EEG.segment,type="l",xlim=c(t[start.Idx],t[end.Idx]))
      plot(f[1:length(EEG.segment)/2],mag[1:length(EEG.segment)/2],type="l")
      
      for (bandIdx in 1:bands.num ) {
        pd[fileIdx, (sensor.Idx-1)*bands.num + bandIdx] =  sum( mag * (bands[bandIdx]<f) * (f<bands[bandIdx+1]) ) / sum(mag)
      }
      # plot(band.centers, pd[fileIdx, sensor.Idx, ]*band.widths, xlim=c(f[1],f[length(EEG.segment)/2]), type="l", col=sensor.Idx)
      #plot(pd[fileIdx-1, sensor.Idx, ], type="l", col=sensor.Idx)
      print(sensor.Idx)
      
      
      for (sensor.plot.Idx in 1:sensor.Idx ) {
        if (sensor.plot.Idx == 1) {
          plot(band.centers, pd[fileIdx, (sensor.plot.Idx-1)*bands.num+1:bands.num], xlim=c(f[1],f[length(EEG.segment)/2]), type="l", col=(sensor.plot.Idx+1))
        } else {
          lines(band.centers, pd[fileIdx, (sensor.plot.Idx-1)*bands.num+1:bands.num], xlim=c(f[1],f[length(EEG.segment)/2]), type="l", col=(sensor.plot.Idx+1))
        }
      }
      
    }
    }
  }
  
  
  dog1 <- data.frame(pd)
  dog1$class <- class
  str(dog1)
} else {
  load("dog_1_pd")
}

i = subset(dog1, class == "inter")
p = subset(dog1, class == "pre")
t = subset(dog1, class == "test")

#Scatterplot the datta, coloring by class.
color = class
color[(class == "pre")] = "red"
color[class == "inter"] = "blue"
color[class == "test"] = "green"

for (j in 1:(sensors.num*bands.num-1)) {
  plot(pd[1:504,j],pd[1:504,j+1],pch=21,bg=color[1:504])
}



# Pricipal Components Analysis
# entering raw data and extracting PCs
# from the correlation matrix
fit <- princomp(pd, cor=TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
biplot(fit)

# plot with the data colored by class
plot(fit$scores[1:504,1:2],pch=21,bg=color[1:504])

# Spinning 3d Scatterplot
library(rgl)
plot3d(fit$scores[,1:3],pch=21,bg=color)


###############
# Compute LDA #
###############

# Compute the covariance of each species data.
Ci = cov(i[1:192])
#Cp = cov(p[1:192])
# !Assuming both have same covariance, until have enough p points to compute a covariance!#
Cp = Ci

# Compute the mean of each species data
im = colMeans(i[,1:192])
pm = colMeans(p[,1:192])
# Compute the number of elements in each class
numi = length(i[,1])
nump = length(p[,1])
# Compute the within class scatter
Sw = nump*Cp +numi*Ci
# Compute the between class scatter
m = (nump*pm + numi*im)/(nump+numi)# mean of all the data
Sb = numi*(im-m)%*%t(im-m) + nump*(pm-m)%*%t(pm-m)
S = solve(Sw)%*%Sb
E<- eigen(S)
LDAvectors = Re(E$vectors[,c(1,2)])
A = pd[,1:192]
LDA <- as.matrix(A)%*%LDAvectors
# plot in LDA-space, colored by class
plot(LDA[1:504,],pch=21,bg=color[1:504])
LDA_detection = 1 - LDA[,1]
plot(LDA_detection[1:504], type="l", col="blue")
points(LDA_detection[1:504], type="p", col=color)
title("Detection results using LDA")


# We can plot on the 2nd and 3rd eigenvectors of the inv(Sw)Sb matrix, but there is no information
# LDAvectors = Re(E$vectors)
# A = pd[,1:192]
# LDA <- as.matrix(A)%*%LDAvectors[,2:3]
# plot in LDA-space, colored by class
# plot(LDA,pch=21,bg=color)



# Compute the Mahalanobis distance from each class, for all data.
Mahali=c()
Mahalp=c()
for (j in 1:504){
  Mahali[j] = mahalanobis(pd[j,1:192], im, Ci)
}
for (j in 1:504){
  Mahalp[j] = mahalanobis(pd[j,1:192], pm, Cp)
}

# Plot the Mahalanobis distance for each class
plot(Mahali, type="l", col="red")
lines(Mahalp, type="l", col="green")
color[Mahali-Mahalp < 0] = "red"
color[Mahali-Mahalp > 0] = "green"
plot(Mahali-Mahalp, type="l", col="blue")
points(Mahali-Mahalp, type="p", col=color)
title("Detection results using GMLC")

# Computing Matched Filter and ACE
ICov = solve(Ci)
ACE=c()# Correlation to target in whitened space
MF=c()#Regression against target in whitened space
t = pm
#t = pd[500,1:192]# if we wnat to try detection against one data values instead of the mean
for (j in 1:504){
  ACE[j] = ((t-im)%*%ICov%*%(pd[j,1:192]-im)) / (((t-im)%*%ICov%*%(t-im) * (pd[j,1:192]-im)%*%ICov%*%(pd[j,1:192]-im))^0.5)
  MF[j] = ((t-im)%*%ICov%*%(pd[j,1:192]-im)) / ((t-im)%*%ICov%*%(t-im))
}
plot(ACE, type="l", col="blue")
points(ACE, type="p", col=color)
title("Detection results using ACE (whitened space correlation)")
plot(MF, type="l", col="blue")
points(MF, type="p", col=color)
title("Detection results using GMLC (whitened space single variable regression)")

# Compute the whitening transformation
E = eigen(Ci)
P = E$vectors
D.inv.sqr = diag(as.array(E$values)^(-1/2))
W = t(P%*%D.inv.sqr)
PD = pd
for (j in 1:504){
  PD[j,] = W%*%(pd[j,]-im)
}


likelihood=array(0, dim=c(504,2))
probability=array(0, dim=c(504,2))
Tidx = 481:504
Bidx = 1:24*20
for (j in 1:504){
    Tidx_temp =  Tidx[! Tidx %in% j]
    Bidx_temp =  Bidx[! Bidx %in% j]
    Y = PD[j,]
    
    #compute target likelihood using the best model with preictal EEG
    X = PD[Tidx_temp,]
    c = solve(X%*%t(X))%*%(X%*%Y)
    model = t(X)%*%c
    error = sum((Y - model)^2)
    if (error > 0) {
      likelihood[j,1] = (error)^(-3/2)* (3)^(-dim(X)[1]/2)
    }
    
    #compute background likelihood using the best model with interictal EEG
    X = PD[Bidx_temp,]
    c = solve(X%*%t(X))%*%(X%*%Y)
    model = t(X)%*%c
    error = sum((Y - model)^2)
    if (error > 0) {
      likelihood[j,2] = (error)^(-3/2) * (3)^(-dim(X)[1]/2)
    }
    probability[j,] = likelihood[j,]/sum(likelihood[j,])
}
plot(probability[,1], type="l", col="blue")
threshold = max(probability[1:480]) 
color[probability > threshold] = "green"
color[probability <= threshold] = "red"
plot(probability[,1], type="l", col="blue")
points(probability[,1], col=color)
title("Probability")

