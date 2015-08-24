
main <- rm(list=ls(all=T))
# Homework 
install.packages("jpeg")
install.packages("adimpro")
library(jpeg)
library(adimpro)
# Q-1 Importing File lincoln.jpg 
setwd("H:/Stat 6021/Hw5")
linc<-readJPEG("lincoln.jpg", native = FALSE)



# Q-2 Code to flip the image upside down and exporting it to hardd --------



# Building a matrix from 3d array
#Though the formual converts to grey scale if we ignore the numbers image is distorted.
gs <- 0.21*linc[,,1] + 0.72*linc[,,2] + 0.07*linc[,,3]
dim(gs)

## Flip vertically
linc.flip <- apply(gs, 2, rev)

writeJPEG(linc.flip,target="linc.flip.jpg",quality=1)


# Q-3 No. of  PC's, so that image not very much distorted ----------------



# Reduicing size using PC


#Computing PC

pc <- princomp(linc.flip)

plot(pc, type="lines",pch=20,cex=2)

# Eigenvalues

pc$sdev[1:150]^2
sum(pc$sdev[1:150]^2)/(sum(pc$sdev^2))

# A lot of the variance is found in just the
# first 50 PC's.  Let's reduce to those, and 
# see what we get.

pc.ct<-200  # This sets the number of PC's used
gs<-t(pc$loadings[,1:pc.ct]%*%t(pc$scores[,1:pc.ct]))
c<-matrix(rep(colMeans(linc.flip),1761),1761,byrow=T)
gs <- t(gs+c)




# Now let's write this to a JPEG and review.
writeJPEG(t(gs[1:nrow(gs),]),target="linc170.jpg",quality=1.0)


# 4. On a document, put the number of PC's
#    from 3., along with flipped image from 2. and the 
#    compressed image corresponding to 3.

# Please refer to the attached PDF
 

