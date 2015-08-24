install.packages("openxlsx")
library(openxlsx)
library(devtools)
install_github("ggbiplot", "vqv")
library(ggplot2)
library(ggbiplot)
setwd("/media/prasad/Edu/Machine_learning/DataSet")
sortbytimespent <- read.xlsx("data.xlsx",sheet =2, startRow = 1, colNames = TRUE)

timespent <- sortbytimespent[,2:10]
str(timespent)
normalize <- function(x){
  (x-min(x))/(max(x)- min(x))
}
colnames(timespent)[4] <- "Total.time" 
colnames(timespent)[7] <- "Exit"
colnames(timespent)[8] <- "time.per"
colnames(timespent)[9] <- "cumulated.time"
#############################################################################
g1<-ggplot(timespent, aes(log(Pageviews))) + geom_histogram()                                       
g2<-ggplot(timespent, aes(log(Pageviews))) + geom_histogram(binwidth=1)                             
g3<-ggplot(timespent, aes(log(Pageviews))) + geom_histogram(fill=NA, color="black") + theme_bw()    

#density on y-axis
g4<-ggplot(timespent, aes(x=Pageviews)) + geom_histogram(aes(y = ..density..), color="black", fill=NA) + theme_bw()

grid.arrange(g1, g2, g3, g4, nrow=1)

h1<-ggplot(timespent, aes(log(Unique.Pageviews))) + geom_histogram()                                        
h2<-ggplot(timespent, aes(log(Unique.Pageviews))) + geom_histogram(binwidth=1)                             
h3<-ggplot(timespent, aes(log(Unique.Pageviews))) + geom_histogram(fill=NA, color="black") + theme_bw()    
h4<-ggplot(timespent, aes(x=Unique.Pageviews)) + geom_histogram(aes(y = ..density..), color="black", fill=NA) + theme_bw()
grid.arrange(h1, h2, h3, h4, nrow=1)

j1<-ggplot(timespent, aes(log(Avg..Time.on.Page)) + geom_histogram()                                        
j2<-ggplot(timespent, aes(log(Avg..Time.on.Page))) + geom_histogram(binwidth=1)                             
j3<-ggplot(timespent, aes(log(Avg..Time.on.Page))) + geom_histogram(fill=NA, color="black") + theme_bw()    
j4<-ggplot(timespent, aes(x=Avg..Time.on.Page)) + geom_histogram(aes(y = ..density..), color="black", fill=NA) + theme_bw()
grid.arrange(j1, j2, j3, j4, nrow=1)
           

k1<-ggplot(timespent, aes(log(Total.time))) + geom_histogram()                                       
k2<-ggplot(timespent, aes(log(Total.time))) + geom_histogram(binwidth=1)                            
k3<-ggplot(timespent, aes(log(Total.time))) + geom_histogram(fill=NA, color="black") + theme_bw()    
k4<-ggplot(timespent, aes(x=log(Total.time))) + geom_histogram(aes(y = ..density..), color="black", fill=NA) + theme_bw()
grid.arrange(k1, k2, k3, k4, nrow=1)

l1<-ggplot(timespent, aes(log(Entrances))) + geom_histogram()                                       
l2<-ggplot(timespent, aes(log(Entrances))) + geom_histogram(binwidth=1)                            
l3<-ggplot(timespent, aes(log(Entrances))) + geom_histogram(fill=NA, color="black") + theme_bw()    
l4<-ggplot(timespent, aes(x=log(Entrances))) + geom_histogram(aes(y = ..density..), color="black", fill=NA) + theme_bw()
grid.arrange(l1, l2, l3, l4, nrow=1)


m1<-ggplot(timespent, aes(Bounce.Rate)) + geom_histogram()                                       
m2<-ggplot(timespent, aes(Bounce.Rate)) + geom_histogram(binwidth=1)                            
m3<-ggplot(timespent, aes(Bounce.Rate)) + geom_histogram(fill=NA, color="black") + theme_bw()    
m4<-ggplot(timespent, aes(x=Bounce.Rate)) + geom_histogram(aes(y = ..density..), color="black", fill=NA) + theme_bw()
grid.arrange(m1, m2, m3, m4, nrow=1)

n1<-ggplot(timespent, aes(Exit)) + geom_histogram()                                       
n2<-ggplot(timespent, aes(Exit)) + geom_histogram(binwidth=1)                            
n3<-ggplot(timespent, aes(Exit)) + geom_histogram(fill=NA, color="black") + theme_bw()    
n4<-ggplot(timespent, aes(x=Exit)) + geom_histogram(aes(y = ..density..), color="black", fill=NA) + theme_bw()
grid.arrange(n1, n2, n3, n4, nrow=1)

o1<-ggplot(timespent, aes(log(time.per))) + geom_histogram()                                       
o2<-ggplot(timespent, aes(log(time.per))) + geom_histogram(binwidth=1)                            
o3<-ggplot(timespent, aes(log(time.per))) + geom_histogram(fill=NA, color="black") + theme_bw()    
o4<-ggplot(timespent, aes(x=log(time.per))) + geom_histogram(aes(y = ..density..), color="black", fill=NA) + theme_bw()
grid.arrange(o1, o2, o3, o4, nrow=1)

p1<-ggplot(timespent, aes(log(cumulated.time))) + geom_histogram()                                       
p2<-ggplot(timespent, aes(log(cumulated.time))) + geom_histogram(binwidth=1)                            
p3<-ggplot(timespent, aes(log(cumulated.time))) + geom_histogram(fill=NA, color="black") + theme_bw()    
p4<-ggplot(timespent, aes(x=log(cumulated.time))) + geom_histogram(aes(y = ..density..), color="black", fill=NA) + theme_bw()
grid.arrange(p1, p2, p3, p4, nrow=1)
           

#############################################################################

timespent_norm  <- data.frame(sapply(timespent,normalize))
summary(timespent)
pca <- prcomp(timespent,center = TRUE,scale= TRUE) 
print(pca)
plot(pca, type = "l") #take first 5 PCA 
summary(pca) # first 5 pca expalins 98.7% of the variance in the data.

g <- ggbiplot(pca, obs.scale = 1, var.scale = 1,  ellipse = TRUE,circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',legend.position = 'top')
print(g)

