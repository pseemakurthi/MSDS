install.packages("genalg")
library(genalg)
library(ggplot2)
dataset <- data.frame(item= c("pk","beans","potatoes","onions", "sleepingbag","rope",
                              "compass"), survivalpoints = c(10,20,15,2,30,10,30),
                      weight = c(1,5,10,1,7,5,1))
weightlimit <- 20
chromosome = c(1,0,0,1,0,0)
dataset[chromosome == 1,]


evalfunc <- function(x){
  current_solution_survivalpoints <- x %*% dataset$survivalpoints
  current_solutions_weight <-x %8% dataset$weight
  
  if(current_solution_weight  > weightlimit)
    return(0) else return (-current_solution_survivalpoints)
  
}