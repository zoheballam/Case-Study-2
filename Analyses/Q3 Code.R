###Question 3###
data(Orange)
Orange

#a
mean <- tapply(Orange$circumference,Orange$Tree, mean,  na.rm=TRUE) 
median <- tapply(Orange$circumference,Orange$Tree, median,  na.rm=TRUE)
cbind(mean, median)

#b
library (ggplot2)
qplot(circumference, age, data=Orange,color=Tree, 
      xlab="Circumference", ylab="Age") 

#c
qplot(factor(Tree), circumference, data = Orange, geom = "boxplot")
