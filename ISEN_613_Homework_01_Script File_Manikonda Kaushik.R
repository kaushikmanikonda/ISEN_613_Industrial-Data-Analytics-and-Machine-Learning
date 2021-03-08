#displays any existing objects in the workspace
ls()

#Step to check which liabraries are loaded
search()

#Removes all existing objects and variable assignments. 
#Will be commented out in the final homework to avoid repetition during future executions
rm(list=ls())

#output is directed to a separate file while still appearing in the console. 
#Includes the full path to show where the output file is being written.
sink("C:/Users/Kaushik/Desktop/TAMU Semesters & Courses/Fall_2020/ISEN_613 ENGR Data Analysis/Homeworks/Homework_01/HW01 Output File",split=TRUE)

pdf("C:/Users/Kaushik/Desktop/TAMU Semesters & Courses/Fall_2020/ISEN_613 ENGR Data Analysis/Homeworks/Homework_01/HW01_Output_File.pdf")

#1. Data generation and matrix indexing 

#(1) Generate a vector with 25 elements and each element 
#independently follows a normal distribution (with mean =0 and sd=1);
a1<-rnorm(25,mean=0, sd=1)
a1

#(2) Reshape this vector into a 5 by 5 matrix in two ways (arranged by row and column);
a2<-matrix(a1,nrow=5,byrow=FALSE)
a2
a3<-matrix(a1,nrow=5,byrow=TRUE)
a3

#(3) Similarly, generate another vector with 100 elements and plot its histogram.
a4<-rnorm(100,mean=0, sd=1)
hist(a4,freq=TRUE)
hist(a4,freq=FALSE)

#(4) Explain the plots in your own words.
#First one is a histogram of frequencies of each element of the vector
#Second one is a histogram of densities for the vector elements
#Since they were generated using rnorm, the histogram forms a normal distribution.

#2. Upload the Auto data set, which is in the ISLR library. 
#Understand information about this data set by either ways 
#we introduced in class (like “?Auto” and names(Auto))
library(ISLR)
names(Auto)
?Auto

#3. Make a scatterplot between any two of the following variables 
#(try to plot all scatterplots in one figure; hint: use pairs() command): 
#“mpg”, “displacement”, “horsepower”, “weight”, “acceleration”. By observing 
#the plots, do you think the two variables in each scatterplot are correlated? If so, how?

attach(Auto)
plot(mpg,horsepower,col="blue",pch=4,ylab="Horsepower",xlab="Miles per Gallon")
plot(mpg,displacement, col="red",pch=2,ylab="Displacement",xlab="Miles per Gallon")
plot(mpg,weight, col="green",pch=3,ylab="Weight",xlab="Miles per Gallon")
plot(mpg,acceleration,col="magenta",pch=6,ylab="Acceleration",xlab="Miles per Gallon")

#4. Draw a line on the scatterplot of mpg vs. horsepower to represent relationship between 
#the two variables. (Hint: Search for “R Plot Line in Scatterplot)

plot(mpg,horsepower,col="blue",pch=4,ylab="Horsepower",xlab="Miles per Gallon")
abline(lm(horsepower~mpg), col="Red")
lm(horsepower~mpg,Auto)


#5. Is there a better way to represent their relationship rather than the linear model 
#you just drew? (No need to use mathematical formula. Just draw something on the figure)

#Yes, an exponential decline might be a better fit for this dataset than the linear
#model I drew.

graphics.off()
