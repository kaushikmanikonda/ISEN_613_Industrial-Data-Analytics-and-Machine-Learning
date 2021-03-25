#Script Title: ISEN_613_Assignment-06_Script File_Manikonda Kaushik
#Script Purpose: To satisfy the requirements for ISEN 613, Homework-06

#displays any existing objects in the workspace
ls()

#Step to check which liabraries are loaded
search()

#Removes all existing objects and variable assignments. 
#Will be commented out in the final homework to avoid repetition during future executions
rm(list=ls())
ls()

#output is directed to a separate file while still appearing in the console. 
#Includes the full path to show where the output file is being written.
sink("C:\\Users\\rajak\\Documents\\Fall_2020\\ISEN_613 ENGR Data Analysis\\Homeworks\\Homework_06/HW06_Output File",split=TRUE)


#This command directs all graphic output to a pdf file
pdf("C:\\Users\\rajak\\Documents\\Fall_2020\\ISEN_613 ENGR Data Analysis\\Homeworks\\Homework_06/HW06_Output_File.pdf")

#Problem 1 


#In the lab, a classification tree was applied to the Carseats data set after converting Sales into 
#a binary response variable. This question will seek to predict Sales using regression trees and 
#related approaches, treating the response as a quantitative variable (that is, without the conversion).

install.packages("tree")
library (ISLR)
library (tree)
attach(Carseats)

#(a) Split the data set into a training set and a test set.

set.seed(2)
train=sample(1: nrow(Carseats), 200)
Carseats.train=Carseats[train,]	#Training dataset
Carseats.test=Carseats[-train,]	#Test dataset

#(b) Fit a regression tree to the training set. Plot the tree, and interpret the results. Then 
#compute the test MSE.

tree.carseats=tree(Sales~.,Carseats,subset=train)
summary(tree.carseats) 	#What are in the outputs?
plot(tree.carseats) 	#display tree structure
text(tree.carseats,pretty=0)
title("Question_1b Unpruned Tree")

#From the regression tree, price and shelveloc condition are the most important factors for
#predicting sales. Price and Shelveloc were the first and second internal nodes respectively
#of the unpruned regression tree. Age, Population, and Advertising form the next three internal
#nodes in the order of importance.

tree.pred=predict(tree.carseats,Carseats.test)

#Calculating the Mean Squared Error of the predicted Sales (Test MSE)
install.packages("Metrics")
library(Metrics)
mean((tree.pred-Carseats.test$Sales)^2)
mse(tree.pred,Carseats.test$Sales)


#(c) Prune the tree obtained in (b). Use cross validation to determine the optimal level of 
#tree complexity. Plot the pruned tree and interpret the results. Compute the test MSE of 
#the pruned tree. Does pruning improve the test error?

set.seed(3)
cv.carseats=cv.tree(tree.carseats)
names(cv.carseats)
cv.carseats
plot(cv.carseats$size,cv.carseats$dev,main="Question_1C_CV Plot", type="b")

#So, from the graph, the optimal level of tree complexity is 10.

prune.carseats=prune.tree(tree.carseats,best=10)
plot(prune.carseats)
text(prune.carseats,pretty=0)

tree.pred=predict(prune.carseats,Carseats.test)
mse(tree.pred,Carseats.test$Sales)
title("Question_1C_Pruned Tree (Complexity = 10)")

#Repeating the process with 14 sublevels for reference
prune.carseats=prune.tree(tree.carseats,best=14)
plot(prune.carseats)
text(prune.carseats,pretty=0)
title("Question_1C_Pruned Tree (Complexity = 14)")

tree.pred=predict(prune.carseats,Carseats.test)
mse(tree.pred,Carseats.test$Sales)

# Test MSE = 4.794411, with optimal complexity = 10
# Test MSE = 4.471569, with optimal complexity = 14

#The test MSE is more for best=10, so best=14 was the right choice and no, the test MSE
#did not change for the pruned tree (best = 14). It did not change because the pruned tree 
#is the same as the original tree.



#(d) Use the bagging approach to analyze the data. What test MSE do you obtain? Determine which 
#variables are most important.

install.packages("randomForest")
library(randomForest)

set.seed(2)
train=sample(1: nrow(Carseats), 200)
Carseats_train<-Carseats[train,"Sales"]
Carseats.test=na.omit(Carseats[-train,"Sales"])

##bagging: randomforest with mtry=#Predictors
set.seed(2)
bag.carseats=randomForest(Sales~.,data=Carseats, subset=train,mtry=10, ntree=100, importance=TRUE)
bag.carseats

##calculate test MSE
yhat.bag <- predict(bag.carseats,newdata=Carseats[-train,])
mean((yhat.bag-Carseats.test)^2)

#Test MSE = 2.682507

importance(bag.carseats)	#Determines which variables are most important
#Price and Shelveloc, followed by Compprice are the most important variables.

##actual observations of test data and predictions
plot(yhat.bag,Carseats.test, pch=3,col="blue", main="Question_1D_Plot")
abline(0,1, col="red")



#(e) Use random forests to analyze the data. What test MSE do you obtain? Determine which 
#variables are most important.

set.seed(2)
rf.carseats <- randomForest(Sales~.,data=Carseats,subset = train, mtry=3,ntree=100,importance=TRUE)
yhat <- na.omit(predict(rf.carseats,newdata=Carseats[-train,]))
mean((yhat-Carseats.test)^2)

#Test MSE= 3.42203
importance(rf.carseats)		#Determines which variables are most important
#Price and Shelveloc, followed by Advertising, and Compprice are the most important variables.


graphics.off()