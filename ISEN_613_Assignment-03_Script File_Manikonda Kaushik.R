#Script Title: ISEN_613_Assignment-03_Script File_Manikonda Kaushik
#Script Purpose: To satisfy the requirements for ISEN 613, Homework-03

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
sink("C:\\Users\\rajak\\Documents\\Fall_2020\\ISEN_613 ENGR Data Analysis\\Homeworks\\Homework_03/HW03_Output File",split=TRUE)


#This command directs all graphic output to a pdf file
pdf("C:\\Users\\rajak\\Documents\\Fall_2020\\ISEN_613 ENGR Data Analysis\\Homeworks\\Homework_03/HW03_Output_File.pdf")

#Problem 1 
#This question should be answered using the Weekly data set, which is part of the ISLR package. 
#This data is similar in nature to the Smarket data from this chapter’s lab, except that it contains 
#1,089 weekly returns for 21 years, from the beginning of 1990 to the end of 2010.

library(ISLR)	#Loads the ISLR library
library(MASS)	#Loads the MASS library
str(Weekly)		#Structure of the Weekly data set
attach(Weekly)

#(a) Produce some numerical and graphical summaries of the Weekly data. Do there appear to be any patterns?
summary(Weekly)
pairs(Weekly[,c(2:7,9)],pch=4, col="blue")

#(b) Use the full data set to perform a logistic regression with Direction as the response and the five lag 
#variables plus Volume as predictors. Use the summary function to print the results. Do any of the predictors 
#appear to be statistically significant? If so, which ones?

logistic.fit_1b=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly, family=binomial)
summary(logistic.fit_1b)
# No, none of the predictors appear to be statistically significant. Lag1, 3,4,5, & Volume
# all have high values suggesting the plausibility of null hypothesis(no relationship).
#Lag2 has a very low p-value (relatively), however, the standard error for lag2 is comparable
#to the actaual parameter itself. So, we have to conclude that even Lag2 is not statistically 
#significant.

#(c) Compute the confusion matrix and performance measures (accuracy, error rate, sensitivity, specificity). 
#Explain what the confusion matrix is telling you about the types of mistakes made by logistic regression. 
#Does the error rate represent the performance of logistic regression in prediction? 
#(hint: is it training error rate or test error rate?)

logistic.probs_1c=predict(logistic.fit_1b,type="response")
logistic.probs_1c[1:10]
dim(Weekly)[1]
logistic.pred=rep("Down",dim(Weekly)[1])
logistic.pred[logistic.probs_1c>0.5]="Up"

#Confusion Matrix
cmatrix_1c<-table(logistic.pred,Direction)
str(cmatrix_1c)
cmatrix_1c

Accuracy_1c<-(cmatrix_1c[1,1]+cmatrix_1c[2,2])/(sum(cmatrix_1c))
Accuracy_1c
Error_1c<-(cmatrix_1c[1,2]+cmatrix_1c[2,1])/(sum(cmatrix_1c))
Error_1c
Sensitivity_1c<-(cmatrix_1c[2,2])/(cmatrix_1c[1,2]+cmatrix_1c[2,2])
Sensitivity_1c
Specificity_1c<-(cmatrix_1c[1,1])/(cmatrix_1c[1,1]+cmatrix_1c[2,1])
Specificity_1c
#The accuracy of this logistic regression model is slightly more than random guessing (50%)
#However, this conclusion is misleading since we used the same training data for predictions
#as well. So, the error rate calculated here, (43.89%) is the training error rate.
#The confusion matrix is telling us that the model is very sensitive (92%) but is not very specific
#(11.2%). So, when the market goes down, this model will very likely not predict the down motion.


#(d) Now fit the logistic regression model using a training data period from 1990 to 2008, with Lag2 as the 
#only predictor. Compute the confusion matrix and performance measures (accuracy, error rate, sensitivity, 
#specificity) for the held out data (that is, the data from 2009 and 2010).

detach(Weekly)
Weekly_1d<-Weekly[Weekly$Year<=2008,]
str(Weekly_1d)
attach(Weekly_1d)
logistic.fit_1d=glm(Direction~Lag2,data=Weekly_1d, family=binomial)
summary(logistic.fit_1d)

detach(Weekly_1d)
Weekly_1d2<-Weekly[Weekly$Year>2008,]
attach(Weekly_1d2)

logistic.fit_1d2=glm(Direction~Lag2,data=Weekly_1d2, family=binomial)
logistic.probs_1d2=predict(logistic.fit_1d2,data=Weekly_1d2, type="response")
#logistic.probs_1d2
logistic.probs_1d2[1:10]
logistic.pred_1d=rep("Down",dim(Weekly_1d2)[1])
logistic.pred_1d[logistic.probs_1d2>0.5]="Up"
logistic.pred_1d

#Confusion Matrix
cmatrix_1d2<-table(logistic.pred_1d,Weekly_1d2$Direction)
str(cmatrix_1d2)
cmatrix_1d2

Accuracy_1d2<-(cmatrix_1d2[1,1]+cmatrix_1d2[2,2])/(sum(cmatrix_1d2))
Accuracy_1d2
Error_1d2<-(cmatrix_1d2[1,2]+cmatrix_1d2[2,1])/(sum(cmatrix_1d2))
Error_1d2
Sensitivity_1d2<-(cmatrix_1d2[2,2])/(cmatrix_1d2[1,2]+cmatrix_1d2[2,2])
Sensitivity_1d2
Specificity_1d2<-(cmatrix_1d2[1,1])/(cmatrix_1d2[1,1]+cmatrix_1d2[2,1])
Specificity_1d2

#(e) Repeat (d) using LDA.


Weekly_1d<-Weekly[Weekly$Year<=2008,]
str(Weekly_1d)
str(Weekly_1d2)
detach(Weekly_1d2)
attach(Weekly_1d)
train=(Year<2009)
#Year<2009 #Just here for reference
lda.fit=lda(Direction~Lag2, data=Weekly_1d)
#Weekly_2009 = Weekly[!train,]
Direction_2009 = Weekly_1d2$Direction
lda.pred_1e = predict(lda.fit,Weekly_1d2)
str(lda.pred_1e)
names(lda.pred_1e)
lda.pred_1e$class
lda.pred_1e$posterior
lda.class_1e = lda.pred_1e$class
cmatrix_1e<-table(lda.class_1e,Direction_2009)
cmatrix_1e
mean(lda.class_1e==Direction_2009)

Accuracy_1e<-(cmatrix_1e[1,1]+cmatrix_1e[2,2])/(sum(cmatrix_1e))
Accuracy_1e
Error_1e<-(cmatrix_1e[1,2]+cmatrix_1e[2,1])/(sum(cmatrix_1e))
Error_1e
Sensitivity_1e<-(cmatrix_1e[2,2])/(cmatrix_1e[1,2]+cmatrix_1e[2,2])
Sensitivity_1e
Specificity_1e<-(cmatrix_1e[1,1])/(cmatrix_1e[1,1]+cmatrix_1e[2,1])
Specificity_1e


#(f) Repeat (d) using QDA.

train=(Year<2009)
#Year<2009 #Just here for reference
qda.fit=qda(Direction~Lag2, data=Weekly_1d)
#Weekly_2009 = Weekly[!train,]
Direction_2009 = Weekly_1d2$Direction
Direction_2009
lda.pred_1f = predict(qda.fit,Weekly_1d2)
str(lda.pred_1f)
names(lda.pred_1f)
lda.pred_1f$class
lda.pred_1f$posterior
lda.class_1f = lda.pred_1f$class
cmatrix_1f<-table(lda.class_1f,Direction_2009)
cmatrix_1f
mean(lda.class_1f==Direction_2009)

Accuracy_1f<-(cmatrix_1f[1,1]+cmatrix_1f[2,2])/(sum(cmatrix_1f))
Accuracy_1f
Error_1f<-(cmatrix_1f[1,2]+cmatrix_1f[2,1])/(sum(cmatrix_1f))
Error_1f
Sensitivity_1f<-(cmatrix_1f[2,2])/(cmatrix_1f[1,2]+cmatrix_1f[2,2])
Sensitivity_1f
Specificity_1f<-(cmatrix_1f[1,1])/(cmatrix_1f[1,1]+cmatrix_1f[2,1])
Specificity_1f


#(g) Repeat (d) using KNN with K = 1.

library(class)
train.X=cbind(Lag2) #training data of predictors
test.X=cbind(Weekly_1d2$Lag2) #test data of predictors
train.Direction=Weekly_1d$Direction #training data of response
set.seed(1) #with multiple obsns tied as nearest neighbors, randomly break the tie
knn.pred=knn(train.X,test.X,train.Direction,k=5)
cmatrix_1g<-table(knn.pred,Direction_2009)
cmatrix_1g

Accuracy_1g<-(cmatrix_1g[1,1]+cmatrix_1g[2,2])/(sum(cmatrix_1g))
Accuracy_1g
Error_1g<-(cmatrix_1g[1,2]+cmatrix_1g[2,1])/(sum(cmatrix_1g))
Error_1g
Sensitivity_1g<-(cmatrix_1g[2,2])/(cmatrix_1g[1,2]+cmatrix_1g[2,2])
Sensitivity_1g
Specificity_1g<-(cmatrix_1g[1,1])/(cmatrix_1g[1,1]+cmatrix_1g[2,1])
Specificity_1g


#(h) Which of these methods appears to provide the best results on this data?

# KNN appears to provide the "best" results with most specificity of all.

#(i) Experiment with different combinations of predictors, including possible transformations and interactions, 
#for each of the methods. Report the variables, method, and associated confusion matrix that appears to provide 
#the best results on the held out data. Note that you should also experiment with values for K in the KNN 
#classifiers. 



#Problem 2
#Perform ROC analysis and present the results for logistic regression and LDA used for the best model chosen 
#in Question 1(i).

##logistic regression 
LR.fit = logistic.fit_1d2
##predict probability of "UP"
LR.pred = predict(LR.fit,type="response")
library(class)
#Contrasts(Weekly_1d$Direction)

## Calculate FPR and TPR under a given threshold
roc.curve=function(s,print=FALSE){
Ps=(LR.pred>s)*1
FP=sum((Ps==1)*(Direction=="Down"))/sum(Direction=="Down")
TP=sum((Ps==1)*(Direction=="Up"))/sum(Direction=="Up")
if(print==TRUE){
print(table(Observed=Weekly_1d2$Direction,Predicted=Ps))
}
vect=c(FP,TP)
names(vect)=c("FPR","TPR")
return(vect)
}
threshold=0.5
roc.curve(threshold,print=TRUE)

## Plot ROC curve
ROC.curve=Vectorize(roc.curve)
M.ROC=ROC.curve(seq(0,1,by=0.01))
plot(M.ROC[1,],M.ROC[2,],col="grey",lwd=2,type="l",xlab="False positive rate",ylab="True positive rate")


LDA.fit = lda.fit
## predict probabilities of training data
LDA.pred0 = predict(LDA.fit,type="response")
LDA.pred = LDA.pred0$posterior[,2]

## Calculate FPR and TPR under a given threshold
roc.curve=function(s,print=FALSE){
Ps=(LDA.pred>s)*1
FP=sum((Ps==1)*(Direction=="Down"))/sum(Direction=="Down")
TP=sum((Ps==1)*(Direction=="Up"))/sum(Direction=="Up")
if(print==TRUE){
print(table(Observed=Weekly_1d$Direction,Predicted=Ps))
}
vect=c(FP,TP)
names(vect)=c("FPR","TPR")
return(vect)
}
threshold=0.5
roc.curve(threshold,print=TRUE)


## Plot ROC Curve
ROC.curve=Vectorize(roc.curve)
M.ROC=ROC.curve(seq(0,1,by=0.01))
plot(M.ROC[1,],M.ROC[2,],col="grey",lwd=2,type="l",xlab="False positive rate",ylab="True positive rate")


#Removes all existing objects and variable assignments. 
rm(list=ls())
ls()

#Problem 3
#In this problem, you will develop a model to predict whether a given car gets high or low gas mileage based 
#on the Auto data set.

library(ISLR)	#Loads the ISLR library
library(MASS)	#Loads the MASS library
str(Auto)

#(a) Create a binary variable, mpg01, that contains a 1 if mpg contains a value above its median, and a 0 
#if mpg contains a value below its median. You can compute the median using the median( ) function. 
#Note that you may find it helpful to use the data.frame( ) function to create a single data set containing 
#both mpg01 and the other Auto variables.
mpg01<-ifelse(Auto$mpg>median(Auto$mpg),1,0)
mpg01
Auto3a<-data.frame(Auto,mpg01)
str(Auto3a)

#(b) Explore the data graphically in order to investigate the association between mpg01 and the other 
#features. Which of the other features seem most likely to be useful in predicting mpg01? Scatterplots 
#and Boxplots may be useful tools to answer this question. Describe your findings.

pairs(Auto3a$mpg01~Auto3a$cylinders+Auto3a$displacement+Auto3a$horsepower+Auto3a$weight+Auto3a$acceleration+Auto3a$year+Auto3a$origin)
pairs(Auto3a, col="purple",pch=3)

#Visually, Weight, Horsepower, and acceleration seem to be useful in predicting mpg01.

#(c) Split the data into a training set and a test set.

#Training data set of the first 300 rows
Auto3c1<-Auto3a[c(1:300),]
str(Auto3c1)

#Test data set of the last 92 rows
Auto3c2<-Auto3a[c(301:392),]
str(Auto3c2)


#(d) Perform LDA on the training data in order to predict mpg01 using the variables that seemed most 
#associated with mpg01 in (b). What is the test error of the model obtained?


lda.fit3d=lda(Auto3c1$mpg01~Auto3c1$horsepower+Auto3c1$weight+Auto3c1$acceleration, data=Auto3c1)
str(lda.fit3d)
Direction3d = Auto3c1$mpg01
lda.pred3d = predict(lda.fit3d,Auto3c2)
str(lda.pred3d)
names(lda.pred3d)
lda.pred3d$class
lda.pred3d$posterior
lda.class3d = lda.pred3d$class
cmatrix3d<-table(lda.class3d,Direction3d)
cmatrix3d
mean(lda.class3d==Direction3d)

Accuracy3d<-(cmatrix3d[1,1]+cmatrix3d[2,2])/(sum(cmatrix3d))
Accuracy3d
Error3d<-(cmatrix3d[1,2]+cmatrix3d[2,1])/(sum(cmatrix3d))
Error3d
Sensitivity3d<-(cmatrix3d[2,2])/(cmatrix3d[1,2]+cmatrix3d[2,2])
Sensitivity3d
Specificity3d<-(cmatrix3d[1,1])/(cmatrix3d[1,1]+cmatrix3d[2,1])
Specificity3d


#(e) Perform QDA on the training data in order to predict mpg01 using the variables that seemed most 
#associated with mpg01 in (b). What is the test error of the model obtained?

qda.fit3e=qda(Auto3c1$mpg01~Auto3c1$horsepower+Auto3c1$weight+Auto3c1$acceleration, data=Auto3c1)
str(qda.fit3e)
Direction3e = Auto3c1$mpg01
qda.pred3e = predict(qda.fit3e,Auto3c2)
str(qda.pred3e)
names(qda.pred3e)
qda.pred3e$class
qda.pred3e$posterior
qda.class3e = qda.pred3e$class
cmatrix3e<-table(qda.class3e,Direction3e)
cmatrix3e
mean(qda.class3e==Direction3e)

Accuracy3e<-(cmatrix3e[1,1]+cmatrix3e[2,2])/(sum(cmatrix3e))
Accuracy3e
Error3e<-(cmatrix3e[1,2]+cmatrix3e[2,1])/(sum(cmatrix3e))
Error3e
Sensitivity3e<-(cmatrix3e[2,2])/(cmatrix3e[1,2]+cmatrix3e[2,2])
Sensitivity3e
Specificity3e<-(cmatrix3e[1,1])/(cmatrix3e[1,1]+cmatrix3e[2,1])
Specificity3e

#(f) Perform logistic regression on the training data in order to predict mpg01 using the variables that 
#seemed most associated with mpg01 in (b). What is the test error of the model obtained?

logistic.fit3f=glm(Auto3c1$mpg01~Auto3c1$horsepower+Auto3c1$weight+Auto3c1$acceleration, family=binomial)
summary(logistic.fit3f)
logistic.probs3f=predict(logistic.fit3f,type="response")
#logistic.probs3f
logistic.probs3f[1:10]
logistic.pred3f=rep("Down",dim(Auto3c2)[1])
logistic.pred3f[logistic.probs3f>0.5]="Up"
logistic.pred3f

#Confusion Matrix
Direction3f = Auto3c1$mpg01
cmatrix3f<-table(logistic.pred3f,Direction3f)
str(cmatrix3f)
cmatrix3f


Accuracy3f<-(cmatrix3f[1,1]+cmatrix3f[2,2])/(sum(cmatrix3f))
Accuracy3f
Error3f<-(cmatrix3f[1,2]+cmatrix3f[2,1])/(sum(cmatrix3f))
Error3f
Sensitivity3f<-(cmatrix3f[2,2])/(cmatrix3f[1,2]+cmatrix3f[2,2])
Sensitivity3f
Specificity3f<-(cmatrix3f[1,1])/(cmatrix3f[1,1]+cmatrix3f[2,1])
Specificity3f


#(g) Perform KNN on the training data, with several values of K, in order to predict mpg01. Use only the 
#variables that seemed most associated with mpg01 in (b). What test errors do you obtain? Which value of 
#K seems to perform the best on this data set?

library(class)
train.X=cbind(Auto3c1$horsepower, Auto3c1$weight, Auto3c1$acceleration) #training data of predictors
test.X=cbind(Auto3c2$horsepower, Auto3c2$weight, Auto3c2$acceleration) #test data of predictors
train.Direction=Auto3c1$mpg01 #training data of response
set.seed(1) #with multiple obsns tied as nearest neighbors, randomly break the tie
knn.pred=knn(train.X,test.X,train.Direction,k=5)
Direction3g = Auto3c2$mpg01
cmatrix3g<-table(knn.pred,Direction3g)
cmatrix3g

Accuracy3g<-(cmatrix3g[1,1]+cmatrix3g[2,2])/(sum(cmatrix3g))
Accuracy3g
Error3g<-(cmatrix3g[1,2]+cmatrix3g[2,1])/(sum(cmatrix3g))
Error3g
Sensitivity3g<-(cmatrix3g[2,2])/(cmatrix3g[1,2]+cmatrix3g[2,2])
Sensitivity3g
Specificity3g<-(cmatrix3g[1,1])/(cmatrix3g[1,1]+cmatrix3g[2,1])
Specificity3g


graphics.off()