#Script Title: ISEN_613_Assignment-05_Script File_Manikonda Kaushik
#Script Purpose: To satisfy the requirements for ISEN 613, Homework-05

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
sink("C:\\Users\\rajak\\Documents\\Fall_2020\\ISEN_613 ENGR Data Analysis\\Homeworks\\Homework_05/HW05_Output File",split=TRUE)


#This command directs all graphic output to a pdf file
pdf("C:\\Users\\rajak\\Documents\\Fall_2020\\ISEN_613 ENGR Data Analysis\\Homeworks\\Homework_05/HW05_Output_File.pdf")

library(ISLR)

#Impute Data
str(College)
dim(College)
names(College)
#Shows the total number of observations with missing "NA" data
sum(is.na(College))

#Cleans the College dataset and produces a new colclean table with no missing observations.
colclean<-na.omit(College)
str(colclean)
sum(is.na(colclean))

#(a) Perform best subset selection to the data. What is the best model obtained according to Cp, 
#BIC and adjusted R2? Show some plots to provide evidence for your answer, and report the coefficients 
#of the best model. 

install.packages("leaps")
library(leaps)
regfit.full = regsubsets(Apps~.,colclean)
summary(regfit.full)

regfit.full = regsubsets(Apps~.,data=colclean,nvmax=18)
summary(regfit.full)
reg.summary = summary(regfit.full)

par(mfrow=c(2,2))
#Figure 1
plot(reg.summary$rss,xlab="Number of predictors",ylab="RSS",type="l")

#Figure 2
plot(reg.summary$adjr2,xlab="Number of predictors",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)

#Figure 3
plot(reg.summary$cp,xlab="Number of predictors",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)

#Figure 4
plot(reg.summary$bic,xlab="Number of predictors",ylab="BIC",type='l')
which.min(reg.summary$bic)
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)

##print the coefficient estimates of the 10-predictor model
coef(regfit.full,10)

#Adjusted R square and Cp predict a 10 predictor model as the best one. While BIC predicts
#a 6 predictor model to be the best for this data. The coefficients for the 10 predictor model
#are shown above.




#(b) Repeat (a) using forward stepwise selection and backwards stepwise selection. How does your answer 
#compare to the results in (a)?

regfit.fwd=regsubsets(Apps~.,data=colclean,nvmax=18, method="forward")
summary(regfit.fwd)

regfit.bwd=regsubsets(Apps~.,data=colclean,nvmax=18, method="backward")
summary(regfit.bwd)

##print the coefficient estimates of the 10-predictor model
coef(regfit.full,10)
coef(regfit.fwd,10)
coef(regfit.bwd,10)

##Randomly split data into a training set and a test set
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(colclean), rep=TRUE)
test=(!train)

##Perform best subset selection
regfit.best=regsubsets(Apps~.,data=colclean[train,],nvmax=18)

##building an "X" matrix from test data
test.mat=model.matrix(Apps~.,data=colclean[test,])

###Compute test MSE of the 18 models
val.errors=rep(NA,17)
for(i in 1:17){
coefi=coef(regfit.best,id=i)
pred=test.mat[,names(coefi)]%*%coefi
val.errors[i]=mean((colclean$Apps[test]-pred)^2)
}
val.errors

###Find the best model
which.min(val.errors)
coef(regfit.best,10)

##after finding the best model, we need to fit this model using
#the full data set to obtain more accurate coefficient estimates
regfit.best=regsubsets(Apps~.,data=colclean,nvmax=19)
coef(regfit.best,10)

predict.regsubsets=function(object,newdata,id,...){
form=as.formula(object$call[[2]])
mat=model.matrix(form,newdata)
coefi=coef(object,id=id)
xvars=names(coefi)
mat[,xvars]%*%coefi
}

##Note: need to perform best subset selection in each fold; need to
#compute test error for each model in each fold
k=10
set.seed(1)
folds=sample(1:k,nrow(colclean),replace=TRUE)
cv.errors=matrix(NA,k,17,dimnames=list(NULL, paste(1:17)))
for(j in 1:k){
best.fit=regsubsets(Apps~.,data=colclean[folds!=j,],nvmax=17)
for(i in 1:17){
pred=predict(best.fit,colclean[folds==j,],id=i)
cv.errors[j,i]=mean((colclean$Apps[folds==j]-pred)^2)
}
}
cv.errors

mean.cv.errors = apply(cv.errors,2,mean) #average errors over folds
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')

##find coefficient estimates of the selected model using all data
reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,4)

#This method selects a 4 predictor model instead of a 10-predictor model like
#best subset selection in a.






#(d) Fit a ridge regression model on the data. Use cross-validation to select the optimal value of l. 
#Create plots of the cross-validation error as a function of l. Report the resulting coefficient 
#estimates.

##the function glmnet() in the glmnet package
install.packages("glmnet")
library(glmnet)

x=model.matrix(Apps~.,colclean)[,-1]
y= colclean$Apps

grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid) #alpha=1: LASSO
dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]

##the sum of squares of those estimates
sqrt(sum(coef(ridge.mod)[-1,50]^2))

##next, the 60th value of lambda
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

##prediction of the coefficients for new value of lambda
##for example, lambda=25

predict(ridge.mod,s=25,type="coefficients")[1:18,]

##split the data into a training and a test set
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

##fit ridge regression on training data
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid)

##predict on test set using lamda=4
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)

##cross validation to get the best lambda
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

##now predict with the best lambda
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)

##refit ridge regression on the full dataset
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:18,]



#(c) Fit a lasso model on the data. Use cross-validation to select the optimal value of l. Create plots 
#of the cross-validation error as a function of l. Report the resulting coefficient estimates.

x=model.matrix(Apps~.,colclean)[,-1]
y= colclean$Apps

grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(x,y,alpha=1,lambda=grid) #alpha=1: LASSO
dim(coef(lasso.mod))

lasso.mod$lambda[50]
coef(lasso.mod)[,50]

##the sum of squares of those estimates
sqrt(sum(coef(lasso.mod)[-1,50]^2))

##next, the 60th value of lambda
lasso.mod$lambda[60]
coef(lasso.mod)[,60]
sqrt(sum(coef(lasso.mod)[-1,60]^2))

##prediction of the coefficients for new value of lambda
##for example, lambda=25

predict(lasso.mod,s=25,type="coefficients")[1:18,]

##split the data into a training and a test set
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

##fit ridge regression on training data
grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)

##predict on test set using lamda=4
lasso.pred=predict(lasso.mod,s=4,newx=x[test,])
mean((lasso.pred-y.test)^2)

##cross validation to get the best lambda
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

##now predict with the best lambda
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

##refit ridge regression on the full dataset
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:18,]



#(e) Now split the data set into a training set and a test set.


#i. Fit the best models obtained in the best subset selection (according to Cp, BIC or adjusted R2)
# to the training set, and report the test error obtained.

#Using the 10-predictor model as predicted by R squared and Cp.
library(leaps)
regfit.full = regsubsets(Apps~.,colclean[c(1:700),])
reg.summary$rss
MSE<-reg.summary$rss/length(colclean$Apps)
MSE
mean(MSE)

#ii. Fit a lasso model to the training set, with l chosen by cross validation. Report the test error obtained.

x=model.matrix(Apps~.,colclean)[,-1]
y= colclean$Apps

x=model.matrix(Apps~.,colclean)[,-1]
y= colclean$Apps
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
##fit ridge regression on training data
grid=10^seq(10,-2,length=100)

#lambda = 405.8404 is the chosen value from above analysis part(c).
lasso.mod=glmnet(x[train,],y[train],alpha=0,lambda=1.97344)
##predict on test set using lamda=4
lasso.pred=predict(lasso.mod,s=1.97344,newx=x[test,])
mean((lasso.pred-y.test)^2) #test

#iii. Fit a ridge regression model to the training set, with l chosen by cross validation. 
#Report the test error obtained.

x=model.matrix(Apps~.,colclean)[,-1]
y= colclean$Apps
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
##fit ridge regression on training data
grid=10^seq(10,-2,length=100)

#lambda = 405.8404 is the chosen value from above analysis part(c).
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=405.8404)
##predict on test set using lamda=4
ridge.pred=predict(ridge.mod,s=405.8404,newx=x[test,])
mean((ridge.pred-y.test)^2) #test

#iv. Compare the test errors obtained in the above analysis (i-iii) and determine the optimal model.

#Best Subset Method: 	1138428
#Lasso Method:  		1126073
#Ridge Regression: 	976897.6
#So, the optimal model is Ridge Regression.

graphics.off()