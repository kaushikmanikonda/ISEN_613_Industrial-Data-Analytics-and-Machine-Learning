#Script Title: ISEN_613_Homework-02_Script File_Manikonda Kaushik
#Script Purpose: To satisfy the requirements for ISEN 613, Homework-02

#displays any existing objects in the workspace
ls()

#Step to check which liabraries are loaded
search()

#Removes all existing objects and variable assignments. 
#Will be commented out in the final homework to avoid repetition during future executions
rm(list=ls())

#output is directed to a separate file while still appearing in the console. 
#Includes the full path to show where the output file is being written.
sink("C:\\Users\\rajak\\Documents\\Fall_2020\\ISEN_613 ENGR Data Analysis\\Homeworks\\Homework_02/HW02_Output File",split=TRUE)


#This command directs all graphic output to a pdf file
pdf("C:\\Users\\rajak\\Documents\\Fall_2020\\ISEN_613 ENGR Data Analysis\\Homeworks\\Homework_02/HW02_Output_File.pdf")

#Problem 1
#Use the Auto data set to answer the following questions:
library(ISLR) #Loads ISLR Library
str(Auto)	#Structure of Auto dataset
names(Auto)	#All variables in the Auto dataset
attach(Auto)	#Makes all variables in Auto dataset available
#for calling with just their names

#(a) Perform a simple linear regression with mpg as the response and 
#horsepower as the predictor. Comment on the output. For example
lm_fit1<-lm(mpg~horsepower)
summary(lm_fit1)

#i.      Is there a relationship between the predictor and the response?
#	Yes, the p-value for the horsepower slope is very close to zero.
# Which means we have to reject the null hypothesis that there is no 
# relationship between the response (mpg) and the predictor (horsepower).

#ii.     How strong is the relationship between the predictor and the response?
#	The coefficient for horsepower is -0.157845, meaning there is a negative
# relationship between mpg and horsepower. An increase of 100 in horsepower would
# additionally decrease the mpg by -15.7845. This is on top of the intercept, which
# is 39.935861.

#iii.    Is the relationship between the predictor and the response positive or negative?
#	The coefficient for horsepower is -0.157845, meaning there is a negative
# relationship between mpg and horsepower.

#iv.    How to interpret the estimate of the slope?
#	The coefficient for horsepower is -0.157845, meaning there is a negative
# relationship between mpg and horsepower. An increase of 100 in horsepower would
# additionally decrease the mpg by -15.7845. This is on top of the intercept, which
# is 39.935861.
#	Mathematically: mpg = 39.935861 - (0.157845)*horsepower

#v.     What is the predicted mpg associated with a horsepower of 98? 
predict(lm_fit1,data.frame(horsepower=c(98)))
# ANS: 24.46708

#What are the associated 95% confidence and prediction intervals?
predict(lm_fit1,data.frame(horsepower=c(98)),interval="confidence")
predict(lm_fit1,data.frame(horsepower=c(98)),interval="prediction")
#Confidence Interval:  fit      lwr      upr
#			1 24.46708 23.97308 24.96108

#Prediction Interval: fit     lwr      upr
#			1 24.46708 14.8094 34.12476


#(b) Plot the response and the predictor. Display the least squares regression line in the plot.
plot(horsepower,mpg,pch=3,ylab="Miles per Gallon",xlab="Horsepower",col="blue")
abline(lm_fit1,col="red") #adds the LS regression line to the plot

#(c) Produce the diagnostic plots of the least squares regression fit. Comment on each plot.
par( mfrow =c(2, 2))	#Multiple Diagnostic plots on the same page
plot(lm_fit1, which=1)
plot(lm_fit1, which=3)
plot(lm_fit1, which=5)

#Residuals plot indicates a nonlinear relationship b/w response and predictor.
#Funnel shape in the residuals plot indicates a non-constant variance
#Residuals vs. Leverage plot shows a few outliers with >3 values and
#many high leverage points.


#(d) Try a few different transformations of the predictor, such as log?(X),vX,X^2. Comment on your findings.
# x^2
lm_fit2=lm(mpg~horsepower+I(horsepower^2),data=Auto)
summary(lm_fit2)
par( mfrow =c(2, 2))	#Multiple Diagnostic plots on the same page
plot(lm_fit2, which=1)
plot(lm_fit2, which=3)
plot(lm_fit2, which=5)

# log(x)
lm_fit3=lm(mpg~horsepower+I(log(horsepower)),data=Auto)
summary(lm_fit3)
par( mfrow =c(2, 2))	#Multiple Diagnostic plots on the same page
plot(lm_fit3, which=1)
plot(lm_fit3, which=3)
plot(lm_fit3, which=5)

# The residuals plots are much closer to zero with the x^2 and log(x)
#transformations than they were with simple linear regression.



#Problem 2
#Use the Auto data set to answer the following questions:

#(a) Produce a scatterplot matrix which includes all of the variables in the data set.
#Which predictors appear to have an association with the response?
# Assumption: I am assuming that we are still keeping mpg as the response variable
# and analyzing the effect of all other predictors on mpg.
str(Auto)
pairs(Auto[,1:8],pch=4, col="blue")
#Displacement, Horsepower, and Weight seem to have an obvious associatio with mpg.

#(b) Compute the matrix of correlations between the variables (using the function cor()). 
#You will need to exclude the name variable, which is qualitative.
Auto2<-Auto[1:8] #Eliminates the names variable from Auto2
str(Auto2) #Verifying the elimination of names variable
cor(Auto2)	#Computing the metrix of correlations between variables

#(c) Perform a multiple linear regression with mpg as the response and all other variables 
#except name as the predictors. Comment on the output. For example,
lm_fit4<-lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year+origin,data=Auto2)
summary(lm_fit4)

#i.      Is there a relationship between the predictors and the response?
#	Yes, the F-statistic is 252.4 which is >>1 and  p-value: < 2.2e-16 
#This indicates that at least one of the predictors is associated with 
#the response (mpg).

#ii.     Which predictors have a statistically significant relationship to the response?
#	The standard errors for cylinders variable and acceleration
# variable are more than the estimated slopes themselves, so I ignored them as 
# being inaccurate parameter estimates. Weight, origin, and year have a very low 
#plausibility for their null hypothesis (of not relationship) being true. 
#Additionall, their standard errors are also smaller than the parameter estimates. 
#So, Weight, origin, and year have a statistically significant relationship with the response.
#Horsepower has a good chance for true null hypothesis. Moreover, its SE is as large
#as its parameter estimate. So, horsepower relationship to the response is not significant
#Finally, displacement might have a mildly significant relationship with the response.

#iii.    What does the coefficient for the year variable suggest?
# It suggests that there is a fairly strong positive relationship
# between the make year and the response variable (mpg). This is
# expected since newer models typically have higher mpg stats.

#(d) Produce diagnostic plots of the linear regression fit. Comment on each plot.
par( mfrow =c(2, 2))	#Multiple Diagnostic plots on the same page
plot(lm_fit4, which=1)
plot(lm_fit4, which=3)
plot(lm_fit4, which=5)

#Residuals plot indicates a nonlinear relationship b/w response and predictors.
#Funnel shape in the residuals plot indicates a non-constant variance
#Residuals vs. Leverage plot shows many outliers with >3 values and
#one high leverage point (#14).

#(e) Is there serious collinearity problem in the model? Which predictors are collinear?
library(car)
vif(lm_fit4)
#Yes, there is a serious collinearity problem in the model
#Cylinders, Displacement, Horsepower, and Weight are Collinear.

#(f) Fit linear regression models with interactions. Are any interactions statistically significant?
lm_fitf1<-lm(mpg~cylinders*displacement,data=Auto2)
summary(lm_fitf1)
lm_fitf2<-lm(mpg~horsepower*weight,data=Auto2)
summary(lm_fitf2)
lm_fitf3<-lm(mpg~acceleration*year,data=Auto2)
summary(lm_fitf3)
lm_fitf4<-lm(mpg~year*origin,data=Auto2)
summary(lm_fitf4)
lm_fitf5<-lm(mpg~displacement*horsepower,data=Auto2)
summary(lm_fitf5)
lm_fitf6<-lm(mpg~acceleration*horsepower,data=Auto2)
summary(lm_fitf6)
lm_fitf7<-lm(mpg~year*horsepower,data=Auto2)
summary(lm_fitf7)
lm_fitf8<-lm(mpg~acceleration*weight,data=Auto2)
summary(lm_fitf8)

#cylinders*displacement, and acceleration*horsepower are statistically
#significant 

#Problem 3
#Use the Carseats data set to answer the following questions:
str(Carseats)
names(Carseats)
attach(Carseats)

#(a) Fit a multiple regression model to predict Sales using Price, Urban, and US.
lm_fit3a<-lm(Sales~Price+Urban+US,data=Carseats)
summary(lm_fit3a)

#(b) Provide an interpretation of each coefficient in the model 
#(note: some of the variables are qualitative).
# For price, the p-value is very small, so we reject the null hypothesis. So, there
# is a relationship between the amount of sales and the price. The relationship
#b/w these two is negative and the slope/coefficient is -0.054459. For an increase
#of $1000 in price, the sales will decrease additionally by -54.46. 
#Mathematically sales = 13.043 - 0.054459 * Price

#For the qualitative variable "Urban", the p-value is significant. So, we
# can not reject the null hypothesis of no relationship. So, there is very
#likely no/very weak relationship between "Urban" model and sales.

#Sales are positively related to make in the U.S. (qualitative variable).
# The p-value is negligible, so there is a relationship. So, 1.2 additional 
#cars made in the U.S. are sold compared to cars not made in the U.S.

#(c) Write out the model in equation form.
#sales = 13.043469 - (0.054459 * Price) - (0.021916 * Urban) + (1.200573 * U.S.)
#Urban is binary with 2 if yes and 1 if no. Same case for U.S.

#(d) For which of the predictors can you reject the null hypothesis H_0: ß_j=0 ?
#We can reject the null hypothesis for Price and U.S. variables.
#These have very low plausibility values and hence low chance for
#null hypothesis being true.

#(e) On the basis of your answer to the previous question, fit a smaller model that only uses the 
#predictors for which there is evidence of association with the response.
lm_fit3e<-lm(Sales~Price+US,data=Carseats)
summary(lm_fit3e)

#(f) How well do the models in (a) and (e) fit the data?
summary(lm_fit3a)
summary(lm_fit3e)
#The model in a accounts for 23.35% of the variance in response while 
#the model in e accounts for 23.54%. So, these two models did not do
#a very good job of fitting the data.

#(g) Is there evidence of outliers or high leverage observations in the model from (e)?
par( mfrow =c(2, 2))	#Multiple Diagnostic plots on the same page
plot(lm_fit3e, which=1)
plot(lm_fit3e, which=3)
plot(lm_fit3e, which=5)
#No nonlinearity, No evidence of outliers (most are <3)
#Very few high leverage points

graphics.off()