
#--------------------------------Linear Regression -------------------------------------
setwd()

###Linear Regression Sample
library(dplyr)
library(ggplot2)
##Loading Data
mmix<-read.csv("MMix.csv",header=TRUE,stringsAsFactors=FALSE)

##Checking Data Characteristics
dim(mmix)
str(mmix)
head(mmix)
tail(mmix)
table(mmix$Website.Campaign)
#DV=1/0 variable :presence /absence of Sales

#Dv = Sales

#summary statistics
summary(mmix)
summary(mmix$NewVolSales)


#checking outliers
x<-boxplot(mmix$NewVolSales,col="blue")
out<-x$out

#To get list of outliers
#Outlier treatment
x$out
index<-mmix$NewVolSales %in% x$out
sum(index)
length(index)
non_outlier<-mmix[-index,]
dim(non_outlier)

#checking missing values
colSums(is.na(mmix))
summary(mmix)

#Treating missing values
mmix$NewVolSales[is.na(mmix$NewVolSales)]<-mean(mmix$NewVolSales,na.rm=TRUE)
summary(mmix$Base.Price)

#--------------------------------Exploratory Analysis -------------------------------------
library(ggplot2)

##Univariate Analysis
#Viz
qplot(mmix$NewVolSales)
hist(mmix$NewVolSales)
hist(mmix$Base.Price)


##Bivariate analysis 

#Viz
with(mmix,qplot(NewVolSales,Base.Price))
with(mmix,qplot(NewVolSales,InStore))
qplot(mmix$NewVolSales,mmix$Radio)

#Correlations
cor(mmix$NewVolSales,mmix$Base.Price)
with(mmix,cor(NewVolSales,Radio))
with(mmix,cor(NewVolSales,InStore))


##What is the use of log variables?
##To make a variable in scale with the other variable
with(mmix,qplot(log(NewVolSales),InStore))

##Creating Indicator Variables
##Why indicator or dummy variables? : Because a model does not accept chaaracter values.
##Only numbers of factors.

unique(mmix$Website.Campaign)
table(mmix$Website.Campaign)
mmix$FB<-ifelse(mmix$Website.Campaign=="Facebook",1,0)


##Creating New Variables

#Data TRansformations
mmix$LnSales<-log(mmix$NewVolSales)
mmix$LnPrice<-log(mmix$Base.Price)
mmix$OfflineSpend<-mmix$Radio+mmix$TV+mmix$InStore


#Create price buckets - converting numeric variable into categorical
mmix$Price_Bkt[mmix$Base.Price < 15.03]<-"Low"
mmix$Price_Bkt[mmix$Base.Price >= 15.03 & mmix$Base.Price < 15.33]<-"Avg"
mmix$Price_Bkt[mmix$Base.Price >= 15.33 & mmix$Base.Price < 15.64]<-"High"
mmix$Price_Bkt[mmix$Base.Price >= 15.64]<-"V.High"


#--------------------------------Building models -------------------------------------

##Building SimpLe Linear Regression Model
?lm
attach(mmix)
Reg<-lm(NewVolSales~Base.Price,data=mmix)

#y=a+bx
#sales=a+b(base.price)
#Newvolsales=53487-2176(base.price)

#Checking summary of the regression object "Reg"
Reg
summary(Reg)

#Metrics to assess a model:
#Rsquare
#Coefficients
#P values : Significance levels of the IDV's
#Residuals distribution

#Factor variables as IDV's
#Remember one of the factor type becomes a baseline.The estimates of the other
#types of factor are only given by the model.

Reg<-lm(NewVolSales~as.factor(Price_Bkt),data=mmix)
summary(Reg)
#Creating dummy for low bucket since it is significant
mmix$PrizeBktLow<-ifelse(mmix$Price_Bkt=="Low",1,0)

attach(mmix)
Reg<-lm(NewVolSales~PrizeBktLow,data=mmix)
summary(Reg)

#Getting the formula
formula(Reg)

##Multivariate Regression Model
#Iteration 1
Mulreg<-lm(NewVolSales~Base.Price+InStore+as.factor(Website.Campaign),data=mmix)
Mulreg
summary(Mulreg)

#Iteration 2
Mulreg<-lm(NewVolSales~Base.Price+InStore+Radio+TV+as.factor(Website.Campaign),data=mmix)
Mulreg
summary(Mulreg)

#Iteration 3
Mulreg<-lm(NewVolSales~LnPrice+InStore+as.factor(Website.Campaign),data=mmix)
Mulreg
summary(Mulreg)

#Choose the model with the highest R square 
#Getting the formula
formula(Mulreg)

#--------------------------------Testing models -------------------------------------

##Getting predicted values
PredSales<-predict(Mulreg,data=train)
PredSales


##Finding Residuals
ResSales<-resid(Mulreg)
ResSales

plot(ResSales)

##Plotting Residuals vs Predicted Values
##Checking Heteroskedastcity - exists if there is a pattern between predicted values and error

plot(PredSales,ResSales,abline(0,0))
plot(NewVolSales,ResSales,abline(0,0))


##Plotting actual vs predicted values
plot(mmix$NewVolSales,col="blue",type="l")
lines(PredSales,col="red",type="l")

##Try on a different validation data
sampling<-sort(sample(nrow(mmix), nrow(mmix)*.7))
head(sampling)
length(sampling)

#--------------------------------Training and Test Splits -------------------------------------

#Select training sample
train<-mmix[sampling,]
test<-mmix[-sampling,]
nrow(train)
nrow(test)

predict(Mulreg,data=test)

#--------------------------------checking for multicollinearity within variables -------------------------------------

# What is multicollinearity , a great article!
# http://blog.minitab.com/blog/understanding-statistics/handling-multicollinearity-in-regression-analysis

#The function is vif .It belongs to car package
library(car)
vif(Mulreg)

#vif= variation inflation factor
##If the variables in the model have a vif>=10, then you can exclude them from the model.

#What is vif?:
#http://support.minitab.com/en-us/minitab/17/topic-library/modeling-statistics/regression-and-correlation/model-assumptions/what-is-a-variance-inflation-factor-vif/