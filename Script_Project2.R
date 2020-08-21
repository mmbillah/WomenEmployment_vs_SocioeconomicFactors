###DATA PREPARATION
#setting working directory
setwd("/Users/muhtasim/Desktop/STAT530/Projects/Project2_MLR")
#importing data
mydata=read.csv("MLR2.csv", header=T)
#data summary
summary(mydata)
dim(mydata)
#checking for NA values
NA_values=data.frame(no_of_na_values=colSums(is.na(mydata)))
#head(NA_values,28)
#removing NA values
ix=apply(mydata,1,function(x) !any(is.na(x))) #for removing any missing data for any x
#new dataset with removed NA values
newdata=mydata[ix,]
#dim of the new data set is now 460, after deleting 4 NA values
dim(newdata)


###APPLYING STATISTICAL METHOD
#fitting the data in a MLR model
fit = lm(PerFemEmploy ~ FertilityRate + RatioMaletoFemale + 
           PerFemEmployers + Agriculture + Industry + Services
         + Wage.Salaried + ContrFamWorkers + OwnAccount + Vulnerable, data = newdata)

#Model Selection
#Stepwise Regression using AIC criteria
library(MASS)
slm=stepAIC(fit,direction="both")
slm
summary(slm)
#new model
fit1=lm(PerFemEmploy ~ RatioMaletoFemale + 
          + Agriculture + Industry + Services
        + Wage.Salaried + ContrFamWorkers + OwnAccount + Vulnerable, data = newdata)
#boxcox transformation
boxcox(PerFemEmploy ~ RatioMaletoFemale + 
         + Agriculture + Industry + Services
       + Wage.Salaried + ContrFamWorkers + OwnAccount + Vulnerable, data = newdata,
       lambda = seq(-8.0, 4.0, length = 10))
#model parameters estimation
#partial slopes for the predictors
round(fit1$coefficients, 3)
#Test for the significance of the slopes
library(vars)
round(coeftest(fit1), 3)


###MODEL DIAGNOSTICS
##normality test
#Shapiro_Wilk test for normality
shapiro.test(fit1$residuals)
#QQ plot for checking normality
qqnorm(fit1$res)
qqline(fit1$res)
hist(fit1$res)
##constant variance test
#residuals vs fitted plot
plot(fit1$fitted.values, fit1$residuals)
#Breusch-Pagan test
lmtest::bptest(fit1)
##test for autocorrelation
#Durbin-Watson test
dwtest(fit1)
##linearity tests between the Y and X's
library(car)
attach(newdata)
fit.l = lm(PerFemEmploy ~ RatioMaletoFemale + 
           Agriculture + Industry + Services
           + Wage.Salaried + ContrFamWorkers + OwnAccount + Vulnerable, data = newdata)

#ceresPlots(fit.l, terms = ~ . - FertilityRate,ylim = c(-3000,100))
ceresPlots(fit.l)
##potential outlier detection
plot(fit1, 3)
plot(fit1, 4, id.n = 5)
##multicollinearity check
#load the package car
library(car) 
#variance inflation factors
vif(fit1)
##influencial points check
#Influence Plot
influenceIndexPlot(model = fit1, id.n = 5)
influencePlot(fit1, main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")


#extra
residualPlots(model = fit1, id.n = 5, layout=c(2,4))
avPlots(model = fit1, id.n = 5)
crPlots(model = fit1, id.n = 5, layout=c(2,4))
#pairwis relation plots
X=newdata[,2:11]
library(GGally)
ggpairs(X)
