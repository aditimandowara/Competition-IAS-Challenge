rm(list=ls())

#QUESTION 3==============================================================

library(tidyverse)
library(data.table)
library(sandwich) # For White correction
library(lmtest) # More advanced hypothesis testing tools
library(tseries)
library(readxl)
library (readr)
library (lubridate)
library (dplyr)
library (tidyr)
library (devtools)
library (miscTools)
library(matrixStats)
library(coefplot)

mydata <- read_excel("Training Dataset.xlsx")
## Remove spaces from the column names
names(mydata) <- gsub(" " ,"_" , names(mydata))
names(mydata)

mydata2 <- subset(mydata, Children > 0)

#Windsorising Outliers for Caring for children

outlier_values_Caring <- boxplot.stats(mydata2$Caring_for_Children)$out  # outlier values.
outlier_values_Caring <- sort(outlier_values_Caring)
boxplot(mydata2$Caring_for_Children, main="Caring for children", boxwex=1)
mtext(paste("Outliers: ", paste(outlier_values_Caring, collapse=", ")), cex=0.8)

CaringQ<-quantile(mydata2$Caring_for_Children)    
CaringRW<-CaringQ[4]+1.5*IQR(mydata2$Caring_for_Children)
mydata2$Caring_for_Children[mydata2$Caring_for_Children > CaringRW]<-CaringRW
mydata2$Caring_for_Children <- mydata2$Caring_for_Children/60

#Windsorising Outliers for weekly earnings 

outlier_values_Earnings <- boxplot.stats(mydata2$Weekly_Earnings)$out  # outlier values.
outlier_values_Earnings <- sort(outlier_values_Earnings)
boxplot(mydata2$Weekly_Earnings, main="weekly earnings", boxwex=1)
mtext(paste("Outliers: ", paste(outlier_values_Earnings, collapse=", ")), cex=0.8)

EarningQ<-quantile(mydata2$Weekly_Earnings)    
EarningRW<-CaringQ[4]+1.5*IQR(mydata2$Weekly_Earnings)
mydata2$Weekly_Earnings[mydata2$Weekly_Earnings > EarningRW]<-EarningRW

#Windsorising Outliers for weekly hours worked  

outlier_values_Working <- boxplot.stats(mydata2$Weekly_Hours_Worked)$out  # outlier values.
outlier_values_Working <- sort(outlier_values_Working)
boxplot(mydata2$Weekly_Hours_Worked, main="Hours worked", boxwex=1)
mtext(paste("Outliers: ", paste(outlier_values_Working, collapse=", ")), cex=0.8)
mydata2$Weekly_Hours_Worked <- mydata2$Weekly_Hours_Worked/7

cor(mydata2$Weekly_Hours_Worked, mydata2$Caring_for_Children)
cor(mydata2$Weekly_Earnings,mydata2$Caring_for_Children)

model2<- lm(mydata2$Caring_for_Children~mydata2$Weekly_Earnings+ mydata2$Weekly_Hours_Worked)
summary(model2)

Education_Level <- as.factor(mydata2$Education_Level)
summary(Education_Level)

cor(mydata2$Weekly_Earnings,mydata2$Weekly_Hours_Worked)

Earnings <- mydata2$Weekly_Earnings*mydata2$Weekly_Hours_Worked

model <- lm(Playing_with_Children~Earnings+Weekly_Hours_Worked+Education_Level+Weekly_Earnings, data= mydata2)
model
summary(model)

summary(model, vcov=vcovHC(model, method = "white1"))

plot(model)

BIC(model)
AIC(model)

plot(model)

#QUESTION 4 AND QUESTION 5===================================================================


#Q4a: How does leisure time change based on income ?

#Read data
mydata <- read_excel("Training Dataset.xlsx")
names(mydata) <- gsub(" " ,"_" , names(mydata))

#Define what leisure time means
mydata$Leisure <- mydata$Shopping + mydata$Eating_and_Drinking + mydata$`Socializing_&_Relaxing` + mydata$Television + mydata$Golfing + mydata$Running + mydata$Volunteering

#Classify income based on its quantile values
W <- mydata$Weekly_Earnings
mydata$Income <- ifelse( (W ==0), 1 , ifelse((W >= 1 & W <= 239),2,ifelse((W >= 240 & W <= 768),3,4)))

#Calculate average leisure time for each subset of income
ZI <- ifelse(mydata$Income == 1,mydata$Leisure,NA)
ZI <- na.omit(ZI)

ZeroIncomeLeisure <- mean(ZI)

LI <- ifelse(mydata$Income == 2,mydata$Leisure,NA)
LI <- na.omit(LI)

LowIncomeLeisure <- mean(LI)

AI <- ifelse(mydata$Income == 3,mydata$Leisure,NA)

AI <- na.omit(AI)
AvgIncomeLeisure <- mean(AI)

HI <- ifelse(mydata$Income == 4,mydata$Leisure,NA)
HI <- na.omit(HI)
HighIncomeLeisure <- mean(HI)

#Plot a graph to visualise income vs leisure 
IncomeVSLeisure <- c(ZeroIncomeLeisure,LowIncomeLeisure,AvgIncomeLeisure,HighIncomeLeisure)
plot(IncomeVSLeisure, main = "Income vs Leisure Time", xaxt = "n", ylab = "Leisure Time (In minutes)", xlab = "Income", type = "b")
Points <- c("0","1-239","240-768", "769+")
axis(1, at = 1:4, labels = Points)

#Q4b: Is there a difference between generations leisure time?

#Factorise the attribute Age Range to represent generations
mydata$Age_Range <- as.factor(mydata$Age_Range)

#Create subsets of data based on the generations
d1 <- subset(mydata, Age_Range == "0-19")
d2 <- subset(mydata, Age_Range == "20-29")
d3 <- subset(mydata, Age_Range == "30-39")
d4 <- subset(mydata, Age_Range == "40-49")
d5 <- subset(mydata, Age_Range == "50-59")
d6 <- subset(mydata, Age_Range == "60-69")
d7 <- subset(mydata, Age_Range == "70-79")
d8 <- subset(mydata, Age_Range == "80+")

#Plot a graph to visualize variation in leisure time based on the generation
AgeVsLeisure <- c(mean(d1$Leisure),mean(d2$Leisure),mean(d3$Leisure),mean(d4$Leisure),mean(d5$Leisure),mean(d6$Leisure),mean(d7$Leisure),mean(d8$Leisure))
Points2 <- c("0-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")
plot(AgeVsLeisure, xaxt = "n", main = "Generations vs Leisure Time", xlab = "Generations", ylab = "Leisure Time (In minutes)", type = "b")
axis(1, at=1:8, labels=Points2[1:8])

#Q5: Is there any change in the pattern when the great recession happened? 
# The great recession happened over the years 2007-09
# So let us check leisure time vs generations and leisure time vs income for the period 2007-09
# We will then compare the plots with the average calculated over the entire dataset

datarecession <- subset(mydata, Year >= 2007 & Year <= 2009)
r1 <- subset(datarecession, Age_Range == "0-19")
r2 <- subset(datarecession, Age_Range == "20-29")
r3 <- subset(datarecession, Age_Range == "30-39")
r4 <- subset(datarecession, Age_Range == "40-49")
r5 <- subset(datarecession, Age_Range == "50-59")
r6 <- subset(datarecession, Age_Range == "60-69")
r7 <- subset(datarecession, Age_Range == "70-79")
r8 <- subset(datarecession, Age_Range == "80+")

AgeVsLeisureR <- c(mean(r1$Leisure),mean(r2$Leisure),mean(r3$Leisure),mean(r4$Leisure),mean(r5$Leisure),mean(r6$Leisure),mean(r7$Leisure),mean(r8$Leisure))
plot(AgeVsLeisureR, xaxt = "n", main = "Recession: Generations vs Leisure Time", xlab = "Generations", ylab = "Leisure Time", type = "b")
axis(1, at=1:8, labels=Points2[1:8])

lines(AgeVsLeisure, type = "o",col = "blue")

#Classify income based on its quantile values
WR <- datarecession$Weekly_Earnings
datarecession$Income <- ifelse( (WR ==0), 1 , ifelse((WR >= 1 & WR <= 239),2,ifelse((WR >= 240 & WR <= 768),3,4)))

#Calculate average leisure time for each subset of income
ZI <- ifelse(datarecession$Income == 1,datarecession$Leisure,NA)
ZI <- na.omit(ZI)
ZeroIncomeLeisureR <- mean(ZI)

LI <- ifelse(datarecession$Income == 2,datarecession$Leisure,NA)
LI <- na.omit(LI)
LowIncomeLeisureR <- mean(LI)

AI <- ifelse(datarecession$Income == 3,datarecession$Leisure,NA)
AI <- na.omit(AI)
AvgIncomeLeisureR <- mean(AI)

HI <- ifelse(datarecession$Income == 4,datarecession$Leisure,NA)
HI <- na.omit(HI)
HighIncomeLeisureR <- mean(HI)

#Plot a graph to visualise income vs leisure 
IncomeVSLeisureR <- c(ZeroIncomeLeisureR,LowIncomeLeisureR,AvgIncomeLeisureR,HighIncomeLeisureR)
plot(IncomeVSLeisureR, main = "Recession: Income vs Leisure Time", xaxt = "n", ylab = "Leisure Time (In minutes)", xlab = "Income", type = "b")
Points <- c("0","1-239","240-768", "769+")
axis(1, at = 1:4, labels = Points)

lines(IncomeVSLeisure, type = "o",col = "blue")

#QUESTION 6===================================================================

mydata <- read_excel("Training Dataset.xlsx")
names(mydata) <- gsub(" " ,"_" , names(mydata))

training6 <- mydata
training6$Weekly_Hours_Worked <- training6$Weekly_Hours_Worked * 60/7

## Filter the activity columns from the dataset required for Question 6
Age_Range_0_19 <- (filter (training6 [ , c(4, 10:24)], training6$Age_Range == "0-19"))
Age_Range_20_29 <- filter (training6 [ , c(4, 10:24)], training6$Age_Range == "20-29")
Age_Range_30_39 <- filter (training6 [ , c(4, 10:24)], training6$Age_Range == "30-39")
Age_Range_40_49 <- filter (training6 [ , c(4, 10:24)], training6$Age_Range == "40-49")
Age_Range_50_59 <- filter (training6 [ , c(4, 10:24)], training6$Age_Range == "50-59")
Age_Range_60_69 <- filter (training6 [ , c(4, 10:24)], training6$Age_Range == "60-69")
Age_Range_70_79 <- filter (training6 [ , c(4, 10:24)], training6$Age_Range == "70-79")
Age_Range_80nAbove <- filter (training6 [ , c(4, 10:24)], training6$Age_Range == "80+")

## Calculate the mean of each activity and store in a dataframe
Mean_per_activity <-  data.frame (colMeans(Age_Range_0_19 [2:15] , na.rm = TRUE), 
                                  colMeans(Age_Range_20_29 [2:15] , na.rm = TRUE), 
                                  colMeans(Age_Range_30_39 [2:15] , na.rm = TRUE), 
                                  colMeans(Age_Range_40_49 [2:15] , na.rm = TRUE), 
                                  colMeans(Age_Range_50_59 [2:15] , na.rm = TRUE),
                                  colMeans(Age_Range_60_69 [2:15] , na.rm = TRUE),
                                  colMeans(Age_Range_70_79 [2:15] , na.rm = TRUE),
                                  colMeans(Age_Range_80nAbove [2:15] , na.rm = TRUE))
## Round to 2 decimal places
Mean_per_activity <- round (Mean_per_activity , 2)
Age_Group <-  c("0-19" , "20-29" , "30-39" , "40-49" , "50-59" , "60-69" , "70-79" , "80+")
colnames (Mean_per_activity) <- Age_Group
View(Mean_per_activity)


#QUESTION 7===================================================================

mydata <- read_excel("Training Dataset.xlsx")
names(mydata) <- gsub(" " ,"_" , names(mydata))

library(nnet)

#read the data
mydata <- read_excel("Training Dataset.xlsx")

#factorize categorical variables and convert them into a numerical form
mydata$Education_Level <- as.factor(mydata$Education_Level)
mydata$Employment_Status <- as.factor(mydata$Employment_Status)
mydata$Gender <- as.factor(mydata$Gender)
mydata$Age_Range <- as.factor(mydata$Age_Range)

mydata$Education_Level <- as.numeric(mydata$Education_Level)
mydata$Employment_Status <- as.numeric(mydata$Employment_Status)
mydata$Gender <- as.numeric(mydata$Gender)
mydata$Age_Range <- as.numeric(mydata$Age_Range)

#Run multinomial regression
model <- multinom(Employment_Status~Education_Level+Weekly_Earnings+Sleeping+Caring_for_Children+Housework+Television+Children+Age+Gender,data=subset(mydata,mydata$Weekly_Hours_Worked==0))
summary(model)

#The summary output gives values of Beta and Sigma. t value = Beta/Sigma. A high t value means a low p value, implying that the variable is significant for the dependent variable.

mydata <- read_excel("Training Dataset.xlsx")
names(mydata) <- gsub(" " ,"_" , names(mydata))

## Remove Age, Year and Total

training7 <- mydata [ , -c(1,3,9,25)]

## Make Weekly hours worked to daily minutes & change the colum name
training7$Weekly_Hours_Worked <- training7$Weekly_Hours_Worked * 60/7
colnames (training7)[7] <- "Working"

## Subset
training7_Employed <- subset(training7 , training7$Employment_Status == "Employed")
training7_UnEmployed <- subset(training7 , training7$Employment_Status == "Unemployed")

## Normalise Employed
training7_Employed$Education_Level <- as.factor(training7_Employed$Education_Level)
training7_Employed$Education_Level <- as.numeric(training7_Employed$Education_Level)
training7_Employed$Age_Range <- as.factor(training7_Employed$Age_Range)
training7_Employed$Age_Range <- as.numeric(training7_Employed$Age_Range)
training7_Employed$Gender <- as.factor(training7_Employed$Gender)
training7_Employed$Gender <- as.numeric(training7_Employed$Gender)
training7_Employed$Employment_Status <- as.factor(training7_Employed$Employment_Status)

## Normalise UnEmployed
training7_UnEmployed$Education_Level <- as.factor(training7_UnEmployed$Education_Level)
training7_UnEmployed$Education_Level <- as.numeric(training7_UnEmployed$Education_Level)
training7_UnEmployed$Age_Range <- as.factor(training7_UnEmployed$Age_Range)
training7_UnEmployed$Age_Range <- as.numeric(training7_UnEmployed$Age_Range)
training7_UnEmployed$Gender <- as.factor(training7_UnEmployed$Gender)
training7_UnEmployed$Gender <- as.numeric(training7_UnEmployed$Gender)
training7_UnEmployed$Employment_Status <- as.factor(training7_UnEmployed$Employment_Status)


## Subset Employed and Unemployed, Normalise
training7_Employed.Norm <- data.frame(sapply(training7_Employed[ ,-3] , scale))
training7_Employed.Norm <- cbind(training7_Employed$Employment_Status , training7_Employed.Norm)
colnames (training7_Employed.Norm)[1] <- "Employment_Status"
training7_Employed.Norm$Employment_Status <- as.numeric(training7_Employed.Norm$Employment_Status)
training7_Employed.Norm[is.nan(as.matrix(training7_Employed.Norm))] <- 0

training7_UnEmployed.Norm <- data.frame(sapply(training7_UnEmployed[ ,-3] , scale))
training7_UnEmployed.Norm <- cbind(training7_UnEmployed$Employment_Status , training7_UnEmployed.Norm)
colnames (training7_UnEmployed.Norm)[1] <- "Employment_Status"
training7_UnEmployed.Norm$Employment_Status <- as.numeric(training7_UnEmployed.Norm$Employment_Status)
training7_UnEmployed.Norm[is.nan(as.matrix(training7_UnEmployed.Norm))] <- 0

## Linear model for Employed
model_Employed <- lm(Employment_Status~ . , data = training7_Employed.Norm)
summary(model_Employed)
coefplot(model_Employed, title = "Coefficient Plot : EMPLOYED" , ylab = " ACTIVITIES",xlab = " COEFFICIENT VALUES",pointSize=2,color="red",lwdOuter = .6,lwdInner=1, intercept = FALSE) +theme_bw()


## Linear model for UnEmployed

model_UnEmployed <- lm(Employment_Status~ . , data = training7_UnEmployed.Norm)
summary(model_UnEmployed)
coefplot(model_UnEmployed, title = "Coefficient Plot : UNEMPLOYED" , ylab = " ACTIVITIES",xlab = " COEFFICIENT VALUES",pointSize=2,color="red",lwdOuter = .6,lwdInner=1, intercept = FALSE) +theme_bw()

#QUESTION 8===================================================================

library(readxl)
library(Hmisc)
library(leaps)
mydata <- read_excel("Training Dataset.xlsx")
mydata2 <- read_excel("Test Dataset.xlsx")

names(mydata) <- gsub(" " ,"_" , names(mydata))
names(mydata2) <- gsub(" " ,"_" , names(mydata2))

colnames(mydata2)[20] <- "Socializing_Relaxing"

#prediction model 

library(nnet)
library(kimisc)
sample2 <- sample.rows(mydata,size=33270,replace=TRUE)

sample2$Employment_Status <- as.factor(sample2$Employment_Status)
str(sample2$Employment_Status)
mydata2$Employment_Status <- relevel(sample2$Employment_Status,ref="Employed")
model <- multinom(Employment_Status~Job_Searching+Age+Gender+Socializing_Relaxing,data=subset(mydata2,Weekly_Hours_Worked==0))
outcome <- ifelse(subset(mydata2,Weekly_Hours_Worked==0)$Employment_Status==predict(model),1,0)
BIC(model)
mean(outcome)
mean(mydata$Weekly_Hours_Worked!=0)

plot(outcome)

write.csv(mydata2, file="TestOutput.csv")


