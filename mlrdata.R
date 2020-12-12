library(tidyverse)
library(ggpubr)
library(rstatix)
library(MASS)
library(ISLR)
library(ggplot2)

#Analyze data
library(readr)
mdata = read.csv("MLR dataset.csv")
head(mdata)

names(mdata)
?? mdata
mdata %>% sample_n_by(Doctor.availability.per.100.000.residents, size = 1)

lm.fit=lm(Doctor.availability.per.100.000.residents ~.,data=mdata)
summary(lm.fit)

#Regression Analysis
summary(lm.fit)$r.sq

lm.fit=lm(Doctor.availability.per.100.000.residents ~.-Annual.per.capita.income..in..1000.,data=mdata)
summary(lm.fit)

lm.fit=lm(Doctor.availability.per.100.000.residents ~ Death.rate.1000.residents*Annual.per.capita.income..in..1000.,data=mdata)
summary(lm.fit)



#Plot to show the relationship between the predictor and its response
plot(mdata$Doctor.availability.per.100.000.residents,mdata$Death.rate.1000.residents)

plot(mdata$Doctor.availability.per.100.000.residents,mdata$Death.rate.1000.residents,
     xlab = "Doctor Availabilty",
     ylab = "Death Rate",
     main="MLR Plot")
