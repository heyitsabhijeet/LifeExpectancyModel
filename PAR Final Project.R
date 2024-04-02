#Reading the dataset
led = read.csv("LED.csv")

#Exploratory Data Analysis of the data
led$Country = factor(led$Country)
led$Status = factor(led$Status)
str(led)
summary(led)

#Splitting the dataset for life expectancy in developing countries and developed countries
mled = subset(led, led$Status =="Developed")
nled = subset(led, led$Status =="Developing")

#Checking for missing values
library(visdat)
vis_miss(led)
vis_miss(mled)
vis_miss(nled)

#Removing all the NA values
led = na.omit(led)
mled = na.omit(mled)
nled = na.omit(nled)

#Omitting "Country","Year","Status" from all 3 datasets as they wont impact life expectancy in any way
led = led[,-c(1:3)]
mled = mled[,-c(1:3)]
nled = nled[,-c(1:3)]

#Renaming the the columns for convinience
colnames(led) = c("LE","AM","ID","Alc","PE","HB","M","BMI","UFD","P","TE","D","HIV","GDP","POP","TT","CT","ICR","S")
colnames(mled) = c("LE","AM","ID","Alc","PE","HB","M","BMI","UFD","P","TE","D","HIV","GDP","POP","TT","CT","ICR","S")
colnames(nled) = c("LE","AM","ID","Alc","PE","HB","M","BMI","UFD","P","TE","D","HIV","GDP","POP","TT","CT","ICR","S")

#Running initial Linear Regression on all 3 datasets
ledreg1 = lm(LE~AM+ ID+ Alc+ PE+ HB+ M+ BMI+ UFD+ P+ TE+ D+ HIV+ GDP+ POP+ TT+ CT+ ICR+ S, data = led)
summary(ledreg1)
mledreg1 = lm(LE~AM+ ID+ Alc+ PE+ HB+ M+ BMI+ UFD+ P+ TE+ D+ HIV+ GDP+ POP+ TT+ CT+ ICR+ S, data = mled)
summary(mledreg1)
nledreg1 = lm(LE~AM+ ID+ Alc+ PE+ HB+ M+ BMI+ UFD+ P+ TE+ D+ HIV+ GDP+ POP+ TT+ CT+ ICR+ S, data = nled)
summary(nledreg1)

#Checking for multi-collinearity between factors
#install.packages("corrplot")
library(corrplot)
CM1 = cor(led[,-c(1)])
CM2 = cor(mled[,-c(1)])
CM3 = cor(nled[,-c(1)])
corrplot(CM1, method = "color")

#Checking for VIF values
library(car)
vif(ledreg1) 
vif(ledreg2)
vif(ledreg3)


# Dataset(led): Linear regression using Step-forward method

#Checking if transformation is required
pairs(LE~AM+ ID+ Alc+ PE+ HB+ M+ BMI+ UFD+ P+ TE+ D+ HIV+ GDP+ POP+ TT+ CT+ ICR+ S, data = led)

#Step-forward method
library(MASS)
freg1 = lm(LE~1, data = led)
summary(freg1)
fstep1 = add1(freg1, scope = led[,-1], test = 'F', trace = TRUE)
freg2 = lm(LE~S, data = led)
summary(freg2)
fstep2 = add1(freg2, scope = led[,-1], test = 'F', trace = TRUE)
freg3 = lm(LE~S+HIV, data = led)
summary(freg3)
fstep3 = add1(freg3, scope = led[,-1], test = 'F', trace = TRUE)
freg4 = lm(LE~S+HIV+ICR, data = led)
summary(freg4)
fstep4 = add1(freg4, scope = led[,-1], test = 'F', trace = TRUE)
freg5 = lm(LE~S+HIV+ICR+BMI, data = led)
summary(freg5)
fstep5 = add1(freg5,scope = led[,-1], test = 'F', trace = TRUE)
freg6 = lm(LE~S+HIV+ICR+BMI+PE, data = led)
summary(freg6)
fstep6 = add1(freg6, scope = led[,-1], test = 'F', trace = TRUE)
freg7 = lm(LE~S+HIV+ICR+BMI+PE+D, data = led)
summary(freg7)
fstep7 = add1(freg7, scope = led[,-1], test = 'F', trace = TRUE)
freg8 = lm(LE~S+HIV+ICR+BMI+PE+D+Alc, data = led)
summary(freg8)
fstep8 = add1(freg8, scope = led[,-1], test = 'F', trace = TRUE)
freg9 = lm(LE~S+HIV+ICR+BMI+PE+D+Alc+CT, data = led)
summary(freg9)
fstep9 = add1(freg9, scope = led[,-1], test = 'F', trace = TRUE)
freg10 = lm(LE~S+HIV+ICR+BMI+PE+D+Alc+CT+P, data = led)
summary(freg10)
fstep10 = add1(freg10, scope = led[,-1], test = 'F', trace = TRUE)
freg11 = lm(LE~S+HIV+ICR+BMI+PE+D+Alc+CT+P+TE, data = led)
summary(freg11)
fstep11 = add1(freg11, scope = led[,-1], test = 'F', trace = TRUE) 
#We stop here as the P-value of all factors is greater than 0.05

# Dataset(mled): Linear Regression using Step-backward method

#Checking if transformation is required
pairs(LE~AM+ ID+ Alc+ PE+ HB+ M+ BMI+ UFD+ P+ TE+ D+ HIV+ GDP+ POP+ TT+ CT+ ICR+ S, data = mled)

#Step-backward method
library(MASS)
breg1 = lm(LE~AM+ ID+ Alc+ PE+ HB+ M+ BMI+ UFD+ P+ TE+ D + HIV+ GDP+ POP+ TT+ CT+ ICR+ S, data = mled)
summary(breg1)
bstep1 = drop1(breg1, scope = mled, test = 'F', trace = TRUE)
breg2 = lm(LE~AM+ ID+ Alc+ PE+ HB+ M+ BMI+ UFD+ P+ TE +D+ GDP+ POP+ TT+ CT+ ICR+ S, data = mled)
summary(breg2)
bstep2 = drop1(breg2, scope = mled[,-c(13)], test = 'F', trace = TRUE)
breg3 = lm(LE~AM+ ID+ Alc+ PE+ HB+ M+ BMI+ UFD+ P+ TE + GDP+ POP+ TT+ CT+ ICR+ S, data = mled)
summary(breg3)
bstep3 = drop1(breg3, scope = mled[,-c(12,13)], test = 'F', trace = TRUE)
breg4 = lm(LE~AM+ ID+ Alc+ PE+ HB+ M+ BMI+ UFD+ P+ GDP+ POP+ TT+ CT+ ICR+ S, data = mled)
summary(breg4)
bstep4 = drop1(breg4, scope = mled[,-c(11,12,13)], test = 'F', trace = TRUE)
breg5 = lm(LE~AM+ ID+ Alc+ PE+ HB+ M+ BMI+ UFD+ P+ GDP+ TT+ CT+ ICR+ S, data = mled)
summary(breg5)
bstep5 = drop1(breg5, scope = mled[,-c(11,12,13,15)], test = 'F', trace = TRUE)
breg6 = lm(LE~ID+ Alc+ PE+ HB+ M+ BMI+ UFD+ P+ GDP+ TT+ CT+ ICR+ S, data = mled)
summary(breg6)
bstep6 = drop1(breg6, scope = mled[,-c(2,11,12,13,15)], test = 'F', trace = TRUE)
breg7 = lm(LE~ID+ Alc+ PE+ HB+ M+ BMI+ UFD+ P+ GDP+ TT+ ICR+ S, data = mled)
summary(breg7)
bstep7 = drop1(breg7, scope = mled[,-c(2,11,12,13,15,17)], test = 'F', trace = TRUE)
breg8 = lm(LE~Alc+ PE+ HB+ M+ BMI+ UFD+ P+ GDP+ TT+ ICR+ S, data = mled)
summary(breg8)
bstep8 = drop1(breg8, scope = mled[,-c(2,3,11,12,13,15,17)], test = 'F', trace = TRUE)
breg9 = lm(LE~Alc+ PE+ HB+ M+ UFD+ P+ GDP+ TT+ ICR+ S, data = mled)
summary(breg9)
bstep9 = drop1(breg9, scope = mled[,-c(2,3,8,11,12,13,15,17)], test = 'F', trace = TRUE)
breg10 = lm(LE~Alc+ PE+ HB+ M+ UFD+ P+ TT+ ICR+ S, data = mled)
summary(breg10)
bstep10 = drop1(breg10, scope = mled[,-c(2,3,8,11,12,13,14,15,17)], test = 'F', trace = TRUE)
breg11 = lm(LE~Alc+ HB+ M+ UFD+ P+ TT+ ICR+ S, data = mled)
summary(breg11)
bstep11 = drop1(breg11, scope = mled[,-c(2,3,5,8,11,12,13,14,15,17)], test = 'F', trace = TRUE)
breg12 = lm(LE~Alc+ HB+ M+ UFD+ TT+ ICR+ S, data = mled)
summary(breg12)
bstep12 = drop1(breg12, scope = mled[,-c(2,3,5,8,10,11,12,13,14,15,17)], test = 'F', trace = TRUE)
breg13 = lm(LE~Alc+ HB+ UFD+ TT+ ICR+ S, data = mled)
summary(breg13)
bstep13 = drop1(breg13, scope = mled[,-c(2,3,5,7,8,10,11,12,13,14,15,17)], test = 'F', trace = TRUE)
breg14 = lm(LE~HB+ UFD+ TT+ ICR+ S, data = mled)
summary(breg14)
bstep14 = drop1(breg14, scope = mled[,-c(2,3,4,5,7,8,10,11,12,13,14,15,17)], test = 'F', trace = TRUE) 
#We stop here as the P-value of all factors is greater than 0.05


# Dataset(nled): Linear regression using Stepwise method
library(rms)
library(olsrr)

swreg1 = ols(LE~AM+ ID+ Alc+ PE+ HB+ M+ BMI+ UFD+ P+ TE+ D + HIV+ GDP+ POP+ TT+ CT+ ICR+ S, data = nled)
fastbw(swreg1, rule = 'p')
freg = lm(LE ~., data = nled)
stepwise = ols_step_forward_p(freg, details = TRUE)
stepwise$model$coefficients
stepwise$model

# Predicting the data for future

#Loading test data
ledtest = read.csv("LED Test.csv")

#Transforming test data to include only the columns present in regression equation
ledtest = ledtest[,-1]

#Predicted the values for new data
prediction = predict(freg11, newdata = ledtest)

#Stored the predicted value in the test data in separate column
ledtest$LE = prediction
