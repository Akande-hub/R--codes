
#prostate <- read.csv("prostate-cancer-1.csv", header = TRUE, sep = ",")
prostate <- read.csv(file.choose())
#gifted <- read.csv("gifted.csv", header = TRUE, sep = ",")
gifted <- read.csv(file.choose())
library(psych)
library(dplyr)
library(knitr)
library(ggplot2)
library(xtable)
library(scales)
library(corrplot)
library(ggpubr)
############# Exercise 2 #########
######### Question 1 ############ History and Discription of the dataset

######### Question 2 ############
prop.table(table(prostate$Y)) # sorting binary categorical response variable in order to use Barplot
barplot((table(prostate$Y)), col="green",main = "Barplot of the response variable Y", xlab="" ,horiz = T)
hist(prostate$Y)
######### Question 3 ############
dim(prostate) # shape and dimensionality of the data
n = dim(prostate)[1]
p = dim(prostate)[2]-1
n
p
######### Question 4 ############
sum(is.na(prostate)) # Checking for missing values
summary(prostate) # Summary of the dataset
str(prostate) # Checking the data type for each variable
plot(gifted[,-11], pch = 2)
xtable(describe(prostate)) # output table in LaTeX code.
rand_var =prostate[,-1][,sample(500)[1:11]] # select some random features variable
rand_var
summary(rand_var)
describe(rand_var) # describe the data rand_var
xtable(describe(rand_var))
cor(rand_var) # correlation matrix
plot(rand_var, main = "Correlation plot of the random features variable" )
corrplot(cor(rand_var))  # correlation plots
boxplot(rand_var, main="boxplot of the random feature variables") # boxplot
xtable(summary(rand_var))



############# Exercise 4 #########
dim(gifted) # shape of the dataset
######### Question 1 ############
sum(is.na(gifted)) # Checking for missing values
plot(gifted[,-8], pch = 19, lower.panel=NULL,col="red", main="Triangular pairwise scatter plot") #Triangular pairwise scatter plot

######### Question 2 ############

cor(gifted) # correlation matrix
xtable(cor(gifted))
corrplot(cor(gifted), main = "Corrplot of the correlation matrix") # corrplot for correlation plot
######### Question 3 ############
# histogram plot with normality line.
hist(gifted$score, breaks = 10, xlim = c(150,170),
     xlab = "Score", ylab = "Probability", main = "Histogram of the response variable with test of normality", probability = T, las=1)
lines(density(gifted$score, adjust = 1), col= "Red", lwd=2)

shapiro.test(gifted$score) # test for normality.

#attach(gifted)
#ggdensity(hist(gifted$score))
#lines(density(gifted$score, adjust = 1), col= "Red", lwd=2)

####### Question 4 #############
model1 <- lm(score~motheriq,data = gifted ) # building SLR model
######### Question 5 ############
####### Investigating the model assumptions and potential outliers using appropriate plots
# Linear Assumption (satisfied)
plot(model1, which=1)
# Normality Assumption ( satisfied)
plot(model1, which=2)
# Homoskedasticity Assumption (satisfied)
plot(model1, which=3)
# Checking Outlier (Presence of outliers)
plot(model1, which=4)

######### Question 6 ############
xtable(summary(model1)) # model summary

######### Question 7 ############
#plot(gifted$motheriq,gifted$score, main='Linear Regression with Confidence and Prediction Bands')
#lines(gifted$motheriq, fitted(model1),col='blue')
#curve(predict(model1,x=gifted$motheriq, interval="confidence")[,2],add=T, col='orange')
#curve(predict(model1,x=gifted$motheriq, interval="confidence")[,3],add=T, col='orange')
##### Confidence and prediction bands.
model1.conf=predict(model1,interval = "conf")
model1.pred=predict(model1,interval = "pred")
giftc = cbind(gifted,model1.conf)
giftp = cbind(gifted,model1.pred)

ggplot(data = giftp, aes(x=motheriq, y = score)) + 
  geom_ribbon(aes(ymin=lwr,ymax=upr),fill="red", alpha=0.2) + 
  geom_ribbon(aes(ymin=giftc$lwr,ymax=giftc$upr),fill="red",alpha=0.6) +
  geom_line(aes(y=fit), color="white", size = 1.5) + 
  geom_point() +
  ggtitle("Regression line, 95% confidence and prediction Bands")
xtable(round(model1.conf))
xtable(round(model1.pred))
####### Question 8 #############
model2 <- lm(score~.,data = gifted ) # building MLR model with all the features
xtable(summary(model2)) # code for laTeX output


