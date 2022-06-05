#### Loading the data set
purch_beh <- read.csv("data_purchase_behaviour.csv", header = TRUE, sep = ",")
head(purch_beh)
xtable(head(purch_beh))

##### Question 1
library(psych)
library(dplyr)
library(knitr)
library(ggplot2)
library(xtable)
library(scales)
################################################ Data set descriptions
####### Fitting model with all the covariates

model1 <- lm(Purchase~Age_num+Gender+Marital_Status+City_Category+Stay_In_Current_City_Years,data = purch_beh )
summary(model1)

####### Fitting model with all the covariates except Stay_In_Current_City_Years
model2 <- lm(Purchase~Age_num+Gender+Marital_Status+City_Category,data = purch_beh )
summary(model2)

####### Fitting model with all the covariates except Stay_In_Current_City_Years and Marital_Status
model3 <- lm(Purchase~Age_num+Gender+City_Category,data = purch_beh )
summary(model3) 

####### Investigating the model assumptions and potential outliers using appropriate plots
# Linear Assumption (satisfied)
plot(model3, which=1)
# Normality Assumption (Not satisfied)
plot(model3, which=2)
# Homoskedasticity Assumption (satisfied)
plot(model3, which=3)
# Checking Outlier (Presence of outliers)
plot(model3, which=4)

#### Comparing Model3 with Model built on Age_num and Gender only
model <- lm(Purchase~Age_num+Gender,data = purch_beh )
## we use anova for comparing the two model because they are nested.
xtable(anova(model,model3))

# checking the association between categorical co variates and response variable
t.test(purch_beh$Purchase[purch_beh$Marital_Status=="0"],
       purch_beh$Purchase[purch_beh$Marital_Status=="1"])
t.test(purch_beh$Purchase[purch_beh$Gender=="M"],
       purch_beh$Purchase[purch_beh$Gender=="F"])
bartlett.test(Purchase ~ Marital_Status, data = purch_beh)
bartlett.test(Purchase ~ Gender, data = purch_beh)

# checking normality and homogeneity of co variate with the response variable
shapiro.test(x =purch_beh$Purchase[purch_beh$City_Category=="A"])
shapiro.test(x= purch_beh$Purchase[purch_beh$City_Category=="B"])
shapiro.test(x= purch_beh$Purchase[purch_beh$City_Category=="C"])

# checking normality with barlett
bartlett.test(Purchase ~ City_Category, data = purch_beh)


