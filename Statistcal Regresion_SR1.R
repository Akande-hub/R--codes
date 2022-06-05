#### Loading the data set
data_purchase <- read.csv("data_purchase_behaviour.csv", header = TRUE, sep = ",")
View(data_purchase)

##### Question 1
library(psych)
library(dplyr)
library(knitr)
library(ggplot2)
library(xtable)
library(scales)

#### Data set descriptions
summary(data_purchase)
describe(data_purchase)

#### Exploring each categorical variable with bar plot to get the percentage of each category in each variable
## Plot Stay_In_Current_City_Years 
count(data_purchase,Stay_In_Current_City_Years)
ggplot(data = data_purchase, aes(x = factor(Stay_In_Current_City_Years), 
                                 y = prop.table(stat(count)), 
                                 label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  geom_bar(fill='blue') +labs(x = 'Stay_In_Current_City_Years', y = 'percentage')

## Plot  Marital_Statues 
count(data_purchase,Marital_Status)
ggplot(data = data_purchase, aes(x = factor(Marital_Status), 
                                 y = prop.table(stat(count)), 
                                 label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  geom_bar(fill='green') +labs(x = 'Marital_Status', y = 'percentage')

## Plot  Gender
count(data_purchase,Gender)
ggplot(data = data_purchase, aes(x = factor(Gender), 
                                 y = prop.table(stat(count)), 
                                 label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  geom_bar(fill='orange') +labs(x = 'Gender', y = 'percentage')

## Plot City_Category
count(data_purchase,City_Category)
ggplot(data = data_purchase, aes(x = factor(City_Category), 
                          y = prop.table(stat(count)), 
                          label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + 
  geom_bar(fill='purple') +labs(x = 'City_Category', y = 'percentage')
################################################
### Tabular representation categorical variable
table2 = table(data_purchase$Gender,data_purchase$Marital_Status)
ftable(table2)
ftable(round(100*prop.table(table2),3))

### Creating a contingency table consisting of the proportion categorical data-set
table3 = table(data_purchase$Gender,data_purchase$Marital_Status,data_purchase$City_Category)
ftable(table3)
ftable(round(100*prop.table(table3),3))

hist(data_purchase$Age_num)
hist(data_purchase$Purchase)

qqnorm(data_purchase$Purchase)
qqnorm(data_purchase$Age_num)
## representing the statistical summary of some numerical variables with table
des_Mar <- data.frame(describe(data_purchase$Marital_Status),row.names = "Marital_Status")
des_Age <- data.frame(describe(data_purchase$Age_num),row.names = "Age_num")
des_SC <- data.frame(describe(data_purchase$Stay_In_Current_City_Years),row.names = "Stay_In_Current_City_Years")
des_Pur <- data.frame(describe(data_purchase$Purchase),row.names = "Purchase")
table1 <- t(rbind.data.frame(des_Age,des_Pur,des_SC,des_Mar))
kable(table1)
###########################

####### Question 2
model2 <- lm(Purchase~Age_num,data = data_purchase )
summary(model2)
describe(model2)

###### Question 3
model3 <- lm(Purchase~Gender,data = data_purchase )
summary(model3)
describe(model3)

