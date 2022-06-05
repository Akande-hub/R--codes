
library(readr)      ...#4.75/12
library(dplyr)
library(psych)
library(dplyr)
library(knitr)
library(ggplot2)
library(xtable)
library(scales)
library(corrplot)
library(ggpubr)
library(class)
library(MASS)
library(kernlab)
library(mlbench)
library(reshape2)
library(ROCR)
library(ggplot2)
library(ggfortify)

prostate <- read.csv(file.choose())  ...#Correct.
View(prostate)
### Q1
dim(prostate)
str(prostate)


#### Q2    ...#Correct.
# The machine learning task is a classification task. the dataset was collected to classify those that has prostate from those that did not.

#### Q3  ...#Correct.
n   <- nrow(prostate)       # Sample size
p   <- ncol(prostate)-1  # Dimensionality of the input space
pos <- p+1            # Position of the response
x   <- prostate[,-pos]      # Data matrix: n x p matrix
y   <- prostate[, pos] 
dim(prostate)
n;p
# The shape of the data matrix is 79 by 500, where the sample space = 79 and the number of features variables  (X1,x2,...) = 500 and response variable = y

#### Q4   ...#Correct.
per = 0.05*p
per
rand_var =prostate[,-1][,sample(p)[1:per]] # select some random features variable
rand_var
summary(rand_var)
describe(rand_var)

cor(rand_var) # correlation matrix
plot(rand_var, main = "Correlation plot of the random features variable" )
corrplot(cor(rand_var))  # correlation plots
boxplot(rand_var, main="boxplot of the random feature variables") # boxplot

#### Q5  ...#Correct.

set.seed (2310)          # Set seed for random number generation to be reproducible

epsilon <- nrow(prostate)              # Proportion of observations in the test set

R <- 50   # Number of replications
test.err <- matrix(0, nrow=R, ncol=11)

for(r in 1:R)
{
  # Split the data
  nte     <- prostate  # Number of observations in the test set
  ntr     <- prostate    # Number of observations in the train set
  
  x_train = ntr[,2:p]
  y_train = ntr[,1, drop=TRUE]
  x_test = nte[,2:p]
  y_test = nte[,1,drop=TRUE]
  
  
  library(ROCR)
  library(class)
  library(MASS)
  library(kernlab)
  library(rpart)
  library(ada)
library(StratifiedRF)
  
  hold <- stratified.holdout(as.factor(prostate[,pos]), epsilon) 

  y.te         <- y_test                           # True responses in test set
  
  # 1-Linear Discriminant Analysis
  
  lda.mod        <- lda(x_train, y_train)
  y.te.hat       <- predict(lda.mod, x_test)$class
  ind.err.te     <- ifelse(y.te!=y.te.hat,1,0)    # Random variable tracking error. Indicator
  test.err[r,1]  <- mean(ind.err.te)
}


#### 1NN

y.tr.hat1 <- knn(x_train, x_train, y_train, k=1)
conf.mat.tr1 <- table(y_train, y.tr.hat1)
conf.mat.tr1
#### Quadratic
qda.prostate <- qda(x_train, y_train)


###   ...# Not finished.

library(naivebayes)
naive.prospate <- naive_bayes(as.factor(y)~., data=prostate)
naive.prospate
