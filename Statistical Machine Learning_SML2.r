############ Exercise 3 #############
mnist_train <- read_csv("https://pjreddie.com/media/files/mnist_train.csv", col_names = FALSE)
mnist_test <- read_csv("https://pjreddie.com/media/files/mnist_test.csv", col_names = FALSE)
library(readr)
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
################### Question 1 ###############
mnist_train <- mnist_train
mnist_test <- mnist_test

#m.tr = as.data.frame(filter(mnist_train, X1 == c(1,7)))
#m.te =as.data.frame(filter(mnist_test, X1 == c(1,7)))
getwd()
#### selection
m.tr = mnist_train%>%filter(X1==1 | X1==7)
m.tr = mnist_train[mnist_train$X1==1 | mnist_train$X1==7,]
dim(m.tr)
dim(m.te)
n <- nrow(m.tr) + nrow(m.te)
p <- ncol(m.tr)
pos <-  1

# Split the data
x_train = m.tr[,2:p]
y_train = m.tr[,1, drop=TRUE]
x_test = m.te[,2:p]
y_test = m.te[,1,drop=TRUE]
dim(y_train)
dim(x_train)

#### Test confusion matrix
y.te.hat9 <- knn(x_train,x_test,y_train, k=9) 
y.te.hat7 <- knn(x_train,x_test,y_train, k=7) # Predicted responses in test set
y.te.hat1 <- knn(x_train,x_test,y_train, k=1) # Predicted responses in test set
conf.mat.te7 <- table(y_test, y.te.hat7)
conf.mat.te1 <- table(y_test, y.te.hat1)
conf.mat.te9 <- table(y_test, y.te.hat9)
conf.mat.te7
conf.mat.te1
conf.mat.te9
libray(xtable)
xtable(conf.mat.te9)
#### Train confusion matrix
y.tr.hat9 <- knn(x_train,x_train,y_train, k=9)
y.tr.hat7 <- knn(x_train,x_train,y_train, k=7) # Predicted responses in test set
y.tr.hat1 <- knn(x_train,x_train,y_train, k=1) # Predicted responses in test set

conf.mat.tr7 <- table(y_train, y.tr.hat7)
conf.mat.tr1 <- table(y_train, y.tr.hat1)
conf.mat.tr9 <- table(y_train, y.tr.hat9)
conf.mat.tr7
conf.mat.tr1
conf.mat.tr9
xtable(conf.mat.tr1)

##########################   Question 2  ###################################

library(ROCR) #### ROC for Training

y.roc <- ifelse(y=='pos',1,0)

kNN.mod <- class::knn(x_train, x_train, y_train, k=1, prob=TRUE)
prob    <- attr(kNN.mod, 'prob')
prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1

pred.1NN <- prediction(prob, y_train)
perf.1NN <- performance(pred.1NN, measure='tpr', x.measure='fpr')

kNN.mod <- class::knn(x_train, x_train, y_train, k=7, prob=TRUE)
prob    <- attr(kNN.mod, 'prob')
prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1

pred.7NN <- prediction(prob, y_train)
perf.7NN <- performance(pred.7NN, measure='tpr', x.measure='fpr')


kNN.mod <- class::knn(x_train, x_train, y_train, k=5, prob=TRUE)
prob    <- attr(kNN.mod, 'prob')
prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1

pred.9NN <- prediction(prob, y_train)
perf.9NN <- performance(pred.5NN, measure='tpr', x.measure='fpr')

plot(perf.1NN, col=2, lwd= 2, lty=2, main=paste('Comparative ROC curves in Training'))
plot(perf.7NN, col=3, lwd= 2, lty=3, add=TRUE)
plot(perf.9NN, col=4, lwd= 2, lty=4, add=TRUE)
abline(a=0,b=1)
legend('bottomright', inset=0.05, c('1NN','7NN','9NN'),  col=2:4, lty=2:4)


########### ROC For Testing 
y.roc <- ifelse(y=='pos',1,0)
kNN.mod <- class::knn(x_train, x_test, y_train, k=1, prob=TRUE)
prob    <- attr(kNN.mod, 'prob')
prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1

pred.1NN <- prediction(prob, x_test)
perf.1NN <- performance(pred.1NN, measure='tpr', x.measure='fpr')

kNN.mod <- class::knn(x_train, x_test, y_train, k=7, prob=TRUE)
prob    <- attr(kNN.mod, 'prob')
prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1

pred.7NN <- prediction(prob, x_test)
perf.7NN <- performance(pred.7NN, measure='tpr', x.measure='fpr')


kNN.mod <- class::knn(x_train, x_test, y_train, k=5, prob=TRUE)
prob    <- attr(kNN.mod, 'prob')
prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1

pred.9NN <- prediction(prob, x_test)
perf.9NN <- performance(pred.5NN, measure='tpr', x.measure='fpr')

plot(perf.1NN, col=2, lwd= 2, lty=2, main=paste('Comparison of Predictive ROC curves'))
plot(perf.7NN, col=3, lwd= 2, lty=3, add=TRUE)
plot(perf.9NN, col=4, lwd= 2, lty=4, add=TRUE)
abline(a=0,b=1)
legend('bottomright', inset=0.05, c('1NN','7NN','9NN'),  col=2:4, lty=2:4)


################# Question 3 ##########################


#In the Report

################## Question 4 ###########################
# In the report


################ Question 5 ######################
f_b = rbind(m.te,m.tr)
pca = prcomp(f_b)
summary(pca)
autoplot(pca)
autoplot(pca,data = f_b) + theme_minimal()

############### Question 6 ####################

pca1 = pca$x[,1:2]
dff_tr_pca = head(dff_pca, n_tr)
dff_te_pca = tail(dff_pca, n_te)

ypca = knn(dff_tr_pca, dff_te_pca, y_tr, k =7, prob = TRUE)
prob    <- attr(ypca, 'prob')
prob    <- 2*ifelse(ypca == "1", 1-prob, prob) - 1

pred.pca <- prediction(prob, y_te)
perf.pca <- performance(pred.pca, measure='tpr', x.measure='fpr')

plot(perf.pca, col=2, lwd= 2, lty=2, main=paste('Comparison of ROC curves Between PCA and KNN at K=7'))
plot(perf.7NN, col=3, lwd= 2, lty=3, add=TRUE)
abline(a=0,b=1)
legend('bottomright', inset=0.05, c('PCA','7NN'),  col=2:3, lty=2:3)







####  Exercise (Bonus): (20 points)
# Question 1
prostate <- read.csv(file.choose())
View(prostate)

n   <- nrow(prostate)       # Sample size
p   <- ncol(prostate)-1  # Dimensionality of the input space
pos <- p+1            # Position of the response
x   <- prostate[,-pos]      # Data matrix: n x p matrix
y   <- prostate[, pos]      # Response vector
n; p;pos
dim(prostate)
# Split the data
nte     <- prostate  # Number of observations in the test set
ntr     <- prostate    # Number of observations in the train set


x_train = ntr[,2:p]
y_train = ntr[,1, drop=TRUE]
x_test = nte[,2:p]
y_test = nte[,1,drop=TRUE]


## Predictive performance of the 1,3,5,7-Nearest Neighbors Classifier

y.te.hat1 <- knn(x_train, x_test, y_train, k=1) 
y.te.hat3 <- knn(x_train, x_test, y_train, k=3)
y.te.hat5 <- knn(x_train, x_test, y_train, k=5)
y.te.hat7 <- knn(x_train, x_test, y_train, k=7)
### Confusion matrix
conf.mat.te1 <- table(y_test, y.te.hat1)
conf.mat.te3 <- table(y_test, y.te.hat3)
conf.mat.te5 <- table(y_test, y.te.hat5)
conf.mat.te7 <- table(y_test, y.te.hat7)
xtable(conf.mat.te1)
xtable(conf.mat.te3)
xtable(conf.mat.te5)
xtable(conf.mat.te7)


########### ROC #####
##Comparative ROC Curves on the training set for k = 1,3,5,7
 
library(ROCR)

y.roc <- ifelse(y=='pos',1,0)

kNN.mod <- class::knn(x_train, x_train, y_train, k=1, prob=TRUE)
prob    <- attr(kNN.mod, 'prob')
prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1

pred.1NN <- prediction(prob, y_train)
perf.1NN <- performance(pred.1NN, measure='tpr', x.measure='fpr')

kNN.mod <- class::knn(x_train, x_train, y_train, k=3, prob=TRUE)
prob    <- attr(kNN.mod, 'prob')
prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1

pred.3NN <- prediction(prob, y_train)
perf.3NN <- performance(pred.3NN, measure='tpr', x.measure='fpr')

kNN.mod <- class::knn(x_train, x_train, y_train, k=5, prob=TRUE)
prob    <- attr(kNN.mod, 'prob')
prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1

pred.5NN <- prediction(prob, y_train)
perf.5NN <- performance(pred.5NN, measure='tpr', x.measure='fpr')

kNN.mod <- class::knn(x_train, x_train, y_train, k=7, prob=TRUE)
prob    <- attr(kNN.mod, 'prob')
prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1

pred.7NN <- prediction(prob, y_train)
perf.7NN <- performance(pred.7NN, measure='tpr', x.measure='fpr')

plot(perf.1NN, col=2, lwd= 2, lty=2, main=paste('Comparative ROC curves in Training'))
plot(perf.3NN, col=3, lwd= 2, lty=3, add=TRUE)
plot(perf.5NN, col=4, lwd= 2, lty=4, add=TRUE)
plot(perf.7NN, col=5, lwd= 2, lty=5, add=TRUE)
abline(a=0,b=1)
legend('bottomright', inset=0.05, c('1NN','3NN', '5NN','7NN'),  col=2:5, lty=2:5)



#######Comparative ROC Curves on the test set for k = 1,3,5,7###############

library(ROCR)

y.roc <- ifelse(y=='pos',1,0)

kNN.mod <- class::knn(x_train, x_test, y_train, k=1, prob=TRUE)
prob    <- attr(kNN.mod, 'prob')
prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1

pred.1NN <- prediction(prob, x_test)
perf.1NN <- performance(pred.1NN, measure='tpr', x.measure='fpr')

kNN.mod <- class::knn(x_train, x_test, y_train, k=3, prob=TRUE)
prob    <- attr(kNN.mod, 'prob')
prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1

pred.3NN <- prediction(prob, x_test)
perf.3NN <- performance(pred.3NN, measure='tpr', x.measure='fpr')

kNN.mod <- class::knn(x_train, x_train, x_test, k=5, prob=TRUE)
prob    <- attr(kNN.mod, 'prob')
prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1

pred.5NN <- prediction(prob, x_test)
perf.5NN <- performance(pred.5NN, measure='tpr', x.measure='fpr')

kNN.mod <- class::knn(x_train, x_test, y_train, k=7, prob=TRUE)
prob    <- attr(kNN.mod, 'prob')
prob    <- 2*ifelse(kNN.mod == "0", 1-prob, prob) - 1

pred.7NN <- prediction(prob, x_test)
perf.7NN <- performance(pred.7NN, measure='tpr', x.measure='fpr')

plot(perf.1NN, col=2, lwd= 2, lty=2, main=paste('Comparison of Predictive ROC curves'))
plot(perf.3NN, col=3, lwd= 2, lty=3, add=TRUE)
plot(perf.5NN, col=4, lwd= 2, lty=4, add=TRUE)
plot(perf.7NN, col=5, lwd= 2, lty=5, add=TRUE)
abline(a=0,b=1)
legend('bottomright', inset=0.05, c('1NN','3NN', '5NN','7NN'),  col=2:5, lty=2:5)



##################### Question 2   (Comment on question 1)
#In the  report

################### Question 3 Predictive performance using stochastic hold out

set.seed (19671210)          # Set seed for random number generation to be reproducible
n = nrow(prostate)
epsilon <- 1/3               # Proportion of observations in the test set
nte     <- round(n*epsilon)  # Number of observations in the test set
ntr     <- n - nte
n
R <- 100   # Number of replications
test.err <- matrix(0, nrow=R, ncol=4)

for(r in 1:R)
{
  # Split the data
  
  id.tr   <- sample(sample(sample(n)))[1:ntr]                   # For a sample of ntr indices from {1,2,..,n}
  id.te   <- setdiff(1:n, id.tr)
  
  y.te         <- y[id.te]                                        # True responses in test set
  
  # First machine: 1NN
  
  y.te.hat     <- knn(x[id.tr,], x[id.te,], y[id.tr], k=1)        # Predicted responses in test set
  ind.err.te   <- ifelse(y.te!=y.te.hat,1,0)                      # Random variable tracking error. Indicator
  test.err[r,1]  <- mean(ind.err.te)
  
  # Second machine: 3NN
  y.te.hat     <- knn(x[id.tr,], x[id.te,], y[id.tr], k=3) # Predicted responses in test set
  ind.err.te   <- ifelse(y.te!=y.te.hat,1,0)
  test.err[r,2]  <- mean(ind.err.te)
  
  # Third machine: 5NN
  y.te.hat     <- knn(x[id.tr,], x[id.te,], y[id.tr], k=5)       # Predicted responses in test set
  ind.err.te   <- ifelse(y.te!=y.te.hat,1,0)                      # Random variable tracking error. Indicator
  test.err[r,3]  <- mean(ind.err.te)
  
  
  # Fourth machine: 7NN
  y.te.hat     <- knn(x[id.tr,], x[id.te,], y[id.tr], k=7)       # Predicted responses in test set
  ind.err.te   <- ifelse(y.te!=y.te.hat,1,0)                      # Random variable tracking error. Indicator
  test.err[r,4]  <- mean(ind.err.te)
}  

test <- data.frame(test.err)
Method<-c('1NN', '3NN', '5NN', '7NN')
colnames(test) <- Method
boxplot(test)

###########################################################

