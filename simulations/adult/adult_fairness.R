setwd("~/Desktop/fairness")

library("dplyr")
library("randomForest")
library(fields)
par(mfrow=c(1,3))

# setup and model
adult = read.csv("adult.txt")
adult <- transform(adult,
                   Male = as.factor(as.integer(Male==levels(adult$Male)[2])),
                   X50K = as.factor(as.integer(X50K==levels(adult$X50K)[2])))

rf <- randomForest(X50K~State.gov+Bachelors+Never.married+Adm.clerical+White+Male, 
                   data=adult)

probs <- predict(rf, type="prob")[,2]


# lmod = glm(X50K~State.gov+Bachelors+Never.married+Adm.clerical+White+Male, data=adult, family="binomial")
# probs = predict(lmod,type="response")

FPR <- function(y, yhat){
  sum(yhat==1 & y==0)/sum(y==0)
}

FNR <- function(y, yhat){
  sum(yhat==0 & y==1)/sum(y==1)
}

PPV <- function(y, yhat){
  sum(yhat==1 & y==1)/sum(yhat==1)
}

get_metrics <- function(cf, cm){
  y = adult$X50K
  yhat = rep(0,length(y))
  yhat[adult$Male==1] = probs[adult$Male==1]>cm
  yhat[adult$Male==0] = probs[adult$Male==0]>cf
  
  y_w = y[adult$Male==1]
  y_b = y[adult$Male==0]
  yhat_w = yhat[adult$Male==1]
  yhat_b = yhat[adult$Male==0]
  
  metrics = list(fpr = FPR(y_w,yhat_w)-FPR(y_b,yhat_b),
                 fnr = FNR(y_w,yhat_w)-FNR(y_b,yhat_b),
                 ppv = PPV(y_w,yhat_w)-PPV(y_b,yhat_b))
  return(metrics)
}

cutoffs = seq(0,1,length=100)
N = length(cutoffs)

fpr = outer(cutoffs,cutoffs, Vectorize(function(x,y){get_metrics(x,y)$fpr}))
fnr = outer(cutoffs,cutoffs, Vectorize(function(x,y){get_metrics(x,y)$fnr}))
ppv = outer(cutoffs,cutoffs, Vectorize(function(x,y){get_metrics(x,y)$ppv}))


par(mfrow=c(1,3))
image.plot(abs(fpr), xlab="Female",ylab="Male", main="FPR")
abline(0,1)
image.plot(abs(fnr), xlab="Female",ylab="Male",main="FNR")
abline(0,1)
image.plot(abs(ppv), xlab="Female",ylab="Male",main="PPV")
abline(0,1)
