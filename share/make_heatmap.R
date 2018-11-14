library(fields)

#Helper functions to compute accuracy metrics
FPR <- function(y, yhat){
  sum(yhat==1 & y==0)/sum(y==0)
}
FNR <- function(y, yhat){
  sum(yhat==0 & y==1)/sum(y==1)
}
PPV <- function(y, yhat){
  sum(yhat==1 & y==1)/sum(yhat==1)
}

#Compute accuracy metrics given true labels,
#predicted probabilities, a sensitive attribute,
#and cutoffs for groups a=1 and a=0
get_metrics <- function(y, probs, a, c1, c0){
  yhat = rep(0,length(y))
  yhat[a==1] = probs[a==1]>c1
  yhat[a==0] = probs[a==0]>c0
  
  y_1 = y[a==1]
  y_0 = y[a==0]
  yhat_1 = yhat[a==1]
  yhat_0 = yhat[a==0]
  
  metrics = list(fpr = FPR(y_1,yhat_1)-FPR(y_0,yhat_0),
                 fnr = FNR(y_1,yhat_1)-FNR(y_0,yhat_0),
                 ppv = PPV(y_1,yhat_1)-PPV(y_0,yhat_0))
  return(metrics)
}

#Make heatmap of differnces in accuracy metrics
#for a grid of cutoff values 
make_heatmap <- function(y, probs, a, cutoffs=seq(0,1,length=100)){
  par(mfrow=c(1,3))
  
  fpr = outer(cutoffs,cutoffs, Vectorize(function(c1, c0){get_metrics(y, probs, a, c1, c0)$fpr}))
  fnr = outer(cutoffs,cutoffs, Vectorize(function(c1, c0){get_metrics(y, probs, a, c1, c0)$fnr}))
  ppv = outer(cutoffs,cutoffs, Vectorize(function(c1, c0){get_metrics(y, probs, a, c1, c0)$ppv}))
  
  image.plot(abs(fpr), xlab="a=0",ylab="a=1", main="FPR")
  abline(0,1)
  image.plot(abs(fnr), xlab="a=0",ylab="a=1",main="FNR")
  abline(0,1)
  image.plot(abs(ppv), xlab="a=0",ylab="a=1",main="PPV")
  abline(0,1)
}
