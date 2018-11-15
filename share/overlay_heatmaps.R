overlay <- function(fpr, fnr, ppv, thresh=.01){
  par(mfrow=c(1,1))
  
  stopifnot(dim(fpr)==dim(fnr))
  stopifnot(dim(fnr)==dim(ppv))
  ppv[is.na(ppv)]=1
  
  mat = matrix(0, nrow=nrow(fpr), ncol=ncol(fpr))
  mat[abs(fpr)<=thresh]=mat[abs(fpr)<=thresh]+.4
  mat[abs(fnr)<=thresh]=mat[abs(fnr)<=thresh]+.3
  mat[abs(ppv)<=thresh]=mat[abs(ppv)<=thresh]+.2
  image(mat, col = grey(seq(1, 0, length = 256)))
}