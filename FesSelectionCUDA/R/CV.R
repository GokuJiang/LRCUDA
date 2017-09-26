####  author:Qinghan Meng
####  email:qinghan.meng@gmail.com
#cv function is to generate training sets and test sets for k-fold validation 

cv <- function(y, fold){
  lables <- unique(y)
  groups <- list()
  for(lable in lables){
    group[[as.character(lable)]] <- which(y == lable)
  }
  
}
