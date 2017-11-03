#cv <- function(y, fold){
#  lables <- unique(y)
#  groups <- list()
#  for(lable in lables){
#    groups[[as.character(lable)]] <- which(y == lable)
#  }
#}



#' loo cross valiation

#' @param features The number of features
#' @param X Gene expression profile matrix. The columns represent different prob sets. The rows represent different samples. The values in the matrix represent gene expression levels.
#' @param Y The label vector, positive(1) or negative(0)
#' @examples
#' X = matrix(1:12, 4, 3)
#' Y = c(1,0,1,1)
#' result <- LOOCV(3,X,Y)
#' print(result)
#' @export
LOOCV <- function(features, X, Y){
  X <- X[,features]
  data <- as.data.frame(cbind(X,Y))
  names(data)[ncol(data)] <- "label"
  error <- 0
  for(i in 1:nrow(data)){
    model.lr <- glm(label ~ ., data = data[-i,], family = binomial(logit))
    pr.lr <- predict(model.lr,data[i,], type = "response")
    pr.lr <- ifelse(pr.lr >= 0.5, 1, 0)
    error <- error + length(which(pr.lr != data[i, ncol(data)]))
  }
  return(error / length(Y))
}

#' k flod cross valiation

#' @param y The label vector, positive(1) or negative(0)
#' @param fold The number of fold of cross validation used in the program.
#' @examples
#' result <- CV(c(1,0,1,1,1,1,1,1,0,0,0,1,0,0,0,1,0),4)
#' print(result)
#' @export
CV <- function(y, fold = 10){
  zero.index <- which(y == 0)
  zero.index <- sample(zero.index, length(zero.index))
  one.index <- which(y == 1)
  one.index <- sample(one.index, length(one.index))
  
  cv.zero.test <- split(zero.index, 1:fold)
  cv.one.test <- split(one.index, 1:fold)
  cv.test <- list()
  for(i in 1:fold){
    cv.test[[i]] <- c(cv.zero.test[[i]], cv.one.test[[i]])
  }
  
  cv.train <- list()
  for(i in 1:fold){
    cv.train[[i]] <- setdiff(1:length(y), cv.test[[i]])
  }
  cv <- list()
  cv$test <- cv.test
  cv$train <- cv.train
  return(cv)
}



KFold <- function(features, X, Y, fold, n){
  X <- as.matrix(X[,features])
  data <- as.data.frame(cbind(X,Y))
  names(data)[ncol(data)] <- "label"
  error <- numeric(n)
  for(t in 1:n){
    set.seed(t)
    cv <- CV(Y, fold)
    predY <- numeric(length(Y))
    for(i in 1:fold){
      model <- glm(label ~ ., data = data[cv$train[[i]],], family = binomial(logit))
      pred <- c()
      if(length(cv$test[[i]]) != 0){
        pred <- predict(model, data[cv$test[[i]],], type = "response")
      }
      predY[cv$test[[i]]] <- pred
    }
    predY <- ifelse(predY >= 0.5, 1, 0)
    error[t] <- length(which(predY != Y)) / length(Y)
  }
  return(c(mean(error),sd(error)))
}


rm.bias <- function(result, data, fold, times, count){
  
  cl <- makeCluster(count , type = "SOCK")
  registerDoParallel(cl)
  
  for(i in 1:length(result)){
    features.set <- as.matrix(result[[i]][,1:(ncol(result[[i]])-1)])
    LOOCV.result <- foreach(j = 1:nrow(features.set), .combine='c') %dopar%{
      LOOCV(features.set[j,], data$x, data$y)
    }
    result[[i]] <- cbind(result[[i]], loocv = LOOCV.result)
    
    KFold.result <- foreach(j = 1:nrow(features.set)) %dopar%{
      KFold(features.set[j,], data$x, data$y ,fold, times)
    }
    KFold.result <- do.call("rbind", KFold.result)
    colnames(KFold.result) <- c("mean-10-fold", "sd-10-fold")
    result[[i]] <- cbind(result[[i]], KFold.result)
  }
  stopCluster(cl)
  return(result)
  
}




