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
  require(foreach)
  require(doParallel)
  
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




