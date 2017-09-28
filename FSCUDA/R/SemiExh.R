################################################################################
# SemiExh impelement a feature selection method called semi-exhaustive #########
################################################################################

#remove duplicated feature sets.
RmDupVal <- function(features){
    features <- t(apply(features, 1, sort))
    features <- subset(features, duplicated(features) == F)
    return(features)
}

LRWithFixedVal <- function(para){
    x <- para$x
    y <- para$y
    n.comb <- para$n.comb
    error.threshhold <- para$error.threshhold
    fold <- para$fold
    device.id <- para$device.id
    fixed.features <- para$fixed.features
    
    result <- .Call("LRCUDAWithFixedVal", t(x), y, as.integer(n.comb), as.integer(error.threshhold), as.integer(fold), as.integer(device.id), t(fixed.features), nrow(fixed.features), ncol(fixed.features))

    #######################################################
    #output format should be consistent
    ######################################################
    result <- t(matrix(result, nrow = n.comb + ncol(fixed.features)+1 ))
    result <- as.data.frame(result)
    features.name <- paste0("feature", 1:(ncol(result)-1))
    names(result) <- c(features.name, "error")
    return(result)

}

LRWithFixedValMG <- function(x, y, n.comb = 1, error.threshhold = 0 , fold = 10, device.id = 0, cl = NULL, fixed.features){
    if(!is.matrix(x)){
        stop("x should be matrix type !")
    }
  
    if(nrow(x) != length(y)){
        stop("x'rows is different from y'length !")
    }
    x <- cbind(rep(1, nrow(x)), x)
    if(is.null(cl)){
        cl <- makeCluster(length(device.id), type = "SOCK")
    }else{
        if(length(cl) != length(device.id)){
            stop("device count should be equal to cluster size! Please check you configures.")
        }
    }
    task.num <- nrow(fixed.features)
    print(paste("task.num", task.num))
    ####################################################### 
    #when task.num is less than device.num
    #######################################################
    if (task.num < length(device.id)){
        device.id <- device.id[1:task.num]
        cl <- cl[1:task.num]
    }
    device.num <- length(device.id)

    clusterEvalQ(cl,library(LRCUDA))
    para <- vector("list", device.num)
    task.piece <- floor(task.num / device.num)
   
    for(i in 1:device.num){
        if(i != device.num){
            para[[i]] <- list(x = x, y = y, n.comb = n.comb, error.threshhold = error.threshhold, fold = fold, device.id = device.id[i], fixed.features = as.matrix(fixed.features[((i-1)*task.piece + 1):(i*task.piece),]))
            #print(n.comb)
            #print(error.threshhold)
            #print(fold)
            #print(device.id[i])
            #print(nrow(fixed.features[((i-1)*task.piece + 1):(i*task.piece),]))
            #print(ncol(fixed.features[((i-1)*task.piece + 1):(i*task.piece),]))
        }else{
            para[[i]] <- list(x = x, y = y, n.comb = n.comb, error.threshhold = error.threshhold, fold = fold, device.id = device.id[i], fixed.features = as.matrix(fixed.features[((i-1)*task.piece + 1):(nrow(fixed.features)),]))
            #print(n.comb)
            #print(error.threshhold)
            #print(fold)
            #print(device.id[i])
            #print(nrow(fixed.features[((i-1)*task.piece + 1):(nrow(fixed.features)),]))
            #print(ncol(fixed.features[((i-1)*task.piece + 1):(nrow(fixed.features)),]))
        }
        
    }
    
    result <- clusterApply(cl, para, LRWithFixedVal)
    result <- do.call("rbind", result)
    return(result)
}



SemiExh <- function(x, y,n.comb =  3 ,error.threshhold = 0, fold = 1, device.id = 0, cl = NULL){

    ############################################################################
    #one feature: error.threshhold is set to half of training set num.
    ############################################################################

    result.l <- list()
   # rand.index <- sample(1:length(y), length(y))
    result.one <- LRCUDA(x = x, y = y, n.comb = n.comb, error.threshhold = length(y) / 2, fold = fold, device.id = device.id, cl = cl)
    print(11111) 
    if(nrow(result.one) == 0){
         print("use one feature can't statisfy the error threshhold")
         return(result.l)
    }
    print(2222)
    result.l[[1]] <- result.one
    error.min.one <- min(result.one$error)
    print(paste("error.min.one", error.min.one))
    if(error.min.one <= error.threshhold){
        return(result.l)
    }else{
        #save(result.one, file = "result.one.feature.RData")
    }
    
    ############################################################################
    #two features: error.threshhold is set to error.min.one
    ############################################################################
 
    fixed.features <- result.one[,1]
    #rand.index <- sample(1:length(y), length(y))    
    result.two <- LRWithFixedValMG(x, y, n.comb = n.comb, error.threshhold = error.min.one, fold = fold, device.id = device.id , cl = cl, fixed.features = as.matrix(fixed.features))
    
    if(nrow(result.two) == 0){
        print("use two features can't satisfy the error theshhold")
        return(result.l)
    }
    result.l[[2]] <- result.two    
    error.min.two <- min(result.two$error)
    if(error.min.two <= error.threshhold){
        return(result.l)
    }
    #save(result.two, file = "result.two.features.RData")
    print(paste("error.min.two", error.min.two))
   
    ############################################################################
    #three or more features, max number of features in model is 9.
    ############################################################################

    fixed.features <- RmDupVal(result.two[,1:2])
    error.para <- error.min.two
    for(i in 3:9){
        print(paste("i = ", i))
        print(paste("fixed features num", nrow(fixed.features)))
        #rand.index <- sample(1:length(y), length(y))
        result <- LRWithFixedValMG(x, y, n.comb = n.comb, error.threshhold = error.para, fold = fold, device.id = device.id ,cl = cl, fixed.features = as.matrix(fixed.features))
        
        if(nrow(result) == 0){
            print(paste("use", i+1 ,  "feature can't statisfy the error threshhold"))
            return(result.l)
        }
        result.l[[i]] <- result
        error.min <- min(result$error)
        if(error.min <= error.threshhold){
            return(result.l)
        }
        else{
            #save(result, file = paste0("features", i, ".RData"))
        }
        fixed.features <- RmDupVal(result[,1:i])
        error.para <- error.min
        
    }
    return(result.l) 
}

#LRWithFixedValMG()
