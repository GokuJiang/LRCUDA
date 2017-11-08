

#' Logistic Regression wiht CUDA

#' @param x Gene expression profile matrix. The columns represent different prob sets. The rows represent different samples. The values in the matrix represent gene expression levels.
#' @param y The label vector, positive(1) or negative(0)
#' @param n.comb The max number of features which are selected.
#' @param error.threshhold The threshhold of error.
#' @param fold The number of fold of cross validation used in the program.
#' @param device.id The GPU device ID.
#' @param cl The cluster of computers which you created. If it is NULL, the program will create clusters based on the number of devices automatically.
#' @export
LRCUDA <- function(x, y, n.comb = 2, error.threshhold = 0 , fold = 10, device.id = 0, cl = NULL){
    if(!is.matrix(x)){
        stop("x should be matrix type !")
    }


    if(nrow(x) != length(y)){
        stop("x'rows is different from y'length !")
    }


    task.num <- choose(ncol(x), n.comb)
    x <- cbind(rep(1,length(y)), x)
    device.num <- length(device.id)


    if(is.null(cl)){
        cl <- makeCluster(length(device.id), type = "SOCK")
    
    }else{
        if(length(cl) != length(device.id)){
            stop("device count should be equal to cluster size! Please check you configures.")
        }
    }

    registerDoParallel(cl)

    clusterEvalQ(cl,library('FSCUDA'))

    print('L7')

    para <- vector("list", device.num)
    task.piece <- floor(task.num / device.num)

    for(i in 1:device.num){
        para[[i]] <- list(x = x, y = y, n.comb = n.comb, error.threshhold = error.threshhold, fold = fold, device.id = device.id[i], start = (i-1)*task.piece + 1, stop = i*task.piece)
    }

    if(para[[device.num]]$stop < task.num){
            para[[device.num]]$stop = task.num
    }

    result <- clusterApply(cl, para, LRMultipleGPU)
    
    #result <- clusterApply(cl, para, LRSingleGPU)
    
    result <- combineResult(result)
    print(result)

    return(result)
}
