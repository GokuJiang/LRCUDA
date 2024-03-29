
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
        #outfile = c("~/goku_LRCUDA_Floder/log1.log","~/goku_LRCUDA_Floder/log2.log","~/goku_LRCUDA_Floder/log3.log","~/goku_LRCUDA_Floder/log4.log")
	cl <- makeCluster(length(device.id), type = "SOCK")
        
    }else{
        if(length(cl) != length(device.id)){
            stop("device count should be equal to cluster size! Please check you configures.")
        }
    }
    registerDoParallel(cl)
    clusterEvalQ(cl,library("FSCUDA"))
    para <- vector("list", device.num)
    task.piece <- floor(task.num / device.num)	
    for(i in 1:device.num){
        para[[i]] <- list(x = x, y = y, n.comb = n.comb, error.threshhold = error.threshhold, fold = fold, device.id = device.id[i], start = (i-1)*task.piece + 1, stop = i*task.piece)
    }
    if(para[[device.num]]$stop < task.num){
            para[[device.num]]$stop = task.num
    }
    print(cl)
    result <- clusterApply(cl, para, LRMultipleGPU)
    return(combineResult(result))
}






