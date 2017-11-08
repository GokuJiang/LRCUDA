#'Run LRCUDA with multiple GPU

#' @param para paramters with LRCUDA
#' @export

LRMultipleGPU <- function(para){
    x <- t(para$x)
    y <- para$y
    n.comb <- as.integer(para$n.comb)
    error.threshhold <- as.integer(para$error.threshhold)
    fold <- as.integer(para$fold)
    device.id <- as.integer(para$device.id)
    start <- as.integer(para$start)
    stop <- as.integer(para$start)
    
    result <- .Call("LRCUDA", x, y, n.comb, error.threshhold, fold, device.id, start, stop)
    print('LM1') 
    print(result)
    result <- t(matrix(result, nrow = n.comb+1))
    print(result)
    return(result)
     
}
