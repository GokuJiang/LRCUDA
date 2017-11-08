#'Run LRCUDA with multiple GPU

#' @param para paramters with LRCUDA
#' @export

LRMultipleGPU <- function(para){
    x <- t(para$x)
    print(x)
    y <- para$y
    print(y)
    n.comb <- as.integer(para$n.comb)
    print(n.comb)
    error.threshhold <- as.integer(para$error.threshhold)
    print(error.threshhold)
    fold <- as.integer(para$fold)
    print(fold)
    device.id <- as.integer(para$device.id)
    print(device.id)
    start <- as.integer(para$start)
    print(start)
    stop <- as.integer(para$start)
    print(stop)
    
     result <- .Call("LRCUDA", x, y, n.comb, error.threshhold, fold, device.id, start, stop)
     print(result)
     return(t(matrix(result, nrow = n.comb + 1)))
     
}
