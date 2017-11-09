
#' run LRCUDA with single GPU

#' @param x Gene expression profile matrix. The columns represent different prob sets. The rows represent different samples. The values in the matrix represent gene expression levels.
#' @param y The label vector, positive(1) or negative(0)
#' @param n.comb The max number of features which are selected.
#' @param error.threshhold The threshhold of error.
#' @param fold The number of fold of cross validation used in the program.
#' @param device.id The GPU device ID. 
#' @param start   
#' @param stop 
#' @export
LRSingleGPU <- function(para){
    x <- t(para$x)
    y <- para$y
    n.comb <- as.integer(para$n.comb)
    error.threshhold <- as.integer(para$error.threshhold)
    fold <- as.integer(para$fold)
    device.id <- as.integer(para$device.id)
    start <- as.integer(para$start)
    stop <- as.integer(para$start)
  
    result <- .Call("LRCUDA", x, y, n.comb, error.threshhold, fold, device.id, start, stop)
    return(matrix(result, ncol = n.comb + 1))
}

	
