
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
     x <- para$x
     y <- para$y
     n.comb <- para$n.comb
     error.threshhold <- para$error.threshhold
     fold <- para$fold
     device.id <- para$device.id
     start <- para$start
     stop <- para$stop    
    result <- .Call("LRCUDA", t(x), y, as.integer(n.comb), as.integer(error.threshhold), as.integer(fold), as.integer(device.id), as.integer(start), as.integer(stop))
    return(matrix(result, ncol = n.comb + 1))
}

	
