
#'Run LRCUDA with multiple GPU

#' @param para paramters with LRCUDA
#' @export

LRMultipleGPU <- function(para){
     x <- para$x
     y <- para$y
     n.comb <- para$n.comb
     error.threshhold <- para$error.threshhold
     fold <- para$fold
     device.id <- para$device.id
     start <- para$start
     stop <- para$stop
     result <- .Call("LRCUDA", t(x), y, as.integer(n.comb), as.integer(error.threshhold), as.integer(fold), as.integer(device.id), as.integer(start), as.integer(stop))
     print(result)
     return(t(matrix(result, nrow = n.comb + 1)))
     
}
