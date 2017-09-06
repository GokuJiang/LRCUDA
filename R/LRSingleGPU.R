LRSingleGPU <- function(x, y, n.comb = 2, error.threshhold = 0 , fold = 10, device.id, start, stop){

    result <- .Call("LRCUDA", t(x), y, as.integer(n.comb), as.integer(error.threshhold), as.integer(fold), as.integer(device.id), as.integer(start), as.integer(stop))
    return(matrix(result, ncol = n.comb + 1))
}


