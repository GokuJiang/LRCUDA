dyn.load("~/LRCUDA/src/libLRCUDA.so")
LRSingleGPU <- function(x, y, n.comb = 2, error.threshhold = 0 , fold = 10, device.id = 0, start, stop){

    result <- .Call("LRCUDA", t(x), y, as.integer(n.comb), as.integer(error.threshhold), as.integer(fold), as.integer(device.id), as.integer(start), as.integer(stop))
    return(matrix(result, ncol = n.comb + 1))
}


X=matrix(1:12,ncol=3,nrow=4)
Y=c(1,3,4,5)
LRSingleGPU(X,Y,start=0,stop=2)
