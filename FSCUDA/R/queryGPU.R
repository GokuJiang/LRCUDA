#' get GPU count

#' @param cl The cluster of computers which you created. If it is NULL, the program will create clusters based on the number of devices automatically.
#' @examples
#' getGPUCount()
#' @export
getGPUCount <- function(cl = NULL){
    if(is.null(cl)){
        count <- vector("list", 1)
	count[[1]] <- .Call("getGPUCount")
        return(count)
    }else{
        clusterEvalQ(cl, library(LRCUDA))
    	count <- clusterEvalQ(cl, .Call("getGPUCount"))
        return(count)
    }
}


#' generate GPU ids from the number of GPUs

#' @param count The number of GPUs.
#' @examples
#' generateIds(3)
#' @export
generateIds <- function(count){
    return(0:(count-1))
}

#' get GPU ids

#' @param cl The cluster of computers which you created. If it is NULL, the program will create clusters based on the number of devices automatically.
#' @examples
#' getGPUIds()
#' @export
getGPUIds <- function(cl = NULL){
    count <- getGPUCount(cl)
    Ids <- lapply(count, generateIds)
    return(Ids)
}

#getGPUIds()
#getGPUIds()
