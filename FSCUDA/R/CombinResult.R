#combine analysis result from cluster.
#store the result as data frame type.

#' Result
#' @param result result of LRCUDA
#' @export
combineResult <- function(result){

    #result is a list
    result <- do.call(rbind, result)
    result <- as.data.frame(result)

    print(result)
    features.num <- ncol(result) - 1
    header.names <- paste("feature", 1:features.num, sep=".")
    header.names <- c(header.names, "error")
    names(result) <- header.names
    return(result)

}
