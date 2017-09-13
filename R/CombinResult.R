#combine analysis result from cluster.
#store the result as data frame type.

@export
combineResult <- function(result){
    
    #result is a list
    result <- do.call(rbind, result)
    result <- as.data.frame(result)
    features.num <- ncol(result) - 1
    header.names <- paste("feature", 1:features.num, sep=".")
    header.names <- c(header.names, "error")
    #print(ncol(result))  #3
    #print(length(header.names))  #4
    names(result) <- header.names
    return(result)
    
}

#rec <- list(name="李明", age=30, scores=c(85, 76, 90)) 
#combineResult(rec)

