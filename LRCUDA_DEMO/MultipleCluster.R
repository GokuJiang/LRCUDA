library("caret")
library("snow")
library("stringr")
load("Colon.Rdata")
library("FSCUDA")

binaryzation <- function(str) {
    
    flag <- 0
    
    if (str == "TumorTissue") {
        flag <- 1
    }else {
        flag <- 0
    }
    return (flag)
}

mat <- matrix(unlist(data), ncol=2001, byrow=TRUE)
inTraining <- createFolds(mat[2:63,1], 5, TRUE)
gpu.ids = getGPUIds() 

i <- 0
for (fold in inTraining) {
    dataSet <- mat[fold,]
    shape <- dim(dataSet)
    row.n <- shape[1]
    colon.n <- shape[2]
    x <- matrix(as.numeric(dataSet[2:shape[1],2:shape[2]]),ncol = colon.n - 1,byrow = TRUE)
    y <- mat[2:row.n,1]
    y <- unlist(lapply(y,function(i) binaryzation(i)))
    result <- SemiExh(x, y,n.comb = 5, error.threshhold = 40, device.id = gpu.ids, cl =NULL)
    print(result)
    i <- i+1  
}

