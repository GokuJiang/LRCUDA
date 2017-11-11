library("caret")
library("snow")
library("stringr")
library("FSCUDA")

binaryzation <- function(str) {

    flag <- 0

    if (str == "Tumor tissue") {
        flag <- 1
    }else {
        flag <- 0
    }
    return (flag)
}


colon<-read.csv('Colon.csv',header=FALSE)

shape <- dim(colon)

row.n <- shape[1] #2000
column.n <- shape[2] #62

gpu.ids <- getGPUIds()

x <- colon[1:row.n,2:column.n]
x <- matrix(as.numeric(unlist(x)),ncol = row.n,byrow=TRUE)

y <- colon[1,2:column.n]
y <- lapply(y,function(i) binaryzation(i))
y <- as.numeric(unlist(y))

result <- iFes(x, y, 2)

write.csv(result, file = "Result.csv",row.names=FALSE)

