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
y <- as.integer(unlist(y))

print(typeof(x))
print(typeof(y))
print(typeof(error.threshhold))
print(typeof(device.id))

result <- SemiExh(x, y,n.comb = 3, error.threshhold = 50, device.id = gpu.ids[1], cl=NULL)
print(result)
