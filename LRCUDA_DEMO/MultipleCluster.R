library("caret")
library("snow")
library("stringr")
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


colon<-read.csv('Colon.csv',header=TRUE)

shape <- dim(colon)

row.n <- shape[1] #2000
column.n <- shape[2] #62

gpu.ids <- getGPUIds()
print(row.n)
print(column.n)

x <- colon[1:row.n,2:column.n]
print(dim(x))

x <- matrix(as.numeric(unlist(x)),ncol = row.n,byrow=TRUE)

#y <- colon[2:2,2:colon.n]
#y <- lapply(y,function(i) binaryzation(i))
#y <- matrix(unlist(y),ncol = 1,byrow=TRUE)
write.csv(x, 'Colon_X.csv')
write.csv(y, 'Colon_y.csv')

#result <- SemiExh(x, y,n.comb = 3, error.threshhold = 50, device.id = gpu.ids, cl =NULL)
#print(result)
