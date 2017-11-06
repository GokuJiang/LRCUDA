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


colon<-read.csv('Colon.csv')

shape <- dim(colon)

row.n <- shape[1]
colon.n <- shape[2]

gpu.ids <- getGPUIds()
print(shape)
x <- colon[1:row.n,2:colon.n]
print(dim(x))
x <- matrix(as.numeric(unlist(x)),ncol = row.n,byrow=TRUE)

y <- colon[1,2:colon.n]
y <- lapply(y,function(i) binaryzation(i))
y <- matrix(unlist(y),ncol = 1,byrow=TRUE)
write.csv(x, 'Colon_X.csv')
write.csv(y, 'Colon_y.csv')

result <- SemiExh(x, y,n.comb = 3, error.threshhold = 50, device.id = gpu.ids, cl =NULL)
print(result)
