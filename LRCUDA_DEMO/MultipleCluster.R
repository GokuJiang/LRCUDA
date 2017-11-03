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


colon<-read.table('Colon.csv',sep=",",as.is=FALSE)


shape <- dim(colon)

row.n <- shape[1]
colon.n <- shape[2]

gpu.ids <- getGPUIds() 

x <- colon[2:row.n,2:colon.n]


x <- matrix(as.numeric(unlist(x)),ncol = row.n-1,byrow=TRUE)


y <- colon[1:1,2:colon.n]

y <- unlist(lapply(y,function(i) binaryzation(i))) 


print(gpu.ids[[1]])
result <- SemiExh(x, y,n.comb = 4, error.threshhold = 40, device.id = 0, cl =NULL)

