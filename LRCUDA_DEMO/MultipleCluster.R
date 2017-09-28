library("FSCUDA")
library("caret")
library("snow")
load("Colon.Rdata")



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
y <- mat[2:63,1]
y <- unlist(lapply(y, function(i) binaryzation(i)))
x = matrix(as.numeric(mat[2:63,2:2001]),ncol=2000,byrow=TRUE)



#result <- LRCUDA(x,y,error.threshhold = 62,n.comb =3, device.id = c(0,1,2,3,0,1,2,3,0,1,2,3))

#result <- SemiExh(x, y,n.comb = 6, error.threshhold = 40, device.id = c(0,1,2,3,0,1,2,3,0,1,2,3), cl = NULL)

#save(result, file = "MultipleClusterResult.RData")

#print(result)


