library("LRCUDA")
library("snow")
load("~/LRCUDA_DEMO/Colon.Rdata")
#l <- makeCluster(c("c0302","c0302", "c0302", "c0302", "c0305", "c0305", "c0305", "c0305", "c0306", "c0306", "c0306", "c0306"), type = "SOCK")
#print(cl)

binaryzation <- function(str) {
    print(str)
    flag <- 0
    if (str == "TumorTissue"){
	flag <- 1
    }else{
        flag <- 0
    }
    return flag
}

a <- binaryzation("TumorTissue")
print(a)
mat <- matrix(unlist(data), ncol=2001, byrow=TRUE)
y = mat[2:63,1]
print(y)
x = matrix(as.numeric(mat[2:63,2:2001]),ncol=2000,byrow=TRUE)
#lapply(y,function(i) binaryzation(i))

#result <- LRCUDA(x,y, error.threshhold = 62, device.id = c(0,1,2,3,0,1,2,3,0,1,2,3))
