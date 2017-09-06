test.Call <- function(a,b){
    if(is.na(a) | is.na(b)){
        stop("a or b can't be obmitted")
    }
    .Call("test", a, b, PACKAGE="LRCUDA")
}
