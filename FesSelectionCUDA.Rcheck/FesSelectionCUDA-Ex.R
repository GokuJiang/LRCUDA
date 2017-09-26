pkgname <- "FesSelectionCUDA"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('FesSelectionCUDA')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("getGPUIds")
### * getGPUIds

flush(stderr()); flush(stdout())

### Name: getGPUIds
### Title: Get GPU device IDs from localhost or a cluster.
### Aliases: getGPU
### Keywords: gpu id

### ** Examples

gpu.ids <- getGPUIds()
print(gpu.ids)




### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
