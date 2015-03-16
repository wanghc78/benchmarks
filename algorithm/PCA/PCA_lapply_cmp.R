# PCA analysis - Direct method with lapply and vecapply
# 
# Author: Haichuan Wang
###############################################################################
app.name <- 'PCA_lapply_cmp'
source('setup_PCA.R')
library(vecapply)

run <- function(dataset) {
    X <- dataset$X
    
    ptm <- proc.time() #previous iteration's time
    
    cross.func <- function(x) {
        tcrossprod(x)
    }
    
    mean.func <- function(x) {
        x
    }
    
    len <- length(X)
    XC <- Reduce('+', lapply(X, cross.func))
    vMean <- Reduce('+', lapply(X, mean.func)) / len
    
    covM <- XC/len - tcrossprod(vMean)
    eigen(covM)
    cat("[INFO]Time =", (proc.time()-ptm)[[3]], '\n')
}

run <- va_cmpfun(run)

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
