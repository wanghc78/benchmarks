# PCA analysis - Direct method with lapply
# 
# Author: Haichuan Wang
###############################################################################
app.name <- 'PCA_lapply'
source('setup_PCA.R')

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

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
