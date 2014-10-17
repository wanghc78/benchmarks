# pca analysis
# 
# Author: Haichuan Wang
###############################################################################
library(vecapply)
setup <- function(args=c('1000000', '10')) {
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 1000000L }
    
    nvar <-as.integer(args[2])
    if(is.na(nvar)){ nvar <- 10L }
    
    
    X <- matrix(runif(n*nvar, 0, 10), nrow=nvar, ncol=n) 
    X <- lapply(1:n, function(i){X[,i]})
    data <- list(X = X);
    
    return(data)
}

run <- function(data) {
    
    
    cross.func <- function(x) {
        tcrossprod(x)
    }
    
    mean.func <- function(x) {
        x
    }
    
    X <- data$X

    len <- length(X)
    XC <- Reduce('+', lapply(X, cross.func))
    vMean <- Reduce('+', lapply(X, mean.func)) / len
    
    covM <- XC/len - tcrossprod(vMean)
    eigen(covM)
}

run<-va_cmpfun(run)

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
