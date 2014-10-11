# lr by built-in lm
# 
# Suppose y = ax + b
# Input: y vectors, x vectors
#   The argument is the input size of x/y, 1M by default
# Author: Haichuan Wang
###############################################################################

setup <- function(args=c('1000000', '10')) {
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 1000000L }
    
    nvar <-as.integer(args[2])
    if(is.na(nvar)){ nvar <- 10L }
    
    
    x<- matrix(runif(n*nvar, 0, 10), nrow=nvar, ncol=n) 
    y<- colSums(x) + rnorm(n) + 1 # now the coefficient are all 1
    yx <- lapply(1:n, function(i){c(y[i],x[,i])})
    data <- list(yx=yx, nvar=nvar);
    
    return(data)
}

run <- function(data) {
    
    
    V_A.func <- function(yx) {
        x <- yx
        x[,1] <- 1 #modify the 1st element set to 1
        crossprod(x)
    }
    
    V_b.func <- function(yx) {
        y <- yx[,1]
        x <- yx
        x[,1] <-1
        x * y
    }
    
    yx <- data$yx
    V_yx <- va_list2vec(yx)
    A <- V_A.func(V_yx)
    b <- colSums(V_b.func(V_yx))
    
    theta <- solve(A, b)
    print(theta)
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
