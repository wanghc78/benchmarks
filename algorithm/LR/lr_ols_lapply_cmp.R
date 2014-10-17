# lr by built-in lm
# 
# Suppose y = ax + b
# Input: y vectors, x vectors
#   The argument is the input size of x/y, 1M by default
# Author: Haichuan Wang
###############################################################################
library(vecapply)
setup <- function(args=c('1000000')) {
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 1000000L }
    
    x<- runif(n, 0, 10) 
    y<- x + rnorm(n) + 1;
    yx <- lapply(1:n, function(i){c(y[i],x[i])})
    data <- list(yx=yx);
    
    return(data)
}

run <- function(data) {
    
    #X includes "1" column, Y column vec    
    A.func <- function(yx) {
        x <- c(1, yx[2])
        tcrossprod(x)
    }
    
    b.func <- function(yx) {
        y <- yx[1]
        x <- c(1, yx[2])
        x * y
    }
    
    yx <- data$yx
    
    A <- Reduce('+', lapply(yx, A.func))
    b <- Reduce('+', lapply(yx, b.func))
    
    theta <- solve(A, b)
    print(theta)
}

run<-va_cmpfun(run)

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
