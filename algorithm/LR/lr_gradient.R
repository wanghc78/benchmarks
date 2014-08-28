# lr by built-in lm
# 
# Suppose y = ax + b
# Input: y vectors, x vectors
#   The argument is the input size of x/y, 1M by default
# Author: Hao Lin, Haichuan Wang
###############################################################################

setup <- function(args=c('1000000', '100')) {
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 1000000L }
    
    niter<-as.integer(args[2])
    if(is.na(niter)){ niter <- 100L }
    
    x<- runif(n, 0, 10) 
    y<- x + rnorm(n) + 1;
    data <- list(x=x, y=y, niter=niter);
    return(data)
}


run <- function(data) {
    #merge x,y
    yx_array<-cbind(data$y, data$x)
    niter<-data$niter
    print(niter)
    #change to list
    yx_list <- lapply(1:nrow(yx_array), function(i)yx_array[i,])
    
    alpha <- 0.05 # small step
    
    grad.func <- function(yx) {
        x <- c(1, yx[2]) #note add 1 here
        y <- yx[1]
        (1 / (1 + exp(-y * (w %*% x))) - 1) * y * x
    }
    
    w <- double(ncol(yx_array)) #note should be ncol(yx_array) - 1 + 1
    for(iter in 1:niter) {
        grad <- lapply(yx_list, grad.func)
        gradient <- Reduce("+", grad, 0)
        w <- w - alpha * gradient / length(yx_list) # simple normalize
        print(w)
    }
    print(w)
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
