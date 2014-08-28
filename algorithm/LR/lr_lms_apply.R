# lr by built-in lm
# 
# Suppose y = ax + b
# Input: y vectors, x vectors
#   The argument is the input size of x/y, 1M by default
# Author: Haichuan Wang
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
    
    #X includes "1" column, Y column vec
    grad.func <- function(x, y) {
        error <- (sum(x *theta) - y)
        #This is a simple normalization
        delta <- error * x
        return(delta)
    }
    
    cost <- function(X, y, theta) {
        # computes the cost of using theta as the parameter for linear regression
        # to fit the data points in X and y
        sum((X %*% theta - y)^2)/(2 * length(y))
    }
    
    X <- cbind(1, matrix(data$x))
    Xlist <- lapply(seq_len(nrow(X)), function(i) X[i,])
    y <- data$y
    niter <- data$niter
    
    theta <- double(ncol(X)) #initial guess
    alpha <- 0.05 # small step

    for(iter in 1:niter) {
        delta <- mapply(grad.func, Xlist, y)
        #cat('delta =', delta, '\n')
        theta <- theta - alpha * rowSums(delta) / length(y)
        cat('theta =', theta, '\n')
        #print(cost(X,y, theta))
    }
    print(theta)
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
