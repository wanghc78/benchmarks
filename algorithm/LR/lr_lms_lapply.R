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
    yx <- lapply(1:n, function(i){c(y[i],x[i])})
    data <- list(yx=yx, niter=niter);
    
    return(data)
}

run <- function(data) {
    
    #X includes "1" column, Y column vec
    grad.func <- function(yx) {
        y <- yx[1]
        x <- c(1, yx[2])  # add 1 to est interception
        error <- (sum(x *theta) - y)
        delta <- error * x
        return(delta)
    }
    
    cost <- function(X, y, theta) {
        # computes the cost of using theta as the parameter for linear regression
        # to fit the data points in X and y
        sum((X %*% theta - y)^2)/(2 * length(y))
    }
    
    yx <- data$yx
    niter<-data$niter
    theta <- double(length(yx[[1]])) #initial guess as 0
    alpha <- 0.05 # small step

    for(iter in 1:niter) {
        delta <- lapply(yx, grad.func)
        #cat('delta =', delta, '\n')
        theta <- theta - alpha * Reduce('+', delta) / length(yx)
        cat('theta =', theta, '\n')
        #print(cost(X,y, theta))
    }
    print(theta)
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
