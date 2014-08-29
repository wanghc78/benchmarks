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
    library(vecapply)
    return(data)
}

run <- function(data) {
    
    V_grad.func <- function(V_yx) {
        V_y <- V_yx[,1]
        V_x <- cbind(va_repVecData(1, V_yx), V_yx[,2])  # add 1 to est interception
        V_error <- (rowSums(V_x  * va_repVecData(theta, V_x)) - V_y)
        V_delta <- V_error * V_x
        return(V_delta)
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
    #V_yx <- t(simplify2array(yx))
    for(iter in 1:niter) {
        delta <- va_vec2list(V_grad.func(va_list2vec(yx)))
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
