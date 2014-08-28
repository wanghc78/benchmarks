# lr by built-in lm
# 
# Suppose y = ax + b
# Input: y vectors, x vectors
#   The argument is the input size of x/y, 1M by default
# Author: Haichuan Wang
###############################################################################

setup <- function(args=c('1000000', '10', '100')) {
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 1000000L }
    
    nvar <-as.integer(args[2])
    if(is.na(nvar)){ nvar <- 10L }
    
    niter<-as.integer(args[3])
    if(is.na(niter)){ niter <- 100L }
    
    x<- matrix(runif(n*nvar, 0, 10), nrow=nvar, ncol=n) 
    y<- colSums(x) + rnorm(n) + 1 # now the coefficient are all 1
    yx <- lapply(1:n, function(i){c(y[i],x[,i])})
    data <- list(yx=yx, nvar=nvar, niter=niter);
    source('../vecutil.R')
    return(data)
}

run <- function(data) {
    
    #X includes "1" column, Y column vec
    V_grad.func <- function(V_yx) {
        V_y <- V_yx[,1]
        V_x <- V_yx
        V_x[,1] <- vecData(1, V_yx) #modify the 1st element
        V_error <- (rowSums(V_x * vecData(theta, V_x)) - V_y)
        V_delta <- V_error * V_x
        return(V_delta)
    }
    
    cost <- function(X, y, theta) {
        # computes the cost of using theta as the parameter for linear regression
        # to fit the data points in X and y
        sum((X %*% theta - y)^2)/(2 * length(y))
    }
    
    yx <- data$yx
    nvar <- data$nvar
    niter<-data$niter
    theta <- double(length(yx[[1]])) #initial guess as 0
    alpha <- 0.05/nvar # small step
    V_yx <- list2vec(yx)
    for(iter in 1:niter) {
        V_delta <- V_grad.func(V_yx)
        #cat('delta =', delta, '\n')
        theta <- theta - alpha * colSums(V_delta) / length(yx)
        cat('theta =', theta, '\n')
        #print(cost(X,y, theta))
    }
    print(theta)
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
