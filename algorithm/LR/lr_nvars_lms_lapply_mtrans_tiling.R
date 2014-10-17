# lr by built-in lm
# 
# Suppose y = ax + b
# Input: y vectors, x vectors
#   The argument is the input size of x/y, 1M by default
# Author: Haichuan Wang
###############################################################################
library(vecapply)
setup <- function(args=c('1000000', '10', '100', '1000')) {
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 1000000L }
    
    nvar <-as.integer(args[2])
    if(is.na(nvar)){ nvar <- 10L }
    
    niter<-as.integer(args[3])
    if(is.na(niter)){ niter <- 100L }
    
    tile_sz<-as.integer(args[4])
    if(is.na(tile_sz)){ tile_sz <- 1000L }
    
    
    x<- matrix(runif(n*nvar, 0, 10), nrow=nvar, ncol=n) 
    y<- colSums(x) + rnorm(n) + 1 # now the coefficient are all 1
    yx <- lapply(1:n, function(i){c(y[i],x[,i])})
    data <- list(yx=yx, nvar=nvar, niter=niter, tile_sz = tile_sz);

    return(data)
}

run <- function(data) {
    
    #X includes "1" column, Y column vec
    V_grad.func <- function(V_yx) {
        V_y <- V_yx[,1]
        V_x <- V_yx
        V_x[,1] <- va_repVecData(1, V_yx) #modify the 1st element
        V_error <- (rowSums(V_x * va_repVecData(theta, V_x)) - V_y)
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
    tile_sz = data$tile_sz
    tiles <- length(yx) / tile_sz # right now it should be simple
    V_yx <- list()
    #construct tiles
    for(i in 1: tiles) {
        start <- (i-1)*tile_sz+1
        end <- start + tile_sz - 1;
        V_yx[[i]] <-va_list2vec(yx[start:end])
    }
    
    
    theta <- double(length(yx[[1]])) #initial guess as 0
    alpha <- 0.05/nvar # small step
    new_theta <- theta
    for(iter in 1:niter) {
        for(i in 1:tiles) {
            V_delta <- V_grad.func(V_yx[[i]])
            #cat('delta =', delta, '\n')
            new_theta <- new_theta - alpha * colSums(V_delta) / length(yx)
        }
        theta <- new_theta
        cat('theta =', theta, '\n')
        #print(cost(X,y, theta))
    }
    print(theta)
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
