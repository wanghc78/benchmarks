# LinearRegression-1var - LMS(least mean square) apply based solution
# 
# Author: Haichuan Wang
###############################################################################
app.name <- 'LR-1var_lms_apply'
source('setup_LR-1var.R')

run <- function(dataset) {
    YX <- dataset$YX
    niter <- dataset$niter
    vYX <- t(simplify2array(YX))
    X <- cbind(1, matrix(vYX[,2]))
    Y <- vYX[,1]
    Xlist <- lapply(seq_len(nrow(X)), function(i) X[i,])
    
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
    

    theta <- double(ncol(X)) #initial guess
    alpha <- 0.05 / length(Y) # small step

    ptm <- proc.time() #previous iteration's time
    for(iter in 1:niter) {
        delta <- mapply(grad.func, Xlist, Y)
        #cat('delta =', delta, '\n')
        theta <- theta - alpha * rowSums(delta) 
        ctm <- proc.time()
        cat("[INFO]Iter", iter, "Time =", (ctm - ptm)[[3]], '\n')
        ptm <- ctm
        cat('theta =', theta, '\n')
        #print(cost(X,y, theta))
    }
    cat('Final theta =', theta, '\n')
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
