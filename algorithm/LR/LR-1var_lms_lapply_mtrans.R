# LinearRegression-1var - LMS(least mean square) lapply based solution with manual vecapply
# 
# Author: Haichuan Wang
###############################################################################
app.name <- 'LR-1var_lms_lapply_mtrans'
source('setup_LR-1var.R')
library(vecapply)

run <- function(dataset) {
    YX <- dataset$YX
    niter <- dataset$niter
    
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

    theta <- double(length(YX[[1]])) #initial guess as 0
    alpha <- 0.05 / length(YX)# small step
    #V_YX <- t(simplify2array(YX))
    ptm <- proc.time() #previous iteration's time
    for(iter in 1:niter) {
        delta <- va_vec2list(V_grad.func(va_list2vec(YX)))
        #cat('delta =', delta, '\n')
        theta <- theta - alpha * Reduce('+', delta) 
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
