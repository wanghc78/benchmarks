# LinearRegression - LMS(least mean square) lapply based solution with manual vecapply and tiling
# 
# Author: Haichuan Wang
###############################################################################
app.name <- 'LR_lms_lapply_mtrans_tiling'
source('setup_LR.R')
library(vecapply)

oldsetup <- setup
setup <- function(args=c('1000000', '10', '50', '1000')) {
    dataset <- oldsetup(args)
    
    tile_sz<-as.integer(args[4])
    if(is.na(tile_sz)){ tile_sz <- 1000L }
    dataset$tile_sz <- tile_sz
    cat('[INFO][', app.name, '] tile_sz=', tile_sz, '\n', sep='')
    dataset
}

run <- function(dataset) {
    YX <- dataset$YX
    niter <- dataset$niter
    tile_sz = data$tile_sz
    
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
    

    tile_sz = data$tile_sz
    tiles <- length(YX) / tile_sz # right now it should be simple
    V_YX <- list()
    #construct tiles
    for(i in 1: tiles) {
        start <- (i-1)*tile_sz+1
        end <- start + tile_sz - 1;
        V_YX[[i]] <-va_list2vec(YX[start:end])
    }
    
    
    theta <- double(length(YX[[1]])) #initial guess as 0
    alpha <- 0.05/ length(YX) / length(theta) # small step
    
    new_theta <- theta
    ptm <- proc.time() #previous iteration's time
    for(iter in 1:niter) {
        for(i in 1:tiles) {
            V_delta <- V_grad.func(V_YX[[i]])
            #cat('delta =', delta, '\n')
            new_theta <- new_theta - alpha * colSums(V_delta)
        }
        theta <- new_theta
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
