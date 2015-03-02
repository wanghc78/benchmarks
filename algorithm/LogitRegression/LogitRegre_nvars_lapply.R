# lr by built-in lm
# 
# Suppose y = ax + b
# Input: y vectors, x vectors
#   The argument is the input size of x/y, 1M by default
# Author: Haichuan Wang
###############################################################################
app.name <-"LogitRegression_nvars"

setup <- function(args=c('1000000', '10', '100')) {
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 1000000L }
    
    nvar <-as.integer(args[2])
    if(is.na(nvar)){ nvar <- 10L }
    
    niter<-as.integer(args[3])
    if(is.na(niter)){ niter <- 100L }
    
    cat('[INFO][', app.name, '] n=', n, ', nvar=', nvar, ', niter=', niter, '\n', sep='')
    
    x<- matrix(runif(n*nvar, -1, 1), nrow=nvar, ncol=n) 
    theta <- rep(1,nvar)
    y<- 1/(1+exp(-(1+colSums(theta*x)))) # now the coefficient are all 1
    yx <- lapply(1:n, function(i){c(y[i],x[,i])})
    data <- list(yx=yx, nvar=nvar, niter=niter);
    
    return(data)
}

run <- function(data) {
    
    #X includes "1" column, Y column vec
    grad.func <- function(yx) {
        y <- yx[1]
        x <- yx  
        x[1] <- 1 #modify the 1st column
        logit <- 1/(1 + exp(-sum(theta*x)))
        (y-logit) * x
    }
    
    yx <- data$yx
    niter<-data$niter
    theta <- double(length(yx[[1]])) #initial guess as 0

    ptm <- proc.time() #previous iteration's time
    for(iter in 1:niter) {
        delta <- lapply(yx, grad.func)
        #cat('delta =', delta, '\n')
        theta <- theta + Reduce('+', delta) / length(yx)
        ctm <- proc.time()
        cat("[INFO]Iter", iter, "Time =", (ctm - ptm)[[3]], '\n')
        ptm <- ctm
        cat('theta =', theta, '\n')
        #print(cost(X,y, theta))
    }
    print(theta)
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
