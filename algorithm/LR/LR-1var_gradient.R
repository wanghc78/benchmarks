# LinearRegression-1var - initial gradient solution
# 
# Author: Haichuan Wang
###############################################################################
app.name <- 'LR-1var_gradient'
source('setup_LR-1var.R')

run <- function(dataset) {
    YX <- dataset$YX
    niter <- dataset$niter
    
    alpha <- 0.05 # small step
    
    grad.func <- function(yx) {
        x <- c(1, yx[2]) #note add 1 here
        y <- yx[1]
        (1 / (1 + exp(-y * (w %*% x))) - 1) * y * x
    }
    
    w <- double(length(YX[[1]])) #note should be ncol(yx_array) - 1 + 1
    ptm <- proc.time() #previous iteration's time
    for(iter in 1:niter) {
        grad <- lapply(YX, grad.func)
        gradient <- Reduce("+", grad, 0)
        w <- w - alpha * gradient / length(YX) # simple normalize
        ctm <- proc.time()
        cat("[INFO]Iter", iter, "Time =", (ctm - ptm)[[3]], '\n')
        ptm <- ctm
        print(w)
    }
    cat('Final w =', w, '\n')
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
