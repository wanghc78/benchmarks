# LinearRegression - OLS(Ordinary Least Squares) lapply based solution with vecapply
# 
# Author: Haichuan Wang
###############################################################################
app.name <- 'LR_ols_lapply_cmp'
source('setup_LR.R')
library(vecapply)

run <- function(dataset) {
    YX <- dataset$YX
    
    ptm <- proc.time() #previous iteration's time
    
    #X includes "1" column, Y column vec    
    A.func <- function(yx) {
        x <- yx
        x[1] <- 1 #modify the 1st element set to 1
        tcrossprod(x)
    }
    
    b.func <- function(yx) {
        y <- yx[1]
        x <- yx
        x[1] <-1
        x * y
    }
    
    A <- Reduce('+', lapply(YX, A.func))
    b <- Reduce('+', lapply(YX, b.func))
    
    theta <- solve(A, b)
    cat("[INFO]Time =", (proc.time()-ptm)[[3]], '\n')
    print(theta)
}

run <- va_cmpfun(run)

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
