# LinearRegression-1var - OLS(Ordinary Least Squares) lapply based solution with vecapply
# 
# Author: Haichuan Wang
###############################################################################
app.name <- 'LR-1var_ols_lapply_cmp'
source('setup_LR-1var.R')
library(vecapply)

run <- function(dataset) {
    YX <- dataset$YX
    #X includes "1" column, Y column vec    
    A.func <- function(yx) {
        x <- c(1, yx[2])
        tcrossprod(x)
    }
    
    b.func <- function(yx) {
        y <- yx[1]
        x <- c(1, yx[2])
        x * y
    }
    
    A <- Reduce('+', lapply(YX, A.func))
    b <- Reduce('+', lapply(YX, b.func))
    
    theta <- solve(A, b)
    print(theta)
}

run<-va_cmpfun(run)

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
