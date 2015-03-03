# LinearRegression - OLS(Ordinary Least Squares) lapply based solution with manual vecapply
# 
# Author: Haichuan Wang
###############################################################################
app.name <- 'LR_ols_lapply_mtrans'
source('setup_LR.R')
library(vecapply)

run <- function(dataset) {
    YX <- dataset$YX
    
    V_A.func <- function(yx) {
        x <- yx
        x[,1] <- 1 #modify the 1st element set to 1
        crossprod(x)
    }
    
    V_b.func <- function(yx) {
        y <- yx[,1]
        x <- yx
        x[,1] <-1
        x * y
    }
    
    V_YX <- va_list2vec(YX)
    A <- V_A.func(V_YX)
    b <- colSums(V_b.func(V_YX))
    
    theta <- solve(A, b)
    print(theta)
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
