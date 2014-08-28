# lr by built-in lm
# 
# Suppose y = ax + b
# Input: y vectors, x vectors
#   The argument is the input size of x/y, 1M by default
# Author: Haichuan
###############################################################################

setup <- function(args=c('1000000', '10')) {
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 1000000L }
    
    nvar <-as.integer(args[2])
    if(is.na(nvar)){ nvar <- 10L }
    
    x<- matrix(runif(n*nvar, 0, 10), nrow=n, ncol=nvar) 
    y<- rowSums(x) + rnorm(n) + 1 # now the coefficient are all 1
    data <- list(x=x, y=y);
    return(data)
}

run <- function(data) {
    res<-lm(data$y ~ data$x);
    print(res)
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
