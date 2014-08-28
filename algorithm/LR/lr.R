# lr by built-in lm
# 
# Suppose y = ax + b
# Input: y vectors, x vectors
#   The argument is the input size of x/y, 1M by default
# Author: Haichuan
###############################################################################

setup <- function(args='1000000') {
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 1000000L }
    
    x<- runif(n, 0, 10) 
    y<- x + rnorm(n) + 1;
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
