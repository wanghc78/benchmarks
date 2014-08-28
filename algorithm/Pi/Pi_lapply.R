# Pi: monte-carlo method
# 
# Input: rand list, each element is [rand1 rand2]. 
#   The argument is the input size of rand list, 10M by default
# Author: Haichuan Wang
###############################################################################

setup <- function(args=c('10000000')) {
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 10000000L }
        
    rdata <- runif(n*2) 
    list_data <- lapply(1:n, function(i){rdata[(2*i-1):(2*i)]})
    
    return(list_data)
}

run <- function(list_data) {
    
    #X includes "1" column, Y column vec
    sample.func <- function(aSample) {
        if((aSample[1]^2 + aSample[2]^2) < 1) {
            1.0
        } else {
            0.0
        }
    }
    
    sampleOut <- lapply(list_data, sample.func)
    
    reduceCount <- Reduce('+', sampleOut)
    mcPi <- 4.0 * reduceCount / length(list_data)
    
    cat('Pi = ', mcPi, '\n');
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
