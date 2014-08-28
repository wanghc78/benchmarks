# k-means by built-in kmeans
# 
# Input: 3-dim points, k-means to 10 clusters, with iteration 10.
#   The argument is the input number of points, 100K by default
# Author: Haichuan
###############################################################################

setup <- function(args='100000') {
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 100000L }

    data <- matrix(rnorm(n * 3L, sd = 1), ncol = 3)
    return(data)
}

run <- function(data) {
    res<-kmeans(data, 10, iter.max=10);
    cat("Centers:\n")
    print(res$centers);
    cat("Sizes:\n")
    print(res$size);
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
