# k-means by built-in kmeans
# 
# Input: 3-dim points, k-means to 10 clusters, with iteration 10.
#   The argument is the input number of points, 100K by default
# Author: Haichuan
###############################################################################
library(vecapply)
setup <- function(args=c('1000000', '10', '10')) {
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 1000000L }
    
    clusters<-as.integer(args[2])
    if(is.na(clusters)){ clusters <- 10L }
    
    niter<-as.integer(args[3])
    if(is.na(niter)){ niter <- 10L }
    
    #the data, each is
    mean_shift <- rep(0:(clusters-1), length.out = n)
    data <- rnorm(n, sd = 0.3) + mean_shift
    data <- lapply(1:n, function(i){data[i]})
    
    return(list(data=data, clusters=clusters, niter=niter))
}

run <- function(data) {
    clusters <- data$clusters
    niter <- data$niter
    pts <- data$data
    
    centers <- pts[1:clusters] #pick 10 as default centers
    size <- integer(clusters);
    
    dist.func <- function(ptr){
        dist.inner.func <- function(center){
                              (ptr-center)^2
                           }
        lapply(centers, dist.inner.func)
    }
    
    for(i in 1:niter) {
        #map each item into distance to 10 centers.
        dists <- lapply(pts, dist.func)
        ids <- lapply(dists, which.min);
        #calculate the new centers through mean
        for(j in 1:clusters) {
            cur_cluster <- pts[ids==j]
            size[j] <- length(cur_cluster)
            centers[[j]] <- Reduce('+', cur_cluster) / size[j]
        }
    }
    #calculate the distance to the 10 centers
    
    cat("Centers:\n")
    print(centers);
    cat("Sizes:\n")
    print(size);
}

run <- va_cmpfun(run)

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
