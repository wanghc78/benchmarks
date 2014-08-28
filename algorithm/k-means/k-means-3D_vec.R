# k-means by built-in kmeans
# 
# Input: 3-dim points, k-means to 10 clusters, with iteration 10.
#   The argument is the input number of points, 100K by default
# Author: Haichuan
###############################################################################

setup <- function(args=c('100000', '10', '10')) {
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 100000L }
    
    clusters<-as.integer(args[2])
    if(is.na(clusters)){ clusters <- 10L }
    
    niter<-as.integer(args[3])
    if(is.na(niter)){ niter <- 10L }
    
    #the data, each is
    mean_shift <- rep(0:(clusters-1), length.out = 3*n)
    data <- matrix(rnorm(3*n, sd = 0.3) + mean_shift, ncol=3)
    
    return(list(data=data, clusters=clusters, niter=niter))
}

run <- function(data) {
    clusters <- data$clusters
    niter <- data$niter
    data <- data$data #data n * 3 matrix

    
    centers <- data[1:clusters, ] #pick 10 as default centers
    size <- integer(clusters);
    n <- nrow(data)
    dists <- matrix(0, nrow=n, ncol=clusters) #pre-allocate memory 
    for(i in 1:niter) {
        #need calculate each points' distance to all centers
        #try to use vec as much as possible
        for(j in 1:clusters) {
            center_expand <- matrix(rep(centers[j,], each=n), n, 3)
            dists[,j] = rowSums((data - center_expand)^2)
        }
        #map each item into distance to 10 centers.
        ids <- apply(dists, 1, which.min)
        #calculate the new centers through mean
        for(j in 1:clusters) {
            cur_cluster <- data[ids==j, ]
            size[j] <- nrow(cur_cluster)
            centers[j,] <- colMeans(cur_cluster)
        }
    }
    #calculate the distance to the 10 centers
    
    cat("Centers:\n")
    print(centers);
    cat("Sizes:\n")
    print(size);
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
