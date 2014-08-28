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
    mean_shift <- rep(0:(clusters-1), length.out = n)
    data <- rnorm(n, sd = 0.3) + mean_shift
    
    return(list(data=data, clusters=clusters, niter=niter))
}



run <- function(data) {
    clusters <- data$clusters
    niter <- data$niter
    pts <- data$data
    
    centers <- pts[1:clusters] #pick 10 as default centers
    size <- integer(clusters);
    for(i in 1:niter) {
        #map each item into distance to 10 centers.
        #tmp_centers <- t(matrix(rep(centers, length(data)), ncol=length(data)))
        #dists <- (data -tmp_centers)^2
        
        dists <- outer(pts, centers, function(x,y){(x-y)^2})
        ids <- apply(dists, 1, which.min);
        #calculate the new centers through mean
        for(j in 1:clusters) {
            cur_cluster <- pts[ids==j]
            size[j] <- length(cur_cluster)
            centers[j] <- mean(cur_cluster)
        }
    }
    #calculate the distance to the 10 centers
    
    cat("Centers:\n")
    print(sort(centers));
    cat("Sizes:\n")
    print(size);
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
