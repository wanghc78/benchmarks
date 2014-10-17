# k-means by built-in kmeans
# 
# Input: 3-dim points, k-means to 10 clusters, with iteration 10.
#   The argument is the input number of points, 100K by default
# Author: Haichuan
###############################################################################

setup <- function(args=c('1000000', '10', '10', '10')) {
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 1000000L }

    dims<-as.integer(args[2])
    if(is.na(dims)){ dims <- 10L }
    
    clusters<-as.integer(args[3])
    if(is.na(clusters)){ clusters <- 10L }
    
    niter<-as.integer(args[4])
    if(is.na(niter)){ niter <- 10L }
    
    #the data, each is
    mean_shift <- rep(0:(clusters-1), length.out = dims*n)
    data <- matrix(rnorm(dims*n, sd = 0.3) + mean_shift, ncol=dims)
    #now change data into list structure
    list_data <- lapply(1:n, function(i) data[i,])
    
    return(list(data=list_data, dims<- dims, clusters=clusters, niter=niter))
}

run <- function(data) {
    clusters <- data$clusters
    niter <- data$niter
    list_data <- data$data
    
    dist.func <- function(ptr){ 
        dist.inner.func <- function(center){
            sum((ptr-center)^2)
        }
        lapply(centers, dist.inner.func)
    }
    
    centers <- list_data[1:clusters] #pick 10 as default centers
    size <- integer(clusters);
    for(i in 1:niter) {
        #map each item into distance to 10 centers.
        dists <- lapply(list_data, dist.func)
        ids <- lapply(dists, which.min)
        #calculate the new centers through mean
        for(j in 1:clusters) {
            cur_cluster <- list_data[ids==j]
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

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
