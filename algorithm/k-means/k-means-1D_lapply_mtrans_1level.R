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
    data <- lapply(1:n, function(i){data[i]})
    source('../vecutil.R')
    return(list(data=data, clusters=clusters, niter=niter))
}

run <- function(data) {

    
    clusters <- data$clusters
    niter <- data$niter
    pts <- data$data
    
    size <- integer(clusters);
    centers <- pts[1:clusters] 
    V_dist.func <- function(V_pts){ #ptr is now vectors
        
        dist.inner.func <- function(center){
                              (V_pts - vecData(center, V_pts))^2
                           }
                           
        #Org #lapply(centers, dist.inner.func)
        #now still maintain the original one. The rule. Only the use of V_pts should be changed
        simplify2array(lapply(V_centers, dist.inner.func))
    }
    
    V_pts <- list2vec(pts)
    V_centers <- list2vec(centers)
    for(i in 1:niter) {
        #map each item into distance to 10 centers.
        V_dists <- V_dist.func(V_pts)
        ids <- apply(V_dists, 1, which.min)
        #calculate the new centers through mean
        for(j in 1:clusters) {
            V_cur_cluster <- V_pts[ids==j]
            size[j] <- length(V_cur_cluster)
            V_centers[j] <- sum(V_cur_cluster) / size[j]
        }
    }
    #calculate the distance to the 10 centers
    
    cat("Centers:\n")
    print(V_centers);
    cat("Sizes:\n")
    print(size);
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
