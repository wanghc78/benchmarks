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
    #now change data into list structure
    list_data <- lapply(1:n, function(i) data[i,])
    library(vecapply)
    return(list(data=list_data, clusters=clusters, niter=niter))
}

run <- function(data) {
    clusters <- data$clusters
    niter <- data$niter
    pts <- data$data
    centers <- pts[1:clusters] 
    
    V_dist.func <- function(V_pts){ 
        #org inner only change V_pts related one
        dist.inner.func <- function(center){
            rowSums((V_pts - va_repVecData(center, V_pts))^2)
        }
        #here lapply will be changed to apply
        simplify2array(apply(V_centers, 1, dist.inner.func))
    }
    
    V_pts <- va_list2vec(pts)
    V_centers <- va_list2vec(centers) #pick 10 as default centers
    size <- integer(clusters);
    for(i in 1:niter) {
        #map each item into distance to 10 centers.
        V_dists <- V_dist.func(V_pts)
        ids <- apply(V_dists, 1, which.min)
        #calculate the new centers through mean
        for(j in 1:clusters) {
            V_cur_cluster <- V_pts[ids==j,]
            size[j] <- nrow(V_cur_cluster)
            V_centers[j,] <- colSums(V_cur_cluster) / size[j]
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
