# k-means-1D - lapply based implementation with manually optimized vecapply
# 
# Author: Haichuan Wang
#
# k-means-1D using lapply based iterative algorithm with manually optimized vecapply transform
###############################################################################
app.name <- "k-means-1D_lapply_mtrans_opt"
source('setup_k-means-1D.R')
library(vecapply)

run <- function(dataset) {
    ncluster <- dataset$ncluster
    niter <- dataset$niter
    Points <- dataset$Points
    
    size <- integer(ncluster);
    centers <- Points[1:ncluster] 
    V_dist.func <- function(V_ptr){
        
        # inner fun, only changes little.
#        dist.inner.func <- function(center){
#            (V_Points - vecData(center, V_Points))^2
#        }
        #now need vec above one
        #simple vec
#        V_dist.inner.func <- rawVecFun(dist.inner.func)
        
        #complex vectorize. Just replace the original one's name
        V_dist.inner.func <- function(V_centers) {
            (VV_Points - va_repVecData(V_centers, V_Points))^2
        }
        
        V_dist.inner.func(V_centers)
    }
    
    V_Points <- va_list2vec(Points)
    V_centers <- va_list2vec(centers)
    VV_Points <- va_colRepVecData(V_Points, V_centers)
    ptm <- proc.time() #previous iteration's time
    for(iter in 1:niter) {
        #map each item into distance to 10 centers.
        V_dists <- V_dist.func(V_Points)
        ids <- apply(V_dists, 1, which.min)
        #calculate the new centers through mean
        for(j in 1:ncluster) {
            V_cur_cluster <- V_Points[ids==j]
            size[j] <- length(V_cur_cluster)
            V_centers[j] <- sum(V_cur_cluster) / size[j]
        }
        ctm <- proc.time()
        cat("[INFO]Iter", iter, "Time =", (ctm - ptm)[[3]], '\n')
        ptm <- ctm
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
