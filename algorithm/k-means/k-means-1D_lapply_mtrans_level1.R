# k-means-1D - lapply based implementation with manually vecapply (outer level)
# 
# Author: Haichuan Wang
#
# k-means-1D using lapply based iterative algorithm with outer level manually vecapply transform
###############################################################################
app.name <- "k-means-1D_lapply_mtran_level1"
source('setup_k-means-1D.R')
library(vecapply)

run <- function(dataset) {
    ncluster <- dataset$ncluster
    niter <- dataset$niter
    Points <- dataset$Points
    
    size <- integer(ncluster);
    centers <- Points[1:ncluster] 
    V_dist.func <- function(V_Points){ #ptr is now vectors
        
        dist.inner.func <- function(center){
                              (V_Points - va_repVecData(center, V_Points))^2
                           }
                           
        #Org #lapply(centers, dist.inner.func)
        #now still maintain the original one. The rule. Only the use of V_Points should be changed
        simplify2array(lapply(V_centers, dist.inner.func))
    }
    
    V_Points <- va_list2vec(Points)
    V_centers <- va_list2vec(centers)
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
