# k-means-1D - lapply based implementation with simple manually vecapply (outer level)
# 
# Author: Haichuan Wang
#
# k-means-1D using lapply based iterative algorithm with simple outer level manually vecapply transform
###############################################################################
app.name <- "k-means-1D_lapply_mtran_level1_simple"
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
        lapply(V_centers, dist.inner.func)
    }
    
    V_Points <- va_list2vec(Points)
    V_centers <- va_list2vec(centers)
    ptm <- proc.time() #previous iteration's time
    for(iter in 1:niter) {
        #map each item into distance to 10 centers.
        V_dists <- V_dist.func(V_Points)
        ids <- apply(simplify2array(V_dists), 1, which.min);
        #calculate the new centers through mean
        for(j in 1:ncluster) {
            cur_cluster <- Points[ids==j]
            size[j] <- length(cur_cluster)
            centers[[j]] <- Reduce('+', cur_cluster) / size[j]
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
