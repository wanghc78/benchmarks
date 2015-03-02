# k-means - lapply based implementation with manually vecapply (inner levels)
# 
# Author: Haichuan Wang
#
# k-means using lapply based iterative algorithm with manually vec apply trans
###############################################################################
app.name <- "k-means_lapply_level2"
source('setup_k-means.R')
library(vecapply)

run <- function(dataset) {
    ncluster <- dataset$ncluster
    niter <- dataset$niter
    Points <- dataset$Points
    centers <- Points[1:ncluster] 
    
    dist.func <- function(ptr){ 
        V_dist.inner.func <- function(V_centers){
            rowSums((va_repVecData(ptr, V_centers) - V_centers)^2)
        }
        V_dist.inner.func(V_centers)
    }
    
    centers <- Points[1:ncluster] #pick 10 as default centers
    size <- integer(ncluster)
    ptm <- proc.time() #previous iteration's time
    for(iter in 1:niter) {
        #map each item into distance to 10 centers.
        V_centers <- va_list2vec(centers)
        dists <- lapply(Points, dist.func)
        ids <- lapply(dists, which.min)
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
    print(centers);
    cat("Sizes:\n")
    print(size);
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
