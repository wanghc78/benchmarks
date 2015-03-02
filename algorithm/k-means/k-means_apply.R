# k-means - aapply based implementation
# 
# Author: Haichuan Wang
#
# k-means using apply based iterative algorithm
###############################################################################
app.name <- "k-means_apply"
source('setup_k-means.R')

run <- function(dataset) {
    ncluster <- dataset$ncluster
    niter <- dataset$niter
    Points <- dataset$Points
    vPoints <- t(simplify2array(Points)) # n * ndim matrix
    
    centers <- vPoints[1:ncluster, ] #pick 10 as default centers
    size <- integer(ncluster);
    ptm <- proc.time() #previous iteration's time
    for(iter in 1:niter) {
        #map each item into distance to 10 centers.
        dists <- apply(vPoints, 1, function(ptr){ 
                                  apply(centers, 1, function(center){
                                              sum((ptr-center)^2)
                                                  })
                                  })
        ids <- apply(dists, 2, which.min)
        #calculate the new centers through mean
        for(j in 1:ncluster) {
            cur_cluster <- vPoints[ids==j, ]
            size[j] <- nrow(cur_cluster)
            centers[j,] <- colMeans(cur_cluster)
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
    run(setup(commandArgs(TRUE)))
}
