# k-means - lapply based implementation with manually vecapply (outer level simple)
# 
# Author: Haichuan Wang
#
# k-means using lapply based iterative algorithm with manually vec apply trans
###############################################################################
app.name <- "k-means_lapply_level1_simple"
source('setup_k-means.R')
library(vecapply)

run <- function(dataset) {
    ncluster <- dataset$ncluster
    niter <- dataset$niter
    Points <- dataset$Points
    centers <- Points[1:ncluster] 
    
    V_dist.func <- function(V_Points){ 
        #org inner only change V_Points related one
        dist.inner.func <- function(center){
            rowSums((V_Points - va_repVecData(center, V_Points))^2)
        }
        #here lapply will be changed to apply
        apply(V_centers, 1, dist.inner.func)
    }
    
    V_Points <- va_list2vec(Points)
    V_centers <- va_list2vec(centers) #pick 10 as default centers
    size <- integer(ncluster)
    ptm <- proc.time() #previous iteration's time
    for(iter in 1:niter) {
        #map each item into distance to 10 centers.
        V_dists <- V_dist.func(V_Points)
        ids <- lapply(va_vec2list(V_dists), which.min);
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
