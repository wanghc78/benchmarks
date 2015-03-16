# k Nearest Neighbor - lapply based implementation with outer level manual vecapply
# 
# Author: Haichuan Wang
###############################################################################
app.name <- "k-NN_lapply_mtrans_level1"
source('setup_k-NN.R')
library(vecapply)

run <- function(dataset) {
    list_train<-dataset$train_set
    train_n <- length(list_train)
    list_test<-dataset$test_set
    test_n <- length(list_test)
    clusters<- dataset$clusters
    k <- dataset$k
    
    ptm <- proc.time() #previous iteration's time
    #outer loop, map function for each test
    
    V_kNN.fun <- function(test_item) {
        #calculate the distance to all 
        dists.fun <- function(train_item) {
            rowSums((va_repVecData(train_item$val, test_item) - test_item$val)^2)
        }
        
        dists_list <- lapply(list_train, dists.fun)
        #change to dists_vec, and do the sorting
        dists <- simplify2array(dists_list)
        mink.indices <- va_list2vec(va_vecApplyWrapper(dists, order))
        #then should pick the first k items, find t
        train_items_indices <- mink.indices[, 1:k]
        #now get the their label and vote
        
        train_items_category <- character(k)
        train_items_category <- va_repVecData(train_items_category, test_item)
        v_list_train <- va_list2vec(list_train)
        for(i in 1:k) {
          train_items_category[,i] <- v_list_train$label[train_items_indices[,i]]
        }
        
        #train_items_category <- sapply(list_train[train_items_indices], function(item){item$label})
        test_item$label <- va_list2vec(va_vecApplyWrapper(train_items_category, function(train_items_category) {
                names(which.max(table(train_items_category)))
            }))
        test_item
    }
    
    V_out_list_test <- V_kNN.fun(va_list2vec(list_test))
    
    #get the cl
    V_test_cl <- (function(test_item){test_item$label})(V_out_list_test)
    test_cl <- factor(V_test_cl)
    cat("[INFO]Time =", (proc.time()-ptm)[[3]], '\n')
    print(summary(test_cl))
}


if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}