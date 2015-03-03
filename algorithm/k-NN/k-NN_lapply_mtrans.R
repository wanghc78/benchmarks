# k Nearest Neighbor - lapply based implementation with manual vecapply
# 
# Author: Haichuan Wang
###############################################################################
app.name <- "k-NN_lapply_mtrans"
source('setup_k-NN.R')
library(vecapply)

run <- function(dataset) {
    list_train<-dataset$train_set
    train_n <- length(list_train)
    list_test<-dataset$test_set
    test_n <- length(list_test)
    clusters<- dataset$clusters
    k <- dataset$k
    
    #outer loop, map function for each test
    
    V_kNN.fun <- function(V_test) {
        
        vec_train<-va_list2vec(list_train)
        #calculate the distance to all 
        V_dist.func<-function(V_train){
            rowSums((V_train$val - va_repVecData(test_item$val, V_train$val))^2)
        }
        
        VV_dist.func<-function(V_train) {
            rowSums((va_colRepVecData(V_train$val, V_test$val)
                                - va_repVecData(V_test$val, V_train$val))^2, dims=2)
        }
        
        VV_dists <- t(VV_dist.func(vec_train))
        mink.indices <- va_list2vec(va_vecApplyWrapper(VV_dists, order))
        #then should pick the first k items, find t
        train_items_indices <- mink.indices[, 1:k]
        #now get the their label and vote
        
        train_items_category <- character(k)
        train_items_category <- va_repVecData(train_items_category, V_test)
        for(i in 1:k) {
          train_items_category[,i] <- vec_train$label[train_items_indices[,i]]
        }
        
        #train_items_category <- sapply(list_train[train_items_indices], function(item){item$label})
        V_test$label <- va_list2vec(va_vecApplyWrapper(train_items_category, function(train_items_category) {
                names(which.max(table(train_items_category)))
            }))
        V_test
    }
    
    V_out_list_test <- V_kNN.fun(va_list2vec(list_test))
    
    #get the cl
    V_test_cl <- (function(test_item){test_item$label})(V_out_list_test)
    test_cl <- factor(V_test_cl)
    print(summary(test_cl))
}


if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}