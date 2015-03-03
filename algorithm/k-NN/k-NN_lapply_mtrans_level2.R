# k Nearest Neighbor - lapply based implementation with inner level manual vecapply
# 
# Author: Haichuan Wang
###############################################################################
app.name <- "k-NN_lapply_mtrans_level2"
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
    
    kNN.fun <- function(test_item) {
        #calculate the distance to all 
        V_dist.func<-function(V_train){
            rowSums((V_train$val - va_repVecData(test_item$val, V_train$val))^2)
        }
        
        V_dists <- V_dist.func(vec_train)
        mink.indices <-order(V_dists)
        #then should pick the first k items, find t
        train_items_indices <- mink.indices[1:k]
        #now get the their label and vote
        
        train_items_category <- character(k)
        for(i in 1:k) {
            train_items_category[i] <- list_train[[train_items_indices[i]]]$label
        }
        #get the category
        test_item$label <- names(which.max(table(train_items_category)))
        test_item
    }
    
    #note moved here
    vec_train<-va_list2vec(list_train) #vec_train$val vec_train$label  
    out_list_test <- lapply(list_test, kNN.fun)
    
    #get the cl
    test_cl_vec <- sapply(out_list_test, function(test_item){test_item$label})
    test_cl <- factor(test_cl_vec)
    print(summary(test_cl))
}


if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}