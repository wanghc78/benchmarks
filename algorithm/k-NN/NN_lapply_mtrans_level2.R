# Nearest Neighbor - lapply based implementation with manual transformation (level2)
# 
# Author: Haichuan Wang
###############################################################################
app.name <- "NN_lapply_mtrans_level2"
source('setup_k-NN.R')
library(vecapply)


run <- function(dataset) {
    
    list_train<-dataset$train_set
    train_n <- length(list_train)
    list_test<-dataset$test_set
    test_n <- length(list_test)
    clusters<- dataset$clusters
    
    #outer loop, map function for each test
    
    NN.fun <- function(test_item) {
        #calculate the distance to all 
        V_dist.func<-function(V_train){
            rowSums((V_train$val - va_repVecData(test_item$val, V_train$val))^2)
        }
        
        V_dists <- V_dist.func(vec_train)
        #get the which min
        min.train <- which.min(V_dists)
        #get the category
        test_item$label <- (vec_train$label)[min.train]
        test_item
    }
    #note moved here
    vec_train<- va_list2vec(list_train) #vec_train$val vec_train$label  
    out_list_test <- lapply(list_test, NN.fun)
    
    #get the cl
    test_cl_vec <- sapply(out_list_test, function(test_item){test_item$label})
    test_cl <- factor(test_cl_vec)
    print(summary(test_cl))
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
