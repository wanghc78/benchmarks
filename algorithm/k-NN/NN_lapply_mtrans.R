# Nearest Neighbor - lapply based implementation with manual transformation
# 
# Author: Haichuan Wang
###############################################################################
app.name <- "NN_lapply_mtrans"
source('setup_k-NN.R')
library(vecapply)


run <- function(dataset) {
    
    list_train<-dataset$train_set
    train_n <- length(list_train)
    list_test<-dataset$test_set
    test_n <- length(list_test)
    clusters<- dataset$clusters
    
    #outer loop, map function for each test
    
    V_NN.fun <- function(V_test) {
        #calculate the distance to all 
        V_dist.func<-function(V_train){
            rowSums((V_train$val - va_repVecData(test_item$val, V_train$val))^2)
        }
        
        VV_dist.func<-function(V_train) {
            rowSums((va_colRepVecData(V_train$val, V_test$val)
                    - va_repVecData(V_test$val, V_train$val))^2, dims=2)
        }
        
        VV_dists <- VV_dist.func(vec_train)
        #get the which min
        V_min.train <- apply(VV_dists, 2, which.min)
        #get the category
        V_test$label <- (vec_train$label)[V_min.train]
        V_test
    }
    #note moved here
    vec_train <- va_list2vec(list_train) #vec_train$val vec_train$label  
    vec_test <- va_list2vec(list_test)
    out_vec_test<- V_NN.fun(vec_test) #it has a new $label as vector
    out_list_test <- va_vec2list(out_vec_test) #change back
    
    #get the cl
    test_cl_vec <- sapply(out_list_test, function(test_item){test_item$label})
    test_cl <- factor(test_cl_vec)
    print(summary(test_cl))
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
