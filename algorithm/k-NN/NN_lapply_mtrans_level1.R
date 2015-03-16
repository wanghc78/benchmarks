# Nearest Neighbor - lapply based implementation with manual transformation (level1)
# 
# Author: Haichuan Wang
###############################################################################
app.name <- "NN_lapply_mtrans_level1"
source('setup_k-NN.R')
library(vecapply)


run <- function(dataset) {
    
    list_train<-dataset$train_set
    train_n <- length(list_train)
    list_test<-dataset$test_set
    test_n <- length(list_test)
    clusters<- dataset$clusters
    
    ptm <- proc.time() #previous iteration's time
    #outer loop, map function for each test
    
    V_NN.fun <- function(V_test) {
        #calculate the distance to all 
        dists.fun <- function(train_item) {
            rowSums((va_repVecData(train_item$val,V_test$val)  - V_test$val)^2)
        }
        
        V_dists <- lapply(list_train, dists.fun)
        VV_dists <- va_list2vec(V_dists)
        #get the which min
        V_min.train <- apply(VV_dists, 2, which.min)
        #get the category
        V_test$label <- ((va_list2vec(list_train))$label)[V_min.train]
        V_test
    }
    #note moved here
    vec_test<-va_list2vec(list_test)
    out_vec_test<- V_NN.fun(vec_test) #it has a new $label as vector
    out_list_test <- va_vec2list(out_vec_test) #change back
    
    #get the cl
    test_cl_vec <- sapply(out_list_test, function(test_item){test_item$label})
    test_cl <- factor(test_cl_vec)
    cat("[INFO]Time =", (proc.time()-ptm)[[3]], '\n')
    print(summary(test_cl))
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
