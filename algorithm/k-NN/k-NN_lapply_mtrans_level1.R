# Nearest Neighbor
# 
# Input 3-dim points, 10 categories, 'C1' to 'C10'.
#   Traing: 10K. 
#   Testing: 10K
# 
# Author: Haichuan Wang
###############################################################################

setup <- function(args=c('10000', '1000', '10', '5')) {
    train_n<-as.integer(args[1])
    if(is.na(train_n)){ train_n <- 10000L }
    
    test_n<-as.integer(args[2])
    if(is.na(test_n)){ test_n <- 1000L }   
    
    clusters<-as.integer(args[3])
    if(is.na(clusters)){ clusters <- 10L }
    
    k<-as.integer(args[4])
    if(is.na(k)){ k <- 5L }    
    
    #generate training
    mean_shift <- rep(0:(clusters-1), length.out = 3*train_n)
    train_set <- matrix(rnorm(3*train_n, sd = clusters/2) + mean_shift, ncol=3)
    list_train_set <- lapply(1:train_n, function(i) {
                                     label_str <-paste('C', as.character(mean_shift[i]), sep="")
                                     list(val=train_set[i,], label=label_str)
                                 })
                         
    test_set <- matrix(runif(3*test_n, min=-clusters, max=2*clusters-1), ncol=3)
    list_test_set <- lapply(1:test_n, function(i) {
                list(val=test_set[i,])
            })
    
    data <-list(train_set=list_train_set, 
            test_set=list_test_set,
            clusters=clusters,
            k=k)
    return(data)
}

run <- function(data) {
    list_train<-data$train_set
    train_n <- length(list_train)
    list_test<-data$test_set
    test_n <- length(list_test)
    clusters<- data$clusters
    k <- data$k
    cat('k-NN: k =', k,', Category =', clusters, ', Train =', train_n, ', Test =', test_n, '\n')
    
    #outer loop, map function for each test
    
    V_kNN.fun <- function(test_item) {
        #calculate the distance to all 
        dists.fun <- function(train_item) {
            rowSums((va_repVecData(train_item$val, test_item) - test_item$val)^2)
        }
        
        dists_list <- lapply(list_train, dists.fun)
        #change to dists_vec, and do the sorting
        
        mink.indices <- va_list2vec(va_vecApplyWrapper(dists_list, order))
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
    print(summary(test_cl))
}


if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}