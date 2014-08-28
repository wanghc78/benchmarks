# Nearest Neighbor
# 
# Input 3-dim points, 10 categories, 'C1' to 'C10'.
#   Traing: 10K. 
#   Testing: 10K
# 
# Author: Haichuan Wang
###############################################################################

setup <- function(args=c('10000', '1000', '10')) {
    train_n<-as.integer(args[1])
    if(is.na(train_n)){ train_n <- 10000L }
    
    test_n<-as.integer(args[2])
    if(is.na(test_n)){ test_n <- 1000L }   
    
    clusters<-as.integer(args[3])
    if(is.na(clusters)){ clusters <- 10L }
    
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
            clusters=clusters)
    return(data)
}

run <- function(data) {

    list_train<-data$train_set
    train_n <- length(list_train)
    list_test<-data$test_set
    test_n <- length(list_test)
    clusters<- data$clusters
    
    cat('NN: Category =', clusters, ', Train =', train_n, ', Test =', test_n, '\n')
    
    #outer loop, map function for each test
    
    NN.fun <- function(test_item) {
        #calculate the distance to all 
        dists.fun <- function(train_item) {
            sum((train_item$val - test_item$val)^2)
        }
        
        dists <- lapply(list_train, dists.fun)
        #get the which min
        min.train <- which.min(dists)
        #get the category
        test_item$label <- (list_train[[min.train]])$label
        test_item
    }
    
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
