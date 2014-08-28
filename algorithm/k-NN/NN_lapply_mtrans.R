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
    source('../vecutil.R')
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
    
    V_NN.fun <- function(V_test) {
        #calculate the distance to all 
        V_dist.func<-function(V_train){
            rowSums((V_train$val - vecData(test_item$val, V_train$val))^2)
        }
        
        VV_dist.func<-function(V_train) {
            rowSums((colVecData(V_train$val, V_test$val)
                    - vecData(V_test$val, V_train$val))^2, dims=2)
        }
        
        VV_dists <- VV_dist.func(vec_train)
        #get the which min
        V_min.train <- apply(VV_dists, 2, which.min)
        #get the category
        V_test$label <- (vec_train$label)[V_min.train]
        V_test
    }
    #note moved here
    vec_train<-list2vec(list_train) #vec_train$val vec_train$label  
    vec_test<-list2vec(list_test)
    out_vec_test<- V_NN.fun(vec_test) #it has a new $label as vector
    out_list_test <- vec2list(out_vec_test) #change back
    
    #get the cl
    test_cl_vec <- sapply(out_list_test, function(test_item){test_item$label})
    test_cl <- factor(test_cl_vec)
    print(summary(test_cl))
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
