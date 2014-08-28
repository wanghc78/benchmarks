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

    library(class) #use built-in knn
    list_train<-data$train_set
    train_n <- length(list_train)
    list_test<-data$test_set
    test_n <- length(list_test)
    clusters<- data$clusters
    
    cat('NN: Category =', clusters, ', Train =', train_n, ', Test =', test_n, '\n')
    
    #change list_train into matrix
    train <- t(sapply(list_train, function(item){item$val}))
    train_cl <- factor(sapply(list_train, function(item){item$label}))
    test <- t(sapply(list_test, function(item){item$val}))
    test_cl <- knn1(train, test, train_cl)

    #the raw data
    test_labels <- attr(test_cl, "levels")
    #finally change the test data to attach the label
    out_list_test <- lapply(1:test_n, function(i){
                                 item<-list_test[[i]]
                                 item$label<- test_labels[test_cl[i]]
                                 item
                             })
    print(summary(test_cl))
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
