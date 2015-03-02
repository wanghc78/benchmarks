


setup <- function(args=c('1000000', '10', '100')) {
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 1000000L }
    
    ndim <-as.integer(args[2])
    if(is.na(ndim)){ ndim <- 10L }
    
    niter<-as.integer(args[3])
    if(is.na(niter)){ niter <- 100L }
    
    cat('[INFO][', app.name, '] n=', n, ', ndim=', ndim, ', niter=', niter, '\n', sep='')
    
    X<- matrix(runif(n*ndim, -1, 1), nrow=ndim, ncol=n) 
    theta <- rep(1,ndim)
    Y<- 1/(1+exp(-(1+colSums(theta*X)))) # now the coefficient are all 1
    YX <- lapply(1:n, function(i){c(Y[i],X[,i])})
    data <- list(YX=YX, ndim=ndim, niter=niter);
    
    return(data)
}