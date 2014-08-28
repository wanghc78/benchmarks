library(compiler)


vecLen <- function(v) { #v must be vec, or SoA
    if(is.list(v)){
        return(vecLen(v[[1]]))
    }
    
    v_dim <- dim(v) 
    if(is.null(v_dim)) {
        length(v)
    } else {
        v_dim[1]
    }
}
vecLen <- cmpfun(vecLen)


#X includes "1" column, Y column vec
vecData <- function(v, ref) {
    #need calculate the ref's length
    n <- vecLen(ref)
    tmp <- rep(v, each=n)
    #now change the shape
    v_len <- length(v)
    if(v_len > 1) {
        if(is.array(v)) {
            dim(tmp) <- c(n, dim(v))
        } else {
            dim(tmp) <- c(n, v_len)
        }
    }
    tmp
}
vecData <- cmpfun(vecData)


colVecData <- function(v, ref) {
    #need calculate the ref's length
    n <- vecLen(ref)
    if(is.array(v)) {
        #very complex
        tmp<-apply(v, 2, function(acol){rep.int(acol, n)})
        #tmp<-rep(v, each=n)
        dimV <- dim(v)
        dimSz <- length(dimV)
        dim(tmp) <- c(dimV[1], n, dimV[2:dimSz])
    } else {
        #scalar or array
        tmp <- rep.int(v, n)
        v_len <- length(v)
        if(v_len > 1) {
            dim(tmp) <- c(v_len, n)
        }
    }
    tmp
}
colVecData <- cmpfun(colVecData)

vec2list <- function(v) {
    if(is.list(v)) {
        v_len <- length(v)
        v_attr <- attributes(v)
        #transform each elements into list
        for(i in 1:v_len){
            v[[i]] <- vec2list(v[[i]])
        }
        l_len <- length(v[[i]])
        #then compose small one
        aos <- lapply(1:l_len, function(i){ 
                    e <- list()
                    for(ei in 1:v_len){
                        e[[ei]] <- v[[ei]][[i]]                  
                    }
                    attributes(e) <- v_attr
                    e
                })
        return(aos)
    }
    
    
    #only support v as vector or matrix
    v_dim <- dim(v) 
    if(is.null(v_dim)) {
        n <- length(v)
        lapply(1:n, function(i) v[i])
    } else {
        n <- v_dim[1]
        lapply(1:n, function(i) v[i,])
    }   
}
vec2list<-cmpfun(vec2list)

list2vec <- function(l) {
    #assert
    stopifnot(length(l) > 0)
    e1 <- l[[1]] #pick the first one
    if(is.list(e1)) {
        len_e <- length(e1)
        soa <- list() #the result
        for(i in 1:len_e) {
            soa[[i]] <- list2vec(lapply(l, function(e){e[[i]]}))
        }
        attributes(soa) <- attributes(e1)
        return(soa)
    }
    
    #then e1 is atomic type
    #only support v as vector or matrix
    tmp <- simplify2array(l)
    v_dim <- dim(tmp) 
    if(is.null(v_dim)) {
        tmp
    } else { #matrix
        t(tmp)
    }
}
list2vec<-cmpfun(list2vec)

getByIdx <- function(v, idx) {
    #if v is a vector, return [idx]
    #if v is a matrix, return [,idx]
    #if v is a 3D arra, [,,idx]
    v_dim <- dim(v) 
    if(is.null(v_dim)) {
        v[idx]
    } else {
        if(length(v_dim) == 2) {
            v[,idx]
        } else {
            v[,,idx]
        }
    }
}
getByIdx <- cmpfun(getByIdx)

# a very simple way to vectorize a function.
rawVecFun <- function(fun) {
    #input a fun, return the fun that can support vector input
    function(v) {
        if(is.array(v)) { 
            apply(v, 1, fun)
        } else {
            sapply(v, fun)
        }
    }
}
rawVecFun <- cmpfun(rawVecFun)

