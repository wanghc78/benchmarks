# Monte-Carlo Pi - lapply based solution with manual vecapply
# 
# Author: Haichuan Wang
###############################################################################
app.name <- 'Pi_lapply'
source('setup_Pi.R')
library(vecapply)

run <- function(S) {
    
    #X includes "1" column, Y column vec
    V_sample.func <- function(V_aSample) {
        ifelse((V_aSample[,1]^2+V_aSample[,2]^2 < 1), 1, 0) 
    }
    
    #vecData <- list_data
    vecData <- va_list2vec(S)
    vecSampleOut <- V_sample.func(vecData)
    
    reduceCount <- sum(vecSampleOut)
    mcPi <- 4.0 * reduceCount / length(S) 
    
    cat('Pi = ', mcPi, '\n');
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
