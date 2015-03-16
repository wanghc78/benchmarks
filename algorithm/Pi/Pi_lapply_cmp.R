# Monte-Carlo Pi - lapply based solution with vecapply
# 
# Author: Haichuan Wang
###############################################################################
app.name <- 'Pi_lapply_cmp'
source('setup_Pi.R')
library(vecapply)

run <- function(S) {
    ptm <- proc.time() #previous iteration's time
    
    #X includes "1" column, Y column vec
    sample.func <- function(aSample) {
        if((aSample[1]^2 + aSample[2]^2) < 1) {
            1.0
        } else {
            0.0
        }
    }
    
    sampleOut <- lapply(S, sample.func)
    
    reduceCount <- Reduce('+', sampleOut)
    mcPi <- 4.0 * reduceCount / length(S)
    
    cat("[INFO]Time =", (proc.time()-ptm)[[3]], '\n')
    cat('Pi = ', mcPi, '\n');
}

run <- va_cmpfun(run)

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
