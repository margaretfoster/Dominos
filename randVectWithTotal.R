## Random vectory with a specific total:                                      
## used to randomly assign ties                                               
## Thanks, stack overflow!


rand.vect.with.total <- function(min, max, total) {
    ## generate random numbers
    x <- sample(min:max, total, replace=TRUE)
    ## count numbers
    sum.x <- table(x)
    ## convert count to index position                    
    out = vector()
    for (i in 1:length(min:max)) {
        out[i] <- sum.x[as.character(i)]
    }
    out[is.na(out)] <- 0
    return(out)
}


