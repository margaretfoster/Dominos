## This doesn't need to be a standalone, but I am doing so for elegance of the eventual simulation script


updateThreshs <- function(nodedf, ideologyvar, threshvar){ 
    aboveT <- ifelse(nodedf[,ideologyvar] > nodedf[,threshvar], 1, 0) ## This matters for recruits to join                  
    belowT <- ifelse(nodedf[,ideologyvar] < nodedf[,threshvar], 1, 0) ## this matters for group members to leave
    return(list(above=aboveT, below=belowT)) ## Returns a pair of vectors, to attach to df
}
