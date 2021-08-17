genDat2 <- function(sim.Rounds){
    print(class(sim.Rounds[[1]]))
    print(length(sim.Rounds[[1]]))

    print(head(sim.Rounds[[1]]))
    
    dat10 <- bind_rows(sim.Rounds[[1]])
     rownames(dat10) <- 1:dim(dat10)[1]

    datcols <- c("nodeID", "round", "type",
                 "nodeIdeo", "i2") 

    print(head(dat10))

    dat10 <- dat10 %>%
        select(any_of(datcols)) ## any_of() call will keep it when there isn't an i2


    ## Want to make a round1 aboveT:                                                                       
    sim.Rounds[[1]][[1]]$aboveT <- ifelse(
        sim.Rounds[[1]][[1]]$nodeIdeo >
        sim.Rounds[[1]][[1]]$nodeThresh, 1, 0)

    dat10 <- bind_rows(sim.Rounds[[1]])
    return(dat10)
}
