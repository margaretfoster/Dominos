
## This is the workhorse function that applies callLoop and GenDat
## to trace network-based ideology updating

callEvo <- function(panel, n, 
                    bothNets, rseed){
    shocks.list <- list() ## for nodes each round          
    rounds.list <-list() ## for edgelist each round
    
    for(p in panel){
        j=p*10
        print(paste0("in panel ", j, "percent shock"))
        sh <- callLoop(bothNets=bN[[n]],
        round=10,
        rshock= 6, pts=p, rseed=rseed)
        dt <- genDat(sh)
        rownames(dt) <- NULL
        dt$shockpercent <- j*10
        dt$iteration <-n ## this is which network                                                                                 
        shocks.list[[j]] <- dt ## this is node  information                                                                       
        rounds.list[[j]] <- sh ## this will have the edgelists for network viz and change in edges                                
    }
    outlist <- list(shocks.list, rounds.list)
    return(outlist)
}
