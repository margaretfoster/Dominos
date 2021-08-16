## This script to initialize the group network
## Heirarchical random graph/influence leader model
#####################
## Node-level variables
#####################

## make a function to vary the type of Group network

## takes: seed, size, type

initPA <- function(r.seed,
                   num.nodes,
                   init.ideo.high,
                   id.letter,
                   type,
                   init.ideo.low){
    
    ##Group Node Ideologies Time T0:
    
    set.seed(r.seed)
    group.ideo <- round(runif(min=init.ideo.low,
                          max=init.ideo.high,
                          n=num.nodes),2)
    ##print(group.ideo)
    
    ## Group affiliation Thresholds
    ## Lower for group members?
    ## Probably better to keep thresholds comperable
    ## and give group members higher initital

    nodeThreshs <- round(runif(n=num.nodes,
                               min = .6,
                               max = 1),2)
    ##print(nodeThreshs)    
    ##Init Adjmat small world
    ## TO HERE

    library(igraph)
    set.seed(r.seed)
    
    init.net <- sample_pa(power=1,
                          n=num.nodes,
                          algorithm=c("psumtree-multiple"), ## rewiring probability
                         directed=FALSE,
                          out.pref=TRUE)
    
    E(init.net)$weight <- ceiling(runif(length(E(init.net)), 1, 4))

    ##plot(init.net)
    
    ## Ego weights

    adjmat.group <- as_adjacency_matrix(init.net,
                                        sparse=FALSE,
                                        attr="weight")
    
    set.seed(r.seed)
    node.eweights <- round(runif(min=.25,
                                 max=.75,
                                 n=num.nodes),2)  
####################
## Node attribute DF
#####################

    ## nodeIDs
    
    g.nodeIDs <- paste0(id.letter, 1:num.nodes)
    
    r0.group <- as.data.frame(cbind(g.nodeIDs,
                                 type,
                                    node.eweights,
                                    group.ideo,
                                    nodeThreshs))
    
    
    names <- c("nodeID", "type", "egoW",
               "nodeIdeo", "nodeThresh")
    colnames(r0.group) <- names
    
    initGroup=list(nodeAttributes=r0.group,
        adjMatrix= adjmat.group)
    
    return(initGroup)
}
  
