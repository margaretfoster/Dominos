## This script to initialize the group network
#####################
## Node-level variables
#####################

## make a function to vary the type of Group network

## takes: seed, size, type

initSWGroup <- function(r.seed,
                        num.nodes,
                      init.ideo.high,
                        init.ideo.low,
                        id.letter,
                        type,
                      init.affil.high,
                      init.affil.low,
                      nei.init,
                      rw.prob){

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
                               max = .8),2)
    ##print(nodeThreshs)    
    ##Init Adjmat small world
    ## TO HERE

    library(igraph)
    set.seed(r.seed)
    init.net <- sample_smallworld(dim=1,
                                  size=num.nodes,
                                  nei=nei.init, ## neighborhood = num within which nodes connected
                                  p=rw.prob, ## rewiring probability
                                  loops = FALSE,
                                  multiple = TRUE)
    E(init.net)$weight <- ceiling(runif(length(E(init.net)), 1, 4))
    
    ## Ego weights

    adjmat.group <- as_adjacency_matrix(init.net,
                                        sparse=FALSE,
                                        attr="weight")
    

    ##
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
  
