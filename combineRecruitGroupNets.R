### After initialize the
### Recruit and Group Networks
## Then combine them into one large
## ajacency matrix + node database

library(igraph)
library(dplyr)
library(ggplot2)
library(tidyr)

####################
## Load helper function(s)
###################

#################
## initial networks
################
## Has: nodeAttributes, an NxM matrix of node attributes
## and adjMatrix, an NxN directed, weighted, adj matrix
## recruits
bothNets <- function(initRecruits, initGroup){
    ## Combine nodes
    
    ## Make a dataframe of all node attributes:
    ## but not ties
    
    allnodes <- rbind(initRecruits$nodeAttributes,
                      initGroup$nodeAttributes)
    
    print("checkpoint 1")
    ## Join Adjacency Networks
    
    ## Combined Adj Matrix
    ## build matrix frame:
    
    num.recruits <- dim(initRecruits$adjMatrix)[1]
    num.group <- dim(initGroup$adjMatrix)[1]
    
    
    frame1 <- matrix(0,nrow=num.recruits,
                     ncol= num.group)
    frame2 <- matrix(0,nrow=num.group,
                     ncol=num.recruits)
    
    recruits.frame <- cbind(initRecruits$adjMatrix, frame1)
    group.frame <- cbind(frame2, initGroup$adjMatrix)
    
    dim(recruits.frame)
    dim(group.frame)

        print("checkpoint 2")
    ## sum:
    ties.tot <- rbind(recruits.frame, ##original
                      group.frame) ## group joined
    
    rownames(ties.tot) <- allnodes$nodeID
    colnames(ties.tot) <- allnodes$nodeID
    
    ## Convert to network
    
    gg <- graph_from_adjacency_matrix(ties.tot,
                                      mode=c("directed"),
                                      weighted=TRUE,
                                      add.colnames=TRUE)
    
    V(gg)$name <- allnodes$nodeID ## node ID for vertex name
    
    network.el <-  as_long_data_frame(gg)

    print("checkpoint 3")
    
    ## Just need: from, to, tie/attention weight
    tokeep <- c("weight", "from_name", "to_name")
    network.el <- network.el[,tokeep] ## edgelist
    
    tmp1 <- network.el %>% ## expand weights 
        tidyr::uncount(weight) ## to produce one row per tie

    
    tmp1$tieID <- paste0("t",1:nrow(tmp1))

        print("checkpoint 4")
    ## add node-level attributes (type)
    ##  sender node type:
    tmp <- merge(x=tmp1,
                 y=allnodes[,c("nodeID", "type")],
                 by.x="from_name",
                 by.y="nodeID",
                 all.x=TRUE)
    
   
    colnames(tmp) <- c("from", "to",
                       "tieID", "from.type")
    
    ## reciever node type:
    expanded.el <- merge(x=tmp,
                         y=allnodes[,c("nodeID", "type")],
                         by.x="to",
                         by.y="nodeID",
                         all.x=TRUE)
    
    colnames(expanded.el) <- c("from", "to",
                               "tieID", "from.type",
                               "to.type")
    
    bothNets <- list(nodeInformation=allnodes,
                     edgelist=expanded.el)
    
    
    return(bothNets)
}
