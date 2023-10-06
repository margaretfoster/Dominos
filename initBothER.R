#################
## init network of potential recruits
################

normalize <- function(x){
 tmp =  (x - min(x)) / (max(x) - min(x))
return(tmp)
}

initER<- function(r.seed,## integer, random seed
                  num.nodes, ##integer, number nodes
                  id.letter,## character, for index
                  type,## character, for classification 
                  init.ideo.high, ## decimal, [0,1]
                  init.ideo.low,
                  lower.bound.thresh,
                  upper.bound.thresh){

    set.seed(r.seed)
    ## Node-level variables:    
    nodes <- 1:num.nodes ## 1: make nodes

    ## 3: Node Ideologies Time T0:
    set.seed(r.seed)
    node.ideo <- round(runif(min=init.ideo.low,
                             max=init.ideo.high,
                             n=num.nodes),2)
    node.ideo
    
    ## Node Thresholds
    ## First iteration: random thresholds
    ## can make theory-based
    
    nodeThreshs <- round(runif(min=lower.bound.thresh,
                               max=upper.bound.thresh,
                               n=num.nodes),
                         2)
    
    nodeThreshs

    ## Ties time T0
    
    ## Want the network to be directed                        
    ## because that will make it easier to cut the ties later 
    ## but means that I need to confirm that the updator only
    ## operates on outgoing/incoming ties
    ## Connecting the updator to outgoing ties is a
    ## pragmatic choice; but means that the ties
    ## substantively represent something like
    ## "attention paid [to connected node]"
    
    library(igraph)
    
    ## this will keep the density of the network even if it won't map onto a vector of ties
    ## approximately the 1-9 ties that I was thinking of:
    
    g1 <- erdos.renyi.game(num.nodes,
                           p.or.m=(num.nodes*3),## number of nodes x 3
                           ## so average of 3 outgoing ties
                           type="gnm",
                           directed=TRUE)
    
    ## Need weights:
    V(g1)$outdeg <- degree(g1, mode=c("out"))
    E(g1)$weight <- ceiling(runif(length(E(g1)), 1, 4))
       
    am1 <- as_adjacency_matrix(g1,
                               sparse=FALSE,
                               attr="weight")
    ## Ego Weight
    
    ## Egoweight is a runif variable between [.25,.75]
    ## reflecting that everyone has some tendency to update and take their own council
    set.seed(r.seed)
    node.eweights <- round(runif(min=.25,
                               max=.75,
                               n=num.nodes),2)
    
    ## previous EW based on number of out-ties. Might be a problem
    ##    ew.base <- rowSums(am1) ## egoweights derived from initial out-ties
    ##    node.eweights <- round(normalize(ew.base),2)

    ## helper function to make ego weights [0,1]                            
    ## Make DF of node-level attributes
    ## To work from
    ## make general node ids:
    r.nodeIDs <- paste0(id.letter, 1:num.nodes)
    
    init.nodes <- as.data.frame(cbind(r.nodeIDs,
                                      type,
                                      node.eweights,
                                      node.ideo,
                                      nodeThreshs))
    
    names <- c("nodeID", "type", "egoW",
               "nodeIdeo", "nodeThresh")
    colnames(init.nodes) <- names
    
    initRecruits=list(nodeAttributes=init.nodes,
        adjMatrix=am1)
    
    return(initRecruits) 
}