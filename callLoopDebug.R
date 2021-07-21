## For round N, takes:#
## Edgelist produced in round N-1
## and node attributes from round N-1
## Then update them

## bothNets is a list of node information in position 1
## and an edgelist in position 2
## R is number of rounds to simulate

rm(list=ls())

load("bothNetsTest.Rdata")
ls()

    ## Shock happens in round "rshock"
bothNets <- bN

###callLoop <- function(bothNets, rounds, rseed,
##                    rshock=NULL, pts){

source("ideologyUpdator.R")
source("updateThreshs.R")
source("rewireTies.R")
source("cutToLeave.R")
    
    ## step 0: how many rounds:
R = 10
r.seed = 408
pts=.3

set.seed(r.seed)
if(pts > 1 | pts < 0){
    print("need shock propotion to be in [0,1]")
}                 

##  R1 = initialized network that is passed in from bothNets:
allnodes <- list()
edge.list <- list()

## Port data objects, make sure they're numeric
allnodes[[1]] <- bothNets$nodeInformation
allnodes[[1]]$nodeIdeo <- as.numeric(allnodes[[1]]$nodeIdeo)
allnodes[[1]]$nodeThresh <- as.numeric(allnodes[[1]]$nodeThresh)
allnodes[[1]]$egoW <- as.numeric(allnodes[[1]]$egoW)
allnodes[[1]]$round <- 1 ## ID that as the first round

## edgelist
edge.list[[1]] <- bothNets$edgelist 

head(allnodes[[1]])
## For rounds 2 through R:
## For each round after the first:

##for(r in 2:R){

  ##  print(paste0("round ", r))
    
## 0- Take the previous round's edgelist and make that an adjmat:
r=2
rshock=5

library(igraph)

gg <- graph_from_data_frame(edge.list[[r-1]],
                                directed=TRUE)
    
am <- as_adj(gg, sparse=FALSE)
    
    ## 1- Update the ideology:
    ## If round 2:
    
##if(r == 2){ ## Look to the initial updator

am

## Figure out denominator on weights:

egoWeights <- allnodes[[r-1]]$egoW

attention.out <- (1-allnodes[[r-1]]$egoW)*10
total.weights <- (rowSums(am)*10)/attention.out
node.ego.weight <- total.weights*egoWeights

i2 <- NULL

for(d in 1:dim(adjMatrix)[1]){ ## for every row in the adjacency matrix    
    s1 <-  sum(adjMatrix[d,]*nodeIdeology)
    s2 <- node.ego.weight[d]* nodeIdeology[d]
    u <- (s1+s2)/total.weights[d] ## updated          
    i2 <- c(i2, u)
    }
   



nodeIdeoUpdate <- ideologyUpdator(adjMatrix=am,
                                  nodeIdeology= as.numeric(allnodes[[r-1]]$nodeIdeo),
                                  egoWeights = as.numeric(allnodes[[r-1]]$egoW))

allnodes[[r]] <- cbind(allnodes[[r-1]], nodeIdeoUpdate)


##}

## else{ ## First see if this is the shock round
##     if(r ==(rshock+1)){ ## the "shock" is put in the previous round ideology
##         ## and affects connected nodes in the rshock round
##         ## Identify length of nodes that are tagged with "r" (started as recruit)
##         starting.r.nodes <- grep(x=allnodes[[r-1]]$nodeID, pattern="r")
##         num.rn <- length(starting.r.nodes)
##         num.to.shock <- floor(num.rn*pts)
        
##         sel <- sample(starting.r.nodes, ## choose which nodes to shock
##                       size=num.to.shock,
##                       replace = FALSE)
        
##         nodes.ts <- allnodes[[r-1]]$nodeID[sel]
        
##         print(paste0("ideology shock for ", nodes.ts))
        
##         allnodes[[r-1]][which(allnodes[[r-1]]$nodeID %in%
##                               nodes.ts),"nodeIdeoUpdate"] <-
##                                   allnodes[[r-1]][which(allnodes[[r-1]]$nodeID %in%
##                                                         nodes.ts),"nodeIdeoUpdate"]/2
##         print("finished shock")
##     }
    
## Then move through:

r=3

nodeIdeoUpdate <- ideologyUpdator(adjMatrix=am,
                                  nodeIdeology=as.numeric(
                                      allnodes[[r-1]]$nodeIdeoUpdate),
                                  egoWeights= as.numeric(
                                      allnodes[[r-1]]$egoW))
allnodes[[r]] <- allnodes[[r-1]]  ## previous info
allnodes[[r]]$nodeIdeoUpdate <- nodeIdeoUpdate


}

        ## 2 - Identitify which nodes are above/below the threshold:
        threshsR2 <- updateThreshs(nodedf=allnodes[[r]],
                                   ideologyvar="nodeIdeoUpdate",
                                   threshvar= "nodeThresh")
        
        allnodes[[r]]$aboveT <- threshsR2$above
        allnodes[[r]]$belowT <- threshsR2$below

        ### Any other round node updating happens here:

        ## 3- Move nodes based on position wrt to threshold:

        ## 3.a: Nodes to rewire:
        ## list of nodes above their threshold and not in group:
        nodes.to.join <- allnodes[[r]][(allnodes[[r]]$aboveT==1 &
                                    allnodes[[r]]$type==
                                    "recruit"),
                                       "nodeID"] 
        print(paste0("Round ", r, " nodes to join are ", nodes.to.join))
        ## 3b: Nodes to move out
        ## list of nodes below their thresholds and in group:
        nodes.to.leave <- allnodes[[r]][(allnodes[[r]]$belowT ==
                                     1 &
                                         allnodes[[r]]$type==
                                         "group"),
                                        "nodeID"]
        
        print(paste0("Round ", r, " nodes leaving group are ",nodes.to.leave))
    
      
        ## Join:      
        if(length(nodes.to.join)> 0){
            print("Making new affiliations")
            
            l1 <- rewire.join(edge.list[[r-1]],
                              node.list=nodes.to.join,
                              r.seed,
                              round=0)
                        
            ## Update the edgelist df:
            ## t2.cuts embeds the unchanged ties:
            t2.cuts <- edge.list[[r-1]][-which(
                edge.list[[r-1]]$tieID %in%
                l1$Ties.To.Cut),]
            ## t2.new adds the new ties:
            t2.new <- cbind(l1$Ties.To.Make,
                            from.type="group", to.type="group")
            
            ## Updated Edgelist:
            ## fields: from, to, from.type, to.type:
            t2.updated <- rbind(t2.cuts, t2.new)
            
            print("finished making new ties")
        }
        
        dim(t2.updated)
        
        ## Leave
        
        if(length(nodes.to.leave) > 0){
            print("Making disconnect")
            
            g1 <- cut.to.leave(edge.list=edge.list[[r-1]],
                               node.list=nodes.to.leave,
                               r.seed,
                               round=0)
            
            
            ## leave::
            g.out <- cut.to.leave(edge.list=edge.list[[r-1]],
                                  node.list=nodes.to.leave,
                                  r.seed,
                                  round=0)
            
            ## Update the updated edgelist:
            ## Use the same edglist from the "join" output
            ## Because the lost ties are  group-group
            ## So won't counteract the new ties
            ## which are recruit-group
            
            t2.updated <- t2.updated[-which(t2.updated$tieID %in%
                                            g.out),] ## list of ties
            
            print("finished removing group ties")
        }
        
        ## If no difference:
        if(length(nodes.to.leave) == 0
           & length(nodes.to.join) == 0){
            
            print("No changes to ties")
            t2.updated <- edge.list[[r-1]] ## previous edgelist
        }
        
        dim(t2.updated) ## Updated edgelist

        ## 3- Record updated node attributes
        
        allnodes[[r]][which(allnodes[[r]]$nodeID
                            %in%
                            nodes.to.join),
                      "type"] <- "group"
        allnodes[[r]][which(allnodes[[r]]$nodeID
                            %in%
                            nodes.to.leave),
                      "type"] <- "out"

        allnodes[[r]]$round <-  r
        ## 4- Record updated edgelist:
        
        edge.list[[r]] <- t2.updated
    
    }
    results <- list(nodeSims = allnodes,
                    edgeSims = edge.list)
    return(results)

}
