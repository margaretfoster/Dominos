
## Break into:
## Pt 1: ideology updator
## Send back out allnodes 

callIdeoUpdators <- function(bothNets, rounds, rseed,
                     rshock=NULL, pts){

    source("ideologyUpdator.R")
    source("updateThreshs.R")
    source("rewireTies.R")
    source("cutToLeave.R")
    
    ## step 0: how many rounds:
    R = rounds
    r.seed = rseed

    set.seed(r.seed)
    if(pts > 1 | pts < 0){ print("need shock propotion to be in [0,1]")}                 
    
    ##  R1 = initialized network that is passed in from bothNets:

    allnodes <- list()
    edge.list <- list()
   
    allnodes[[1]] <- bothNets$nodeInformation
    allnodes[[1]]$nodeIdeo <- as.numeric(allnodes[[1]]$nodeIdeo)
    allnodes[[1]]$nodeThresh <- as.numeric(allnodes[[1]]$nodeThresh)
    allnodes[[1]]$egoW <- as.numeric(allnodes[[1]]$egoW)

    allnodes[[1]]$round <- 1 ## ID that as the first round
     
    edge.list[[1]] <- bothNets$edgelist 

    head(allnodes[[1]])

    ## For rounds 2 through R:
    ## For each round after the first:

    for(r in 2:R){
        print("in case A")
        print(paste0("round ", r))
        ## 0- Take the previous round's edgelist and make that an adjmat:
        gg <- graph_from_data_frame(edge.list[[r-1]],
                                    directed=TRUE)
        
        am <- as_adj(gg, sparse=FALSE)
        
        ## 1- Update the ideology:
        ## Three cases: round 2, after shock round, base
        
        if(r == 2){ ## Look to the initial updator
            nodeIdeoUpdate <- ideologyUpdator(adjMatrix=am,
                                              nodeIdeology= as.numeric(allnodes[[r-1]]$nodeIdeo),
                                              egoWeights = as.numeric(allnodes[[r-1]]$egoW))
            
            allnodes[[r]] <- cbind(allnodes[[r-1]], nodeIdeoUpdate)
        }else{
            if(r ==(rshock+1)){ ## is it a shock round?
                ## the "shock" is put in the previous round ideology
                ## and affects connected nodes in the rshock round
                
                ## Identify length of nodes that are tagged with "r" (started as recruit)
                starting.r.nodes <- grep(x=allnodes[[r-1]]$nodeID, pattern="r")
                num.rn <- length(starting.r.nodes)
                num.to.shock <- floor(num.rn*pts)
                
                sel <- sample(starting.r.nodes, ## choose which nodes to shock
                              size=num.to.shock,
                              replace = FALSE)
                
                nodes.ts <- allnodes[[r-1]]$nodeID[sel]
                
                print(paste0("ideology shock for ", nodes.ts))
                
                ## Shock: cut ideology score in half
                allnodes[[r-1]][which(allnodes[[r-1]]$nodeID %in%
                                      nodes.ts),"nodeIdeoUpdate"] <-
                                          allnodes[[r-1]][which(allnodes[[r-1]]$nodeID %in%
                                                                nodes.ts),"nodeIdeoUpdate"]/2
                print("finished shock")
                print("post-shock ideo update")
                ## Then ideo update:
                nodeIdeoUpdate <- ideologyUpdator(adjMatrix=am,
                                                  nodeIdeology=as.numeric(
                                                      allnodes[[r-1]]$nodeIdeoUpdate),
                                                  egoWeights= as.numeric(
                                                      allnodes[[r-1]]$egoW))
                allnodes[[r]] <- allnodes[[r-1]]  ## previous info
                allnodes[[r]]$nodeIdeoUpdate <- nodeIdeoUpdate

                print("Shock round ideo update Done")
                
            }else{
                ## No shock, ideo update:
                ## If not Round 2, do ideology update::
                nodeIdeoUpdate <- ideologyUpdator(adjMatrix=am,
                                                  nodeIdeology=as.numeric(
                                                      allnodes[[r-1]]$nodeIdeoUpdate),
                                                  egoWeights= as.numeric(
                                                      allnodes[[r-1]]$egoW))
                allnodes[[r]] <- allnodes[[r-1]]  ## previous info
                allnodes[[r]]$nodeIdeoUpdate <- nodeIdeoUpdate

                print("Ideology update Done")
            }
        }

        ## Part Two: Edges
        
        ## 2 - Identitify which nodes are above/below the threshold:
        threshsR2 <- updateThreshs(nodedf=allnodes[[r]],
                                   ideologyvar="nodeIdeoUpdate",
                                   threshvar= "nodeThresh")
        
        allnodes[[r]]$aboveT <- threshsR2$above
        allnodes[[r]]$belowT <- threshsR2$below

        save(allnodes[[r]],
             file="mostRecentallnodes.Rdata")

        print(allnodes[[r]])
        
        ##Node updating:
        ## 3- Move nodes based on position wrt to threshold:

        ## 3.a: Nodes to rewire:
        ## list of nodes above their threshold and not in group already:        
        nodes.to.join <- na.omit(as.character(allnodes[[r]][(allnodes[[r]]$aboveT==1 &
                                                             allnodes[[r]]$type==
                                                             "recruit"),
                                                            "nodeID"])) ## char
        print(paste0("Round ", r, " nodes to join are ", nodes.to.join))

        ## 3b: Nodes to move out
        ## list of nodes below their thresholds and in group:
        nodes.to.leave <- na.omit(as.character(allnodes[[r]][(allnodes[[r]]$belowT ==
                                         1 &
                                         allnodes[[r]]$type==
                                         "group"),
                                        "nodeID"]))
        
        print(paste0("Round ", r, " nodes leaving group are ",nodes.to.leave))

        print("length of nodes to join is: ")
        print(length(nodes.to.join))
        
        print("length of nodes to leave is: ")
        print(length(nodes.to.leave))

        ## JOIN
        ## CASE 1: no nodes to join:
        
        if(length(nodes.to.join) == 0){## no nodes to join group
            print("No nodes to join, using previous edgelist")
            t2.updated <- edge.list[[r-1]] ## no change to edgelist
            ## pass that into the next stage, which is nodes to leave
            
        }
        
         ## CASE 2: There are nodes to join::      
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

            ## update changed node identities:           
            allnodes[[r]][which(allnodes[[r]]$nodeID
                                %in%
                                nodes.to.join),
                          "type"] <- "group"
            print("Updated node information")
        }
        
        dim(t2.updated)
        
        ## Leave
        ## Case 1: No nodes to leave
        if(length(nodes.to.leave) == 0){## no nodes to leave group
            print("no nodes to leave, returning previous edgelist")
            t2.updated <- edge.list[[r-1]] ## no change to edgelist
        }

        ## Case 2: Nodes to leave:
        if(length(nodes.to.leave) > 0){
            print("Making disconnect")
                    
            ## leave:
            g.out <- cut.to.leave(edge.list=edge.list[[r-1]],
                                  node.list=nodes.to.leave,
                                  r.seed,
                                  round=0)

            print("g.out is")
            print(g.out)
            
            ## Update the updated edgelist:
            ## Same edglist from the "join" output
            ## B/c lost ties are group-group
            ## And new ties are recruit-group
            if(g.out==NA){ ## if no departing nodes
                t2.updated <- t2.updated
                print("finished removing group ties, no cut nodes")
            }else{                
                t2.updated <- t2.updated[-which(t2.updated$tieID %in%
                                                g.out),] ## list of ties
                print("finished removing group ties, cut nodes")
                
                ## Update any changed node identities:
                allnodes[[r]][which(allnodes[[r]]$nodeID
                                    %in%
                                    nodes.to.leave),
                              "type"] <- "out"
                
                print("finished updating node information")
            }

            allnodes[[r]]$round <-  r
            ## 4- Record updated edgelist:
            edge.list[[r]] <- t2.updated
                
        }
    }

        results <- list(nodeSims = allnodes,
                        edgeSims = edge.list)
         
        return(results)
    
}
