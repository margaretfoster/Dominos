
##source("ideologyUpdatorDebug.R")
source("updateThreshs.R")
source("rewireTies.R")
source("cutToLeave.R")

## This takes allNodes with the updated node values and thresholds
## then does the edge permutation for those nodes

## returns updated allNodes and Edge.List lists

callEdges <- function(allnodes,## list
                      edge.list,## list
                      r,## current round
                      rseed){ ## seed
    ## Part Edges
    ##Node updating:
    ## 3- Move nodes based on position wrt to threshold:
    
    ## 3.a: Nodes to rewire:
    ## list of nodes above their threshold and not in group already:        
    nodes.to.join <- na.omit(as.character(allnodes[[r]][(allnodes[[r]]$aboveT==1 &
                                                         allnodes[[r]]$type==
                                                         "recruit"),
                                                        "nodeID"])) ## char
    ## print(paste0("Round ", r, " nodes to join are ", nodes.to.join))
    
    ## 3b: Nodes to move out
    ## list of nodes below their thresholds and in group:
    nodes.to.leave <- na.omit(as.character(allnodes[[r]][(allnodes[[r]]$belowT ==
                                                          1 &
                                                          allnodes[[r]]$type==
                                                          "group"),
                                                         "nodeID"]))
    
    ##print(paste0("Round ", r, " nodes leaving group are ",nodes.to.leave))
    
    ##print("length of nodes to join is: ")
    ##print(length(nodes.to.join))
    
    ##print("length of nodes to leave is: ")
    ##print(length(nodes.to.leave))
    
    ## JOIN
    if(length(nodes.to.join) == 0){## no nodes to join group
        ##print("No nodes to join, using previous edge.list")
        if(length(nodes.to.leave)== 0){
            ## print("No nodes to join or leave, returning previous")
            allnodes[[r]] <- allnodes[[r-1]]
            edge.list[[r]] <- edge.list[[r-1]]
            allnodes[[r]]$round <-  r
            ## & back to here for a send-out
        }else{ ## close brace closes case of both nodes to join and to leave = 0
        t2.updated <- edge.list[[r-1]] ## no change to edge.list
        ## pass that into the next stage, which is nodes to leave
  } ## closes nodes to join = 0 but nodes to leave !=
    }else{ ## case of size nodes.to.join > 0; bracket closes length of nodes to join is 0
            print("Making new affiliations")
            
            l1 <- rewire.join(edge.list=edge.list[[r-1]],
                              node.list=nodes.to.join,
                              r.seed=rseed,
                              round=r)
            
            
            ## Update the edge.list df:
            ## t2.cuts embeds the unchanged ties:
            t2.cuts <- edge.list[[r-1]][-which(
                edge.list[[r-1]]$tieID %in%
                l1$Ties.To.Cut),]
            
            ## t2.new adds the new ties:

            if(dim(l1$Ties.To.Make)[1]==0){
                ## make a new empty df:
                t2.new <-data.frame(matrix(ncol=5,nrow=0,
                                           dimnames=list(NULL, c("from", "to", "tieID",
                                               "from.type", "to.type")))) ## still going to be empty
            }else{## close brace closes l1 is empty:
                t2.new <- cbind(l1$Ties.To.Make,
                                from.type="group", to.type="group")
                
            }##close brace closes if ties.to.make is not empty
            ##print(head(t2.new))
            ## Updated Edge.List:
            ## fields: from, to, from.type, to.type:
            t2.updated <- rbind(t2.cuts, t2.new)
            
            print("finished making new ties")
            
            ## update changed node identities:           
            allnodes[[r]][which(allnodes[[r]]$nodeID
                                %in%
                                nodes.to.join),
                          "type"] <- "group"
            print("Updated node information")
        } ## closes case of size nodes.to.join !=0 clause

    ## LEAVE
        ## Nodes to Leave
        ## Case 1: No nodes to leave
        if(length(nodes.to.leave) == 0){## no nodes to leave group
            print("no nodes to leave, returning previous edgelist")
            t2.updated <- edge.list[[r-1]] ## no change to edgelist
            print("head of t2 updated prev edgelist")
            print(head(t2.updated))
        }else{ ## case of nodes.to.leave > 0; bracket closes length of nodes to leave == 0
            print("Making disconnect")
            
            ## leave:
            g.out <- cut.to.leave(edge.list=edge.list[[r-1]],
                                  node.list=nodes.to.leave,
                                  r.seed=r.seed,
                                  round=0)
           
            print("the debug:")
            print(g.out)
            ## Update the updated edgelist:
            ## Same edglist from the "join" output
            ## B/c lost ties are group-group
            ## And new ties are recruit-group
            
            if(is.na(g.out[1])=="none"){ ## if no departing nodes
                t2.updated <- t2.updated
                print("finished removing group ties, no cut nodes")
            }else{## bracket closes g.out is NA clause
                
                t2.updated <- t2.updated[-which(t2.updated$tieID %in%
                                                 g.out),] ## list of ties
                t2.updated
                
                print("finished removing group ties, cut nodes")
                
                ## Update any changed node identities:
                allnodes[[r]][which(allnodes[[r]]$nodeID
                                    %in%
                                    nodes.to.leave),
                              "type"] <- "out"
                
                print("finished updating node information")
            }## bracket closes g.out !=NA
        } ## bracket closes nodes.to.leave > 0 case

    

    ##Prepare the updated allnodes and edgelist:
    allnodes[[r]]$round <-  r
    
    ## 4- Record updated edgelist:
    edge.list[[r]] <- t2.updated

    results <- list(nodeSims = allnodes,
                    edgeSims = edge.list)
   
    return(results)
} ## brace ends the function
