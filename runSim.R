## runSim takes:
## bothNets = list of simulated networks
## rounds (which iteration), rseed (random seed), rshock round, and percent to shock parameters
## returns a list in which every network has a list of periods through the updating process
## this is the workhorse of the simulation:
runSim <- function(nodeInfo,
                   edgeInfo,
                   sim.length, rseed,
                   rshock=NULL, pts){
    source("ideologyUpdatorDebug.R")
    source("updateThreshs.R")
    source("callLoopEdges.R") ## edge rewiring
    ## step 0: How many simulation rounds:

    SL = sim.length ## length of the simulation
    r.seed = rseed
    
    ##Make sure the shock percent is reasonable
    if(pts > 1 | pts < 0){ print("need shock proportion to be in [0,1]")}                 
    
    ## Initialize some data structures
    print("starting runSim")
    allnodes <- list()
    edge.list <- list()

  
    ##%%%%%%%%%%%%%%%%%%%%%%
    ## Round 1: Initialize the first structure
    ## Rounds 2:SL
    ## For simulation round 2 through the upper limit of steps:
    ## For each round after the first:
    ##%%%%%%%%%%%%%%%%%%%%%% 

    allnodes[[1]] <- nodeInfo
    
    allnodes[[1]]$nodeIdeo <- as.numeric(allnodes[[1]]$nodeIdeo)
    allnodes[[1]]$nodeThresh <- as.numeric(allnodes[[1]]$nodeThresh)
    allnodes[[1]]$egoW <- as.numeric(allnodes[[1]]$egoW)
    allnodes[[1]]$round <- 1 ## ID as the first round
    
    edge.list[[1]] <- edgeInfo
    ##head(edge.list[[1]])
    
    for(s in 2:sim.length){ ## for each step in the simulation after 2:
        sim.step = s ## make the parameter more human-readable
        print(paste0("simulation step: ", sim.step)) 
        ## Make adjacency matrix of previous round configuration
        gg <- graph_from_data_frame(edge.list[[sim.step-1]],
                                    directed=TRUE)
        am <- as_adj(gg, sparse=FALSE)
        
        ## 1- Update the ideology:
        ## Three cases: round 2, after shock round, R>2 and not shock:
        
        if(sim.step == 2){ ## Corner Case:               
            ## in step 2 there here is not an "i2" column already
            nodeIdeoUpdate <- ideologyUpdator(adjMatrix=am,
                                              nodeIdeology= as.numeric(allnodes[[sim.step-1]]$nodeIdeo),
                                              egoWeights = as.numeric(allnodes[[sim.step-1]]$egoW))
            
            updated <- merge(allnodes[[sim.step-1]],
                             nodeIdeoUpdate,
                                 by="nodeID")
                
            ##            print(head(updated))
                allnodes[[sim.step]] <- updated
                                
                print("current length of allnodes is:")
                print(length(allnodes))
                print("current length of edge.list is:")
                print(length(edge.list))
                
                ## Edges:
                threshsR2 <- updateThreshs(nodedf=allnodes[[sim.step]],
                                           ideologyvar="i2",
                                           threshvar= "nodeThresh")
                
                
                allnodes[[sim.step]]$aboveT <- threshsR2$above
                allnodes[[sim.step]]$belowT <- threshsR2$below
                
                ## Update edges:
                tmp <- callEdges(allnodes=allnodes,
                                 edge.list=edge.list,
                                 r=sim.step,
                                 rseed=rseed)
            
            allnodes <- tmp$nodeSims
            edge.list <- tmp$edgeSims
            
            print("current length of allnodes is:")
            print(length(allnodes))
            print("current length of edge.list is:")
            print(length(edge.list))
            
            print("Edges Done")
             print("leaving r=2")
        }else{ #Close brace closes case sim.step==2
            if(sim.step ==(rshock+1)){ ## is it a shock round?
                print("test for not exist")
                if(exists(allnodes[[sim.step-1]])==FALSE){ ## end the sim if there isn't a previous round
                    break ## this gets the corner case where the shock crashes
                }## closes case of previous round doesn't exist
                
                print(colnames(allnodes[[sim.step-1]]))
                print(head(allnodes[[sim.step-1]]))

                if("i2" %in% colnames(allnodes[[sim.step-1]]) == FALSE){
                    print("updated ideology score doesn't exist, ending sim")
                    break
                }else{
                    
                starting.r.nodes <- grep(x=allnodes[[sim.step-1]]$nodeID,
                                         pattern="r") #look for nodes tagged recruit
                num.rn <- length(starting.r.nodes)
                num.to.shock <- floor(num.rn*pts)
                
                sel <- sample(starting.r.nodes, ## choose which nodes to shock
                              size=num.to.shock,
                              replace = FALSE)
                ##print(sel)
                nodes.ts <- allnodes[[sim.step-1]]$nodeID[sel]

                print("nodes.ts are")
                print(nodes.ts)

                print(paste0("ideology shock for ", nodes.ts))

                ##print("shock nodes are:")

                ##print(dim(allnodes[[sim.step-1]]))                
                ##print(allnodes[[sim.step-1]][which(allnodes[[sim.step-1]]$nodeID %in%
                  ##                                                    nodes.ts),"i2"])
                ## Shock: cut tideology score in half in previous round:
                ## (So that the effect is carried through in the "shock" round)
                 
                allnodes[[sim.step-1]][which(allnodes[[sim.step-1]]$nodeID %in%
                                      nodes.ts),"i2"] <-
                                          allnodes[[sim.step-1]][which(allnodes[[sim.step-1]]$nodeID %in%
                                                                       nodes.ts),"i2"]/2
                print("allnodes shocked")
            
                nodeIdeoUpdate <- ideologyUpdator(adjMatrix=am, ## post-shock ideo update 
                                                  nodeIdeology=as.numeric(
                                                      allnodes[[sim.step-1]]$i2),
                                                  egoWeights= as.numeric(
                                                      allnodes[[sim.step-1]]$egoW))
                print("shock ideo update done")
                updated <- merge(allnodes[[2]][, !names(allnodes[[2]]) %in% c("i2")],
                                 nodeIdeoUpdate,
                                 by="nodeID")
                
                allnodes[[sim.step]] <- updated
                
                ## Edges work starts here:
                threshsR2 <- updateThreshs(nodedf=allnodes[[sim.step]],
                                           ideologyvar="i2",
                                           threshvar= "nodeThresh")
                
                
                allnodes[[sim.step]]$aboveT <- threshsR2$above
                allnodes[[sim.step]]$belowT <- threshsR2$below
                
                ## Update edges:
                tmp <- callEdges(allnodes=allnodes,
                                 edge.list=edge.list,
                                 r=sim.step,
                                 rseed=rseed)
                
                allnodes <- tmp$nodeSims
                edge.list <- tmp$edgeSims
                print("current length of allnodes is:")
                print(length(allnodes))
                print("current length of edge.list is:")
                
                print(length(edge.list))
                print("Edges Done")
            }## closes the case of i2 exists in allnodes[[sim.step-1]]  
                
            }else{print("In case step > 2 and not shock") ## Close brace closes shock round
                  print(paste0("current simulation step is: ", sim.step))
                  print(dim(am))
                  
                  if(dim(am)[1]==0){
                   break ## end the step through because we're in a dead-end
               }else{
                   print(colnames(allnodes[[sim.step-1]]))
                   print(head(allnodes[[sim.step-1]]))
                   
                   if("i2" %in% colnames(allnodes[[sim.step-1]]) == FALSE){
                       print("updated ideology score doesn't exist, ending sim")
                       break
                   }## closes case of no i2 in allnodes[[sim.step-1]]
                      ## No shock, ideo update:
                  ## If not Round 2, do ideology update::
                  nodeIdeoUpdate <- ideologyUpdator(adjMatrix=am,
                                                    nodeIdeology=as.numeric(
                                                        allnodes[[sim.step-1]]$i2),
                                                    egoWeights= as.numeric(
                                                        allnodes[[sim.step-1]]$egoW))
                  
                  ## merge without a duplicated i2 column:
                  updated <- merge(allnodes[[sim.step-1]]
                                   [, !names(allnodes[[sim.step-1]])
                                    %in% c("i2")],
                                   nodeIdeoUpdate,
                                   by="nodeID")
                  
                  allnodes[[sim.step]] <- updated

                  
                  print("current length of allnodes is:")
                  print(length(allnodes))
                  print("current length of edge.list is:")
                  print(length(edge.list))
                  
                  ## Edges work starts here:
                  threshsR2 <- updateThreshs(nodedf=allnodes[[sim.step]],
                                             ideologyvar="i2",
                                             threshvar= "nodeThresh")
                  
                  
                  allnodes[[sim.step]]$aboveT <- threshsR2$above
                  allnodes[[sim.step]]$belowT <- threshsR2$below
                  
                  ## Update edges:
                  tmp <- callEdges(allnodes=allnodes,
                                   edge.list=edge.list,
                                   r=sim.step,
                                   rseed=rseed)
                  
                  allnodes <- tmp$nodeSims
                  edge.list <- tmp$edgeSims
            

                  print("current length of allnodes is:")
                  print(length(allnodes))
                  print("current length of edge.list is:")
                  
                  print(length(edge.list))
                  print("Edges Done")
              }## closes the clause dim(am)[1]!=0
                  ##print(allnodes[[sim.step]])
              } ## closes case SL>2 & not-shock
        }## close brace closes else clause of R!=2 
    } ## closes for s in 2:SL
    
    results <- list(nodeSims = allnodes,
                    edgeSims = edge.list)
    
    return(results)
} ## Ends the function
