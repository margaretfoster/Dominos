
rm(list = ls()) 

setwd("~/Dropbox/DominosPaper/Dominos-Code/code2022/")

library(dplyr)
library(ggplot2)
library(igraph)

## Load data:


"ER-PASims.Rdata"

## Format data:
## (Reconstruct the initial network statistics for the networks)

## Create initial centrality measure
## and number of rounds to entry into group

process <- function(filename, netstype){
  
  load(filename)
  netstype= netstype ## code network types
  
  exit= data.frame()
  entry= data.frame()
  alldat = data.frame()

  ## Reconstruct initial network statistics
  for(r in 1:length(node.traj)){
    print(r)
    tmp = bind_rows(node.traj[[r]])
    ## fallthrough if the node.traj object doesn't
  ## have 11 columns [indicates a corner case]
    if(!dim(tmp)[2]==11){
      print(paste0("skipping round ", r, "for dim incompatibility"))
      next}
  
  ## Recreate initial centrality values
  ## for group members (g nodes) & recruits (r nodes) 
  g1 <-  graph_from_edgelist(
    as.matrix(bN[[r]]$edgelist[grepl(pattern="g",
                                     x=bN[[r]]$edgelist$from),
                               c("from", "to")]))
  
  gcent <- round(eigen_centrality(g1)$vector, 2)
  
  r1 <-  graph_from_edgelist(
    as.matrix(bN[[r]]$edgelist[grepl(pattern="r",
                                     x=bN[[r]]$edgelist$from),
                               c("from", "to")]))
  rcent <- round(eigen_centrality(r1)$vector, 2)
  
  ## Graph-level centralization:
  rcent.eigen <- centr_eigen(r1)$centralization
  rcent.degree <- centr_degree(r1)$centralization
  
  gcent.eigen <- centr_eigen(g1)$centralization
  gcent.degree <- centr_degree(g1)$centralization
  
  ## move to a dataframe:
  init.centrality <- as.data.frame(c(gcent, rcent))
  init.centrality$nodeID <- as.character(rownames(init.centrality))
  init.centrality$ggraph.ecent <- round(gcent.eigen, 3)
  init.centrality$ggraph.degcent <- round(gcent.degree, 3)
  init.centrality$rgraph.ecent <-   round(rcent.eigen, 3) 
  init.centrality$rgraph.degcent <- round(rcent.degree, 3)
  colnames(init.centrality)[1:2] <- c("initialcent", "nodeID")
  init.centrality$whichSim <- r
  
  ## get an initial metric centralization score:
  ## for recruits and for group members:
  
  ## Simulation data 
  ## Observe that I'm throwing away a lot of data in the second index;
  ##need to figure out what
  
  to.work.with <- node.traj[[r]] ## list of node attributes by round
  
  tww <- bind_rows(to.work.with,## list to dataframe
                   .id = "column_label")
  tww2 <- left_join(tww, ## add in the centrality info
                    init.centrality, 
                    by=c("nodeID"= "nodeID"))
  tww2$structure = netstype ## TWW2 is one round
  
  alldat = rbind(alldat, tww2) ## alldat is nodes for all rounds
  } ## end node info reconstruction

  ## dignostics, if desired
##plot(density(tww2$ggraph.ecent)) ## uniform
##plot(density(tww2$rgraph.ecent)) ## uniform
##table(tww2$iteration)
##table(tww2$round)
##table(tww2$nodeID) ##  63 per node, except g17(?)
##table(tww2$shockpercent) ## 10-90

## clean up into a dataframe:

  dev.alldat <-alldat %>%
    group_by(iteration, shockpercent, round) %>%
    count(type) %>%
    mutate(total.nodes = sum(n)) %>%
    mutate(rep.percent = round(n/total.nodes, 2))#

  ## add the graph centralization info:
  ## list the graph attribute columns:
  ## get the columns with "graph" in the name:
  ## (thes are graph attributes, and I don't feel like typing all)
  gas <-colnames(alldat)[grep(colnames(alldat), pattern="graph")]
  graph.atts <- c("iteration", "shockpercent", "round",
                gas)

  ## add original network centrality attributes:
  dev.alldat2 <- dev.alldat %>%
  left_join(unique(alldat[,graph.atts]),
            by=c("iteration" = "iteration", ##reminder: iteration = which simulated network
                 "shockpercent" = "shockpercent",
                 "round" = "round"))

  ## Add the network configuration:
  dev.alldat2$type <- netstype
  return(dev.alldat2)
}


erpa_nets <- process(filename="ER-PASims.Rdata", 
               netstype="ER-PA") ## this one is about 100?


papa_nets <- process(filename="PA-PAEvolutionSims.Rdata", 
                     netstype="PA-PA") ## this is about 10k?

erer_nets <- process(filename = "ER-EREvolutionSims.Rdata",
                     netstype="ER-ER")

table(dev.alldat2$round) ## 1-7, not all end up at 7 because the simulation exits a round if it gets stuck

plot(density(dev.alldat2$ggraph.ecent)) ## more or less uniform
plot(density(dev.alldat2$ggraph.ecent)) ## more or less uniform



## Now, need underlying research questions:
## eg: size of "out" at R3; R5; R7 ~ original network centrality

r5 <- dev.alldat2[which(dev.alldat2$round==5 & 
                          dev.alldat2$type=="out"),]  

plot(density(r5[which(r5$type=="out"),]$rep.percent)) ## bimodal peaks at .2 and ~.75

## Write out, and do analysis on all sets i a different script

