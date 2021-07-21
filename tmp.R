 ## test out a few iterations of the updater
rm(ls=list())

library(igraph)
library(dplyr)
library(ggplot2)
library(tidyr)

####################
## Load helper function(s)
###################

#################
## init networks:
################
## Consists of N Nodes
## t ties between nodes
## N node-level joining thresholds
## N node-level ideology preferences

## recruits
load("initRecruits.Rdata")

## group
load("initgroup.Rdata")

names(initGroup)
names(initRecruits)

####################################
##### Combine nodes
####################################

### make a dataframe of all node attributes:
## but not ties

allnodes <- rbind(initRecruits$nodeAttributes,
                  initGroup$nodeAttributes)

head(initRecruits)

allnodes$aboveT <- ifelse(allnodes$nodeIdeoT1 > allnodes$nodeThresh, 1, 0) ## This matters for recruits to join

allnodes$belowT <- ifelse(allnodes$nodeIdeoT1 < allnodes$nodeThresh, 1, 0) ## this matters for group members to leave

allnodes
###########################
## Join Adjacency Networks
############################

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

## Just need: from, to, tie/attention weight
tokeep <- c("weight", "from_name", "to_name")
network.el <- network.el[,tokeep] ## edgelist

##################################
## Update
##################################
## Rest of algorithm:

## 1: print nodes that are above the nodeThreshold
## (a) Those are the nodes that are now "in" the group
## 2: rewire ties for nodes tagged as "group"/above nodeThresh

tmp1 <- network.el %>% ## expand weights 
    tidyr::uncount(weight) ## to produce one row per tie

tmp1$tieID <- paste0("t",1:nrow(tmp1))

## add node-level attributes (type)
##  sender group type:
tmp <- merge(x=tmp1,
             y=allnodes[,c("nodeID", "type")],
             by.x="from_name",
             by.y="nodeID",
             all.x=TRUE)

head(tmp)

colnames(tmp) <- c("from", "to",
                   "tieID", "from.type")


## reciever group type:
expanded.el <- merge(x=tmp,
             y=allnodes[,c("nodeID", "type")],
             by.x="to",
             by.y="nodeID",
             all.x=TRUE)

colnames(expanded.el) <- c("from", "to",
                   "tieID", "from.type",
                           "to.type")

expanded.el

################################
## Update:
## Script to break ties with former comrades
## called "disassociate"

head(allnodes)

## First: list of nodes to rewire:
## Want all nodes NOT in the group AND above the threshold
nodes.to.wire <- allnodes[(allnodes$nodeIdeoT1 >
                           allnodes$nodeThresh) &
                          allnodes$type=="recruit",
                          "nodeID"]
nodes.to.wire

## Function to rewire the ties
source("rewireTies.R")
## Produces a list of ties to make
## And ties to cut

## Cut
r.seed=070121

l1 <- rewire.join(expanded.el,
                   node.list=nodes.to.wire, r.seed,
                   round=0)

l1

## Check out the details:
l1$Ties.To.Cut

## Update the edgelist df:
## t2.cuts embeds the unchanged ties:
t2.cuts <- expanded.el[-which(expanded.el$tieID %in%
                              l1$Ties.To.Cut),]
## t2.new adds the new ties:
t2.new <- cbind(l1$Ties.To.Make,
                from.type="group", to.type="group")

#### Updated Edgelist:
## fields: from, to, from.type, to.type:
t2.updated <- rbind(t2.cuts, t2.new)

## Update adjacency matrix:
gg2 <- graph_from_data_frame(t2.updated, directed=TRUE)
t2.adjmat <- as_adj(gg2, sparse=FALSE)

##%%%%%%%%%%%%%%
##Plot:
#%%%%%%%%%%%%%%%
gg.0 <- graph_from_data_frame(expanded.el,directed=TRUE)

par(mfrow=c(1, 2))
plot(gg.0, main="Network Round 0",
     arrow.size=.1,
     vertex.color=ifelse(V(gg.0)$type=="group",
         "navyblue", "lightblue"))
plot(gg2, main="Network Round 1",
     arrow.size=.25,
     vertex.color=ifelse(V(gg2)$type=="group", "navyblue",
         "lightblue"))

### (2) Finish round by updating the affiliation:

allnodes[which(allnodes$nodeID %in% nodes.to.wire),
         "type"] <- "group"


######%%%%%%%%%%%%%%%%
#### 7/15: Round TWO
#####%%%%%%%%%%%%%%%%
## 7/15: NEED TO:
## Update node 'type' for the groups that moved
## in the previous round
## From "nodes to wire"

## Ideology updator takes
## adjMatrix; nodeIdeology; egoWeights

source("updateThreshs.R")
source("ideologyUpdator.R")

######Ideology:

nodeIdeoT2<- ideologyUpdator(adjMatrix=t2.adjmat,
                             nodeIdeology=as.numeric(
                                 allnodes$nodeIdeoT1),
                             egoWeights= as.numeric(
                                 allnodes$egoW))

allnodesR2 <- cbind(allnodes, nodeIdeoT2)

threshsR2 <- updateThreshs(nodedf=allnodesR2,
                           ideologyvar="nodeIdeoT2",
                           threshvar= "nodeThresh")

allnodesR2$aboveT <- threshsR2$above
allnodesR2$belowT <- threshsR2$below

#########Nodes to wire:

nodes.to.join.r2 <- allnodes[(allnodesR2$aboveT==1 &
                             allnodesR2$type=="recruit"),
                             "nodeID"]
nodes.to.join.r2 ## r7, r8, r10

nodes.to.leave.r2 <- allnodes[(allnodesR2$belowT == 1 &
                             allnodesR2$type=="group"),
                              "nodeID"]

nodes.to.leave.r2 ## none?

###
