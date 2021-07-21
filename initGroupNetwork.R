## This script to initialize the group network
#####################
## Node-level variables
#####################

r.seed <- 070121
num.group <- 6

#########################
##Group Ideologies Time T0:
########################

## Establish original ideology range:
## Can vary this, or set according to theory

low=.7
high = 1

set.seed(r.seed)
group.ideo <- round(runif(min=low,
                          max=high,
                          n=num.group),2)
group.ideo

# Group affiliation Thresholds
## Lower for group members?
## Probably better to keep thresholds comperable
## and give group members higher initital

lower.bound.thresh <- .6
upper.bound.thresh <- .8

groupThreshs <- round(runif(min=lower.bound.thresh,
                            max=upper.bound.thresh,
                            n=num.group),2)
groupThreshs


#########################                     
## Group Adjacency Matrix
########################

g.nodes <-1:num.group
    
t.n5 <- c(0,3,3,0,0,0)
t.n6 <- c(3,0,2,2,2,0)
t.n7 <- c(3,2,0,0,2,2)
t.n8 <- c(0,2,2,0,1,1)
t.n9 <- c(0,2,2,1,0,1)
t.n10 <- c(0,0,2,0,1,0)

adjmat.group <- rbind(t.n5,
                t.n6,
                t.n7,
                t.n8,
                t.n9,
                t.n10)

## Ego weights

ew.g <- (10-rowSums(x=adjmat.group))/10

ew.g
####################
## Node attribute DF
#####################

## nodeIDs

g.nodeIDs <- paste0("g", 1:num.group)

r0.group <- as.data.frame(cbind(g.nodeIDs,
                                "group",
                                ew.g,
                                group.ideo,
                                groupThreshs))


names <- c("nodeID", "type", "egoW",
           "nodeIdeo", "nodeThresh")
colnames(r0.group) <- names

r0.group

initGroup=list(nodeAttributes=r0.group,
    adjMatrix= adjmat.group)

save(initGroup,
     file="initgroup.Rdata")

