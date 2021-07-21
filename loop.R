### This calls the actual simulation
## and then does some preliminary data analysis

rm(list=ls())


####################
## Load helper function(s)
###################

###################
### Load disconnected recruit + group network
##################

load("bothNets.Rdata")
source("callLoop.R")

##


ten.Rounds <- callLoop(bothNets=bothNets,
                       round=10,
                       rshock= 6,
                       pts=.3,
                       rseed=71621)

class(ten.Rounds)

dat10 <- bind_rows(ten.Rounds[[1]])
rownames(dat10) <- 1:dim(dat10)[1]

datcols <- c("nodeID", "round", "type",
             "nodeIdeo", "nodeIdeoUpdate")

dat10[,datcols]

### Brainstorm analysis:

## Want to make a round1 aboveT:

ten.Rounds[[1]][[1]]$aboveT <- ifelse(
    ten.Rounds[[1]][[1]]$nodeIdeo >
    ten.Rounds[[1]][[1]]$nodeThresh, 1, 0)

dat10 <- bind_rows(ten.Rounds[[1]])

ct10 <- dat10 %>%
    group_by(round, type) %>% tally()

print(ct10, n=30)

##########
## Visualize the networks:

library(igraph)
library(dplyr)

## Round 1 edgelist and node info
r1.ndf <- ten.Rounds[[1]][[1]]


makeGraphObjects <- function(simList, rounds, letter){

    graphsOut <- list()
    for(r in 1:rounds){
        print(r)
        ## edgelist:
        rr.el <- as.data.frame(simList[[2]][[r]][,1:2]) %>%
            count(from, to, name="weight") ## edge weights
        
        rr.ndf <- simList[[1]][[r]] ## node-level data
        
        g <- graph_from_data_frame(rr.el,
                                   directed = TRUE,
                                   vertices = rr.ndf)
        V(g)$infl.on = 100-as.numeric(V(g)$egoW)*100
        V(g)$size <- V(g)$infl.on*0.33
        E(g)$width = E(g)$weight
        E(g)$arrow.size = .2
        V(g)$color <-ifelse(V(g)$type=="group",
                            "tomato",
                            ifelse(V(g)$type=="recruit",
                                   "steel blue",
                                   "gray50"))
        graphsOut[[r]] <- assign(paste0(letter,r), g)
    }
    
    return(graphsOut)
}

gs <- makeGraphObjects(five.Rounds, rounds=5)

ts <- makeGraphObjects(ten.Rounds, rounds=10, letter="t")
#########plot


set.seed(1234)
pdf("R10TestFirstHalf.pdf")
par(mfrow=c(2,3))
plot(ts[[1]], main="Initial Network")
plot(ts[[2]], main="Time 2")
plot(ts[[3]], main="Time 3")
plot(ts[[4]], main="Time 4")
plot(ts[[5]], main="Time 5")
dev.off()

set.seed(1234)
##pdf("R10TestSecondHalf.pdf")
par(mfrow=c(2,3))
plot(ts[[6]], main="Time 6")
plot(ts[[7]], main="Time 7")
plot(ts[[8]], main="Time 8")
plot(ts[[9]], main="Time 9")
plot(ts[[10]], main="Time 10")
dev.off()

## Five Round Viz:
## (ten round is basically the same)
### Why do G3 and G2 stay out of the
## group, even though they're very influecable?
## The answer is that they both start a very high
## committment, but also have a relatively high threshold
## so they're very committed AND also very influencable
## which meant hta they drop out quickly... and stay out.
rownames(dat) <- paste(dat$nodeID,dat$round)

colnames(dat)
ofint <- c("g2", "g3")
colsint <- c("nodeID", "type", "egoW",
            'nodeThresh',
            'nodeIdeoUpdate', "belowT")

dat[which(dat$nodeID %in% ofint),colsint]
