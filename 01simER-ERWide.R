### Debug the simulation
## ## MJF: 10/24/22
## Adding more nodes to networks
## and widening the ideology bounds
## (see correspondence with Z Steinert-Threlkeld)

rm(list=ls())

## This config: ER-ER
####################
## Make groups:
###################
source("initBothER.R") ## makes the initial networks 
source("combineRecruitGroupNets.R") ## combines them
source("runSim.R") ## runs the simulation
source("genDat.R")

## wrap this to create an arbitrary number of
## networks. Startat 10

numsim.nets <-  1000 ## number of simulated networks

rseed=181

set.seed(rseed)
seeds <- sample.int(numsim.nets*10, ## range, set to 10x the number of simulations
                    size=numsim.nets, ## number for simulation
                    replace=FALSE)

summary(seeds) ##7-9998

num.recruits <- 10 ## up from 5 originally
num.group <- num.recruits *2 ## group 2x size of recruits; no real reason 
when.shock <- 4 ## round for the shock
sim.length <-  7

bN <- list()
j <- 1

for(s in seeds){
    print(s)
    r.test <- initER(r.seed=s,
                     id.letter = "r",
                     type="recruit",
                     num.nodes=num.recruits,
                     init.ideo.high=.75, 
                     init.ideo.low=.25,
                 lower.bound.thresh=.6,
                 upper.bound.thresh=1)

    
    g.test <- initER(r.seed= abs(1000-s), ## need to perturb s, or the recruit & group are the same network,
                     num.nodes=num.group,
                     id.letter= "g",
                     type="group",
                     init.ideo.high=1,
                     init.ideo.low=.5,
                     lower.bound.thresh=.6,
                     upper.bound.thresh=1)

    ## Check for zero-sender rows:
    
    tst <- bothNets(initRecruits=r.test,
                        initGroup=g.test)

    tmp <- as.matrix(tst$edgelist[,1:2])
    
    tmp2 <- as_adjacency_matrix(graph_from_edgelist(tmp,
                                                    directed = TRUE),
                                sparse=FALSE)
    
    l <- sum(rowSums(tmp2)==0)## Count number of returns of TRUE
    print(paste0(l, " rows with no sending ties")) 
    
    
    while(l > 0){## If row(s) in the adj matrix with no sending ties:
        
        ## Reset the seed and redraw the network:
        print("Redrawing network")
        s=s+10 ##simplest way to add a new starting point is to increment
        ## but one was too close to the original
        print(s)
        r.test <- initER(r.seed=s,
                         id.letter = "r",
                         type="recruit",
                         num.nodes=5,
                         init.ideo.high=.8, 
                         init.ideo.low=.3,
                         lower.bound.thresh=.6,
                         upper.bound.thresh=1)
        
        g.test <- initER(r.seed=s,
                         num.nodes=num.group,
                         id.letter= "g",
                         type="group",
                         init.ideo.high=1,
                         init.ideo.low=.6,
                         lower.bound.thresh=.5,
                         upper.bound.thresh=.8)
        ##overwrite tst:
       tst<- bothNets(initRecruits=r.test,
                            initGroup=g.test)
        print("wrote new network")
               
       ## Again for zero-sender rows:
        tmp <- as.matrix(tst$edgelist[,1:2])
        
        tmp2 <- as_adjacency_matrix(graph_from_edgelist(tmp,
                                                        directed = TRUE),
                                    sparse=FALSE)
        
        l <- sum(rowSums(tmp2)==0)## sum is number of rows with TRUE
        print(paste0(l, " rows with no sending ties")) 
    }
    
    bN[[j]] <- tst
    j <- j+1
}

### Check out the edgelists
save.image(file="ER-ERSimDataWide.Rdata")



