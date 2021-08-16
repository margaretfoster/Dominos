### Debug the simulation
## 

rm(list=ls())

## This config: ER-ER


####################
## Make groups:
###################
source("initBothER.R") ## makes the initial networks
source("initPA.R")
source("combineRecruitGroupNets.R") ## combines them


## wrap this to create an arbitrary number of
## networks. Startat 10

numsim.nets <-  10000
num.recruits <- 5
num.group <- num.recruits * 2
rseed <- 689
sim.length=7
when.shock=4

set.seed(rseed)
seeds <- sample.int(numsim.nets*10, ## 10x number of simulations
                size=numsim.nets, ## number for simulation
               replace=FALSE)


bN <- list()
j <- 1

for(s in seeds){
    print(s)
    r.test <- initER(r.seed=s,
                     id.letter = "r",
                     type="recruit",
                     num.nodes=num.recruits,
                     init.ideo.high=.8, 
                     init.ideo.low=.3,
                     lower.bound.thresh=.5,
                     upper.bound.thresh=.8)

    
    g.test <- initPA(r.seed=54321,
                     num.nodes=num.group,
                     id.letter= "g",
                     type="group",
                     init.ideo.high=1,
                     init.ideo.low=.5)
    ## Check for zero-sender rows:
    
    tst <- bothNets(initRecruits=r.test,
                        initGroup=g.test)

    tmp <- as.matrix(tst$edgelist[,1:2])
    
    tmp2 <- as_adjacency_matrix(graph_from_edgelist(tmp,
                                                    directed = TRUE),
                                sparse=FALSE)
    
    l <- sum(rowSums(tmp2)==0)## Count number of returns of TRUE
    print(paste0(l, " rows with no sending ties")) 
    
    
    while(l > 0){## While any rows in adjmat with no sending ties:
        
        ## Reset the seed and redraw the network:
        print("Redrawing network")
        s=s+10 ##simplest way to add a new starting point is to increment
        ## but one was too close to the original
        print(s)
        r.test <- initER(r.seed=s,
                         id.letter = "r",
                         type="recruit",
                         num.nodes=num.recruits,
                         init.ideo.high=.8, 
                         init.ideo.low=.3,
                         lower.bound.thresh=.5,
                         upper.bound.thresh=.8)
        
        g.test <- initPA(r.seed=54321,
                         num.nodes=num.group,
                         id.letter= "g",
                         type="group",
                         init.ideo.high=1,
                         init.ideo.low=.6)
        
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

length(bN)

### Check out the edgelists
save.image(file="ER-PASimData.Rdata")
