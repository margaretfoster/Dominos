### Debug the simulation
## 

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

numsim.nets <-  500 ## number of simulated networks

rseed=181

set.seed(rseed)
seeds <- sample.int(numsim.nets*10, ## range, set to 10x the number of simulations
                size=numsim.nets, ## number for simulation
               replace=FALSE)

num.recruits <- 5
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
                     init.ideo.high=.8, 
                     init.ideo.low=.3,
                 lower.bound.thresh=.5,
                 upper.bound.thresh=.8)

    
    g.test <- initER(r.seed=s,
                     num.nodes=num.group,
                     id.letter= "g",
                     type="group",
                     init.ideo.high=1,
                     init.ideo.low=.5,
                     lower.bound.thresh=.5,
                     upper.bound.thresh=.8)

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
                         lower.bound.thresh=.5,
                         upper.bound.thresh=.8)
        
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
save.image(file="ER-ERSimData.Rdata")


###############

## ## Panel shocks:
## ## from .1-.9 of the node recruits
## ## with an out-shock


## ## ReWiring simulation for all rounds:
## ## EOD 8/9--- need to think though what I am doing with
## ## the callLoops and panel:
 
## callEvo <- function(panel, n,## n is which simulated network
##                     when.shock,
##                     sim.length,
##                     network,
##                     rseed){
##     shocks.list <- list() ## for node information for each round of simulation
##     rounds.list <-list() ## for edgelist each round
##     j=1 ## this is just to index the results from the panel sims
##     for(p in panel){ ## for each shock value:
##         ## Run the affiliation/defiliation simulation for sim.length rounds

##         print(paste0("in panel ", p, " percent shock"))
##         sh <- runSim(nodeInfo = network$nodeInformation,
##                      edgeInfo=network$edgelist,
##                        sim.length = sim.length, ## how many steps in the sim
##                        rshock= when.shock,
##                        pts=p,## prop To Shock
##                        rseed=rseed)
##         dt <- genDat(sh)
##         rownames(dt) <- NULL
##         dt$shockpercent <- p*100
##         dt$iteration <- n ## this is which network 
##         shocks.list[[j]] <- dt ## this is node  information
##         rounds.list[[j]] <- sh ## this will have the edgelists for network viz and change in edges
##         j=j+1 ## need the list index to increment separately from other params
##     } ## brace closes for p in panel clause
##     print("shocks.list is of length: ", )
##     print(length(shocks.list))

##     ## make the node summary:
##     dat <-bind_rows(shocks.list) ## list of dataframes with edge attributes
##     rs <- dat[grep(dat$nodeID, pattern='r'),]
##     rsm<- rs %>%
##         group_by(shockpercent, round, type) %>% tally()
##     rsm$percent <- rsm$n/num.recruits
##     rsm$num.nodes <- rsm$n ## rename output of tally
##     rsm$n <- NULL ## delete original output of tally (for clarity)
##     rsm$iteration <- n
##     rsm$when.shock <- when.shock
    
##     ## TODO 8/11: move the summary in/out script here
##     ## make a third component to the outlist
    
##     outlist <- list(shocks.list=shocks.list,
##                     rounds.list=rounds.list,
##                     summary.movement= rsm)
##     return(outlist)
## } ## brace closes callEvo function

## ## Take each of the generated networks and run it through
## ## the affiliation/defiliation simulation:


## node.traj <- list() ## for nodes across all sims
## edge.traj <- list() ## for edges across all sims
## node.stats <- list() ## for the node movement summary

## ##
## panel <- seq(from=.1, to=.9, by=.1)  
## panel2 <- seq(from=.1, to=.9, by=.25) ##shocks at quarter points

## for(n in 1:numsim.nets){
## ##for(n in 2:3){
##     print(paste0("afiliation/defiliation simulation for network: ", n))
##     ##
##     out <- callEvo(panel=panel, ## callEvo calls the simulation for each
##                    ## of the network cuts
##                   ## sim.step=,
##                    rseed=181,
##                    n=n,
##                    when.shock=when.shock,
##                    sim.length=sim.length,
##                    network=bN[[n]])
##     node.traj[[n]] <- out$shocks.list ## nodes
##     edge.traj[[n]] <-out$rounds.list ## edges
##     node.stats[[n]] <- out$summary.movement ## in/out summary
##     print(paste0("finishing iteration:  ",n))
## }


## ls()
## class(node.stats)
## length(node.stats)
## print(node.stats)

## node.stats[[1]]
## print(node.stats[[1]], n=10)


## class(node.traj)
## length(node.traj)
## class(node.traj[[1]])
## length(node.traj[[1]])

