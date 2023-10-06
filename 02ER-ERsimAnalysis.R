## Runs the main simulation for the
# ER-ER networks

rm(list=ls())
setwd("~/Dropbox/DominosPaper/Dominos-Code/code2022/")

library(igraph)
library(dplyr)

## load the simulated networks:
load("ER-ERSimDataWide.Rdata") ## Load a bN workspace file
## with bN list, numsims, num.recruit, num.group
## seed, 

## debug: only keep the networks
## redo everything else:
rm(list=setdiff(ls(), c("bN", "num.recruits")))
###############


source("runSim2.R")
source("genDat2.R")

ls()

## Take each of the generated networks and run it through
## the affiliation/defiliation simulation:

node.traj <- list() ## for nodes across all sims
edge.traj <- list() ## for edges across all sims
node.stats <- list() ## for the node movement summary

##
panel <- seq(from=.1, to=.9, by=.1)  
## panel2 <- seq(from=.1, to=.9, by=.25) ##shocks at quarter points

## Panel shocks:
## from .1-.9 of the node recruits
## with an out-shock


## ReWiring simulation for all rounds:
## EOD 8/9--- need to think though what I am doing with
## the callLoops and panel:
 
callEvo <- function(panel, n,## n is which simulated network
                    when.shock,
                    sim.length,
                    network,
                    rseed){
    shocks.list <- list() ## for node information for each round of simulation
    rounds.list <-list() ## for edgelist each round
    j=1 ## this is just to index the results from the panel sims
    for(p in panel){ ## for each shock value:
        ## Run the affiliation/defiliation simulation for sim.length rounds

        print(paste0("in panel ", p, " percent shock"))
        sh <- runSim2(nodeInfo = network$nodeInformation,
                     edgeInfo=network$edgelist,
                       sim.length = sim.length, ## how many steps in the sim
                       rshock= when.shock,
                       pts=p,## prop To Shock
                       rseed=rseed)
        dt <- genDat2(sh)
        rownames(dt) <- NULL
        dt$shockpercent <- p*100
        dt$iteration <- n ## this is which network 
        shocks.list[[j]] <- dt ## this is node  information
        rounds.list[[j]] <- sh ## this will have the edgelists for network viz and change in edges
        j=j+1 ## need the list index to increment separately from other params
    } ## brace closes for p in panel clause
    print("shocks.list is of length: ", )
    print(length(shocks.list))

    ## make the node summary:
    dat <-bind_rows(shocks.list) ## list of dataframes with edge attributes
    rs <- dat[grep(dat$nodeID, pattern='r'),]
    rsm<- rs %>%
        group_by(shockpercent, round, type) %>% tally()
    rsm$percent <- rsm$n/num.recruits
    rsm$num.nodes <- rsm$n ## rename output of tally
    rsm$n <- NULL ## delete original output of tally (for clarity)
    rsm$iteration <- n
    rsm$when.shock <- when.shock
    
    ## TODO 8/11: move the summary in/out script here
    ## make a third component to the outlist
    
    outlist <- list(shocks.list=shocks.list,
                    rounds.list=rounds.list,
                    summary.movement= rsm)
    return(outlist)
} ## brace closes callEvo function

numsim.nets= 100
r.seed= 91722
when.shock = 4
sim.length= 7

##%%%%%%% Prep the data:
for(n in 1:80){
##for(n in 2:3){
    print(paste0("afiliation/defiliation simulation for network: ", n))
    ##
    out <- callEvo(panel=panel, ## callEvo calls the simulation for each
                   ## of the network cuts
                  ## sim.step=,
                   rseed=r.seed,
                   n=n,
                   when.shock=when.shock,
                   sim.length=sim.length,
                   network=bN[[n]])
    node.traj[[n]] <- out$shocks.list ## nodes
    edge.traj[[n]] <-out$rounds.list ## edges
    node.stats[[n]] <- out$summary.movement ## in/out summary
    print(paste0("finishing iteration:  ",n))
}

length(node.traj) ## 84

table(node.traj[[1]][[1]]$round)

ls()
length(node.stats) ## 47

## Output is three lists:
## node.traj
## node.stats
## edge.traj

save.image("ER-EREvolutionSims.Rdata")
