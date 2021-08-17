##!/usr/bin/env Rscript

args=commandArgs(trailingOnly=TRUE) ## first arg is input second arg is output

## Run the simulation for the ER-PA networks

print(paste0("Data from: ", args[[1]]))
print(paste0("Data saved to: ", args[[2]]))

load(paste0(args[1], ".Rdata")) ## Random networks with affiliation threashold at [.6,1]

source("genDat2.R")
source("runSim2.R")

library(igraph)
library(dplyr)

print(numsim.nets)
## with bN list, numsims, num.recruit, num.group
## seed, 
###############

## Take each of the generated networks and run it through
## the affiliation/defiliation simulation:

node.traj <- list() ## for nodes across all sims
edge.traj <- list() ## for edges across all sims
node.stats <- list() ## for the node movement summary

##
panel <- seq(from=.1, to=.9, by=.1)  
 panel2 <- seq(from=.1, to=.9, by=.25) ##shocks at quarter points

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
    outlist <- list(shocks.list=shocks.list,
                    rounds.list=rounds.list,
                    summary.movement= rsm)
    return(outlist)
} ## brace closes callEvo function


##%%%%%%% Prep the data:
for(n in 1:numsim.nets){
##for(n in 2:3){
    print(paste0("afiliation/defiliation simulation for network: ", n))
    ##
    out <- callEvo(panel=panel, ## callEvo calls the simulation for each
                   ## of the network cuts
                  ## sim.step=,
                   rseed=rseed,
                   n=n,
                   when.shock=when.shock,
                   sim.length=sim.length,
                   network=bN[[n]])
    node.traj[[n]] <- out$shocks.list ## nodes
    edge.traj[[n]] <-out$rounds.list ## edges
    node.stats[[n]] <- out$summary.movement ## in/out summary
    print(paste0("finishing iteration:  ",n))
}


length(node.traj)

## Output is three lists:
## node.traj
## node.stats
## edge.traj

save.image(file=paste0(args[2], "Rdata"))
