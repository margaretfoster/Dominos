### Debug the simulation
## 

### Check out the edgelists
load(bN.Rdata) ## Load a bN workspace file
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
        sh <- runSim(nodeInfo = network$nodeInformation,
                     edgeInfo=network$edgelist,
                       sim.length = sim.length, ## how many steps in the sim
                       rshock= when.shock,
                       pts=p,## prop To Shock
                       rseed=rseed)
        dt <- genDat(sh)
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


##%%%%%%% Prep the data:
for(n in 1:numsim.nets){
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

## Output is three lists:
## node.traj
## node.stats
## edge.traj

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##Summarize the trajectory of those
## nodes that started as recruits: 
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##Single iteration:

gg <- ggplot(data=node.stats[[1]],
             aes(x=round,
                 y=percent,
                 color=type))+
    geom_line()+
    facet_wrap(facets = vars(shockpercent))+
    labs(title = "Node Affiliations By Shock Percent \n ER-ER",
         x = "Simulation Ste[",
         y = "Affiliation Categories of Original Recruit Group") +
    geom_vline(xintercept = 6, linetype="dotted",
                color = "red", size=.5) +
    scale_x_continuous(limits=c(1, 10),
                     breaks=seq(from=1, to=10, by=1))+
    theme_bw()

gg

ggsave(plot=gg,
      file="ER-ERShockAnalysis.pdf")

## Spaghetti plot of more than one simulation:

dat2 <- bind_rows(node.stats) ## 375 x 7
dat2$type <- as.factor(dat2$type)

summary(dat2)

dat2r <- dat2[which(dat2$type=="recruit"),]

dat2g <- dat2[which(dat2$type=="group"),]


gg2 <- ggplot(dat2g,
              aes(x=round,
                  y=percent,
                  color=iteration))+
    geom_line(aes(group=iteration))+
    facet_wrap(facets = vars(shockpercent))+
    labs(title = "Recruit Nodes in Group By Shock Percent \n ER-ER",
         x = "Simulation Step",
         y = "Affiliation Categories of Original Recruit Nodes") +
    geom_vline(xintercept = 6, linetype="dotted",
               color = "red", size=.5) +
    scale_x_continuous(limits=c(1, 10),
                       breaks=seq(from=1, to=10, by=1))+
    theme_bw()

gg2

dev.off()


#########
## Visualize the networks:
source("makeGraphObjects.R")

## Visualize one p of the "shock", in this case to 60% of the recruit nodes
ts <- makeGraphObjects(rounds.list[[6]], rounds=10, letter="t")

## Contrast with 20% of recruit nodes shocked
ls <- makeGraphObjects(rounds.list[[2]], rounds=10, letter="l")
#########plot

set.seed(072121)
pdf("initialER-ER.pdf")
plot(ts[[1]], main="Initial Network",
     sub="ER-ER",
     node.size=V(ts[[1]])$infl.on*0.25)
dev.off()

set.seed(1234)
pdf("ERER1to5Sshockto60.pdf")
par(mfrow=c(2,3))
plot(ts[[1]], main="Initial Network")
plot(ts[[2]], main="Time 2")
plot(ts[[3]], main="Time 3")
plot(ts[[4]], main="Time 4")
plot(ts[[5]], main="Time 5")
dev.off()


set.seed(1234)
pdf("ERER6to10Shockto60.pdf")
par(mfrow=c(2,3))
plot(ts[[6]], main="Time 6",
          sub="20% of Recruit Nodes Shocked in T6")
plot(ts[[7]], main="Time 7")
plot(ts[[8]], main="Time 8")
plot(ts[[9]], main="Time 9")
plot(ts[[10]], main="Time 10")
dev.off()


##

set.seed(1234)
pdf("ERER1to5Shockto20.pdf")
par(mfrow=c(2,3))
plot(ls[[1]], main="Initial Network")
plot(ls[[2]], main="Time 2")
plot(ls[[3]], main="Time 3")
plot(ls[[4]], main="Time 4")
plot(ls[[5]], main="Time 5")
dev.off()

set.seed(1234)
pdf("ERER6to10Shockto20.pdf")
par(mfrow=c(2,3))
plot(ls[[6]], main="Time 6",
     sub="20% of Recruit Nodes Shocked in T6")
plot(ls[[7]], main="Time 7")
plot(ls[[8]], main="Time 8")
plot(ls[[9]], main="Time 9")
plot(ls[[10]], main="Time 10")
dev.off()

