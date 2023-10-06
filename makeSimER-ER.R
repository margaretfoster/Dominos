### This calls the actual simulation
## and then does some preliminary data analysis

rm(list=ls())
setwd("~/Dropbox/DominosPaper/Dominos-Code/code2022/")

## This config: ER-ER


####################
## Make groups:
###################
source("initBothER.R") ## makes the initial networks 
source("combineRecruitGroupNets.R") ## combines them
source("callLoop.R") ## runs the simulations

## wrap this to create an arbitrary number of
## networks. Startat 10

numsims <-  10

set.seed(6889)
seeds <- sample.int(1e5, ## range
                size=numsims, ## number for simulation
               replace=FALSE)


bN <- list()
j <- 1

for(s in seeds){
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
                     num.nodes=10,
                     id.letter= "g",
                     type="group",
                     init.ideo.high=1,
                     init.ideo.low=.6,
                     lower.bound.thresh=.5,
                     upper.bound.thresh=.8)
    
    bN[[j]] <- bothNets(initRecruits=r.test,
                        initGroup=g.test)
    j <- j+1
}


class(bN) ## list

attributes(bN[[8]]) ## node Information, edgelist

length(bN) ##


###############

## Panel shocks:
## from .1-.9 of the node recruits
## with an out-shock

source("genDat.R")

panel <- seq(from=.1, to=.9, by=.1)
shocks.list <- list() ## for nodes each round 
rounds.list <-list() ## for edgelist each round
node.traj <- list() ## for nodes across all sims
edge.traj <- list() ## for edges across all sims

## TODO: go into callLoop and add in a
## way to handle isolates and NA such as in:
##"Round 9 nodes to join are "
## and "Round 9 nodes leaving group are NA"
## this situation creates:
##[1] "the node had 0 existing ties"
## | [1] "NaN ties to cut"

for(n in 1:numsims){
    print(n)    
    for(p in panel){
        j=p*10
        sh <- callLoop(bothNets=bN[[n]],
                       round=10,
                       rshock= 6,
                       pts=p,## prop To Shock
                       rseed=71621)
        dt <- genDat(sh)
        rownames(dt) <- NULL
        dt$shockpercent <- j*10
        shocks.list[[j]] <- dt ## this is node  information
        rounds.list[[j]] <- sh ## this will have the edgelists for network viz and change in edges
    }
    
    node.traj[[n]] <-bind_rows(shocks.list) ## list of dataframes with edge attributes
    edge.traj[[n]] <-rounds.list ## list of lists
    ## edge trajc has node indo and also the edge info 
    n=n+1
} 

## Summarize the trajectory of those
## nodes that started as recruits: 

recruit.trajs <- list()
for(i in 1:length(shocks.list)){
    print(i)
    dat <- shocks.list[[i]]
    rs <- dat[grep(dat$nodeID, pattern='r'),]
    rsm<- rs %>%
        group_by(round, type) %>% tally()
    rsm$shockpercent <- i*10
    rsm$percent <- rsm$n/5
    recruit.trajs[[i]] <- rsm
}

rtrajs <- bind_rows(recruit.trajs)
dim(rtrajs)

gg <- ggplot(data=rtrajs,
             aes(x=round,
                 y=percent,
                 color=type))+
    geom_line()+
    facet_wrap(facets = vars(shockpercent))+
    labs(title = "Node Affiliations By Shock Percent \n ER-ER",
         x = "Simulation Round",
         y = "Affiliation Categories of Original Recruit Group") +
    geom_vline(xintercept = 6, linetype="dotted",
                color = "red", size=.5) +
    scale_x_continuous(limits=c(1, 10),
                     breaks=seq(from=1, to=10, by=1))+
    theme_bw()

gg

ggsave(plot=gg,
      file="ER-ERShockAnalysis.pdf")


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

