### This calls the actual simulation
## and then does some preliminary data analysis

rm(list=ls())

## This config: ER-SW


####################
## Make groups:
###################
source("initBothER.R") ## makes the initial ER network
source("initSW.R")

source("combineRecruitGroupNets.R") ## combines them
source("callLoop.R") ## runs the simulations

r.net <- initER(r.seed=12345,
                 id.letter = "r",
                 type="recruit",
                 num.nodes=5,
                 init.ideo.high=.8,
                 init.ideo.low=.3,
                 lower.bound.thresh=.5,
                 upper.bound.thresh=.8)

g.net <- initSWGroup(r.seed=12345,
                      num.nodes=10,
                      init.ideo.high=.9,
                      init.ideo.low=.6,
                      id.letter="g",
                      type="group",
                      init.affil.high=.8,
                      init.affil.low=.5,
                      nei.init=1,
                      rw.prob=.1)
    
bN <- bothNets(initRecruits=r.net,
                       initGroup=g.net)


### Panel Shocks

source("genDat.R")

panel <- seq(from=.1, to=.9, by=.1)
shocks.list <- list()
rounds.list <- list()

for(p in panel){
    j=p*10
    sh <- callLoop(bothNets=bN,
                   round=10,
                   rshock= 6,
                   pts=p,## prop To Shock                                                         
                   rseed=71621)
    dt <- genDat(sh)
    rownames(dt) <- NULL
    dt$shockpercent <- j*10
    shocks.list[[j]] <- dt
    rounds.list[[j]] <- sh
}

## Summarize the trajectory of those                                                              
## nodes that started at recruits:                                                                

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

##Percent out:                                                                                    
library(ggplot2)

gg <- ggplot(data=rtrajs,
             aes(x=round,
                 y=percent,
                 color=type))+
    geom_line()+
    facet_wrap(facets = vars(shockpercent))+
    labs(title = "Node Affiliations By Shock Percent \n ER-SW",
         x = "Simulation Round",
         y = "Affiliation Categories of Original Recruit Group") +
    geom_vline(xintercept = 6, linetype="dotted",
                color = "red", size=.5) +
    scale_x_continuous(limits=c(1, 10),
                     breaks=seq(from=1, to=10, by=1))+
    theme_bw()

gg

ggsave(plot=gg,
      file="ER-SWShockAnalysis.pdf")


##########
## Visualize the networks:

source("makeGraphObjects.R")

## illustrate the network effect of shock to 60% of recruits:                                     
ts <- makeGraphObjects(rounds.list[[6]],
                       rounds=10, letter="t")

## Contrast with network effect of shock to 20% of recruits:                                      
ls <- makeGraphObjects(rounds.list[[2]],
                       rounds=10, letter="l")

#########plot

set.seed(072121)
pdf("initialER-SW.pdf")
plot(ts[[1]], main="Initial Network",
     sub="ER-SW",
     node.size=V(ts[[1]])$infl.on*0.25)
dev.off()


set.seed(1234)
pdf("ERSW1to5hockto60.pdf")
par(mfrow=c(2,3))
plot(ts[[1]], main="Initial Network")
plot(ts[[2]], main="Time 2")
plot(ts[[3]], main="Time 3")
plot(ts[[4]], main="Time 4")
plot(ts[[5]], main="Time 5")
dev.off()

set.seed(1234)
pdf("ERSW6to10Shockto60.pdf")
par(mfrow=c(2,3))
plot(ts[[6]], main="Time 6",
     sub="60% of Recruit Nodes Shocked in T6")
plot(ts[[7]], main="Time 7")
plot(ts[[8]], main="Time 8")
plot(ts[[9]], main="Time 9")
plot(ts[[10]], main="Time 10")
dev.off()

## Compare with 20% shocks

set.seed(1234)
pdf("ERSW1to5Shockto20.pdf")
par(mfrow=c(2,3))
plot(ls[[1]], main="Initial Network")
plot(ls[[2]], main="Time 2")
plot(ls[[3]], main="Time 3")
plot(ls[[4]], main="Time 4")
plot(ls[[5]], main="Time 5")
dev.off()

set.seed(1234)
pdf("ERSW6to10Shockto20.pdf")
par(mfrow=c(2,3))
plot(ls[[6]], main="Time 6",
     sub="20% of Recruit Nodes Shocked in T6")
plot(ls[[7]], main="Time 7")
plot(ls[[8]], main="Time 8")
plot(ls[[9]], main="Time 9")
plot(ls[[10]], main="Time 10")
dev.off()
 

