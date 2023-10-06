### Takes output of 02simAnalysis.R:
## node.traj
## node.stats
## edge.traj

## Produces visualizations:

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##Summarize the trajectory of those
## nodes that started as recruits: 
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##Visualize single-path:

gg <- ggplot(data=node.stats[[1]],
             aes(x=round,
                 y=percent,
                 color=type))+
    geom_line()+
    facet_wrap(facets = vars(shockpercent))+
    labs(title = "Node Affiliations By Shock Percent \n ER-ER",
         x = "Simulation Step",
         y = "Affiliation Categories of Original Recruit Group") +
    geom_vline(xintercept = 6, linetype="dotted",
                color = "red", size=.5) +
    scale_x_continuous(limits=c(1, 10),
                     breaks=seq(from=1, to=10, by=1))+
    theme_bw()

gg

##ggsave(plot=gg,
##      file="ER-ERShockAnalysis.pdf")

## Spaghetti plot of more than one simulation:

dat2 <- bind_rows(node.stats) ## 375 x 7
dat2$type <- as.factor(dat2$type)

summary(dat2)

dat2r <- dat2[which(dat2$type=="recruit"),]
dat2g <- dat2[which(dat2$type=="group"),]
dat2o <- dat2[which(dat2$type=="out"),]

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

