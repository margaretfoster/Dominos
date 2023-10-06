rm(list = ls()) 
setwd("~/Dropbox/DominosPaper/Dominos-Code/code2022/")

### Takes output of 02simAnalysis.R:
## node.traj
## node.stats
## edge.traj

library(ggplot2)
library(dplyr)

## Produces visualizations:
##load("ER-PAEvolutionSims.Rdata")
load("ER-EREvolutionSims.Rdata")
length(node.traj) ##1k
length(edge.traj)

rm(list=setdiff(ls(), "node.stats")) ## clean up the space

ls()

all <- bind_rows(node.stats,## list to dataframe
                 .id = "column_label")

## %% Summarize 
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##Summarize the trajectory of those
## nodes that started as recruits: 
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##Visualize  a single-path:

## 100 is interesting
## most of these don't have notable movement
## Might need to reconfigure the PA params

gg <- ggplot(data=node.stats[[50]],
             aes(x=round,
                 y=percent,
                 color=type))+
    geom_line()+
    facet_wrap(facets = vars(shockpercent))+
    labs(title = "Node Affiliations By Shock Percent \n ER-PA",
         x = "Simulation Step",
         y = "Affiliation Categories of Original Recruit Group") +
    geom_vline(xintercept = 6, linetype="dotted",
                color = "red", size=.5) +
    scale_x_continuous(limits=c(1, 10),
                     breaks=seq(from=1, to=10, by=1))+
    theme_bw()

gg

dev.off()

##ggsave(plot=gg,
##      file="ER-ERShockAnalysis.pdf")

## Spaghetti plot of more than one simulation:

dat2 <- bind_rows(node.stats) 

## proportion table:
## proportion of each type / total number of nodes
round(table(dat2$type)/dim(dat2)[1],2)
## ER-ER
##  group     out recruit 
## 0.37    0.07    0.56 

## The relative frequency of types
# tells us that ER-PA is very hard to get people out:

dat2$type <- as.factor(dat2$type)

summary(dat2)

dat2r <- dat2[which(dat2$type=="recruit"),]
dat2g <- dat2[which(dat2$type=="group"),]
dat2o <- dat2[which(dat2$type=="out"),]

unique(all$iteration)

gg2 <- ggplot(all,
              aes(x=round,
                  y=percent,
                  color=type,
                  group=iteration))+
    geom_line()+
    facet_wrap(facets = vars(shockpercent))+
    labs(title = "Recruit Nodes in Group By Shock Percent \n ER-ER",
         x = "Simulation Step",
         y = "Percen of Original Recruit Nodes in Group") +
    geom_vline(xintercept = 6, linetype="dotted",
               color = "red", size=.5) +
    scale_x_continuous(limits=c(1, 10),
                       breaks=seq(from=1, to=10, by=1))+
    theme_bw()

gg2

dev.off()

## shock percent =50
## facet wrap by rounds
group.at.50  <- dat2g %>%
    filter(shockpercent==50)

out.at.50<- dat2o %>%
    filter(shockpercent==50)

both <- rbind(group.at.50,
              out.at.50)

table(group.at.50$round)
## 2:7 (all nodes at round 1 are "recruit")
summary(both[which(both$type=="out"),]$percent)

gg2 <- ggplot()+
    geom_density(data=both,
                 aes(x=percent, color=type))+
    facet_wrap(~round) +
    labs(title="Distribution of Recruit Nodes Two Rounds After Shock (ER-PA)",
         subtitle="Ideology Shock to 50% of Nodes")+
    xlab("Percentage of Recruit Nodes")+
    theme_bw()

gg2

ggsave(gg2, file="DistribER-PANodeAffiliation-50s-1k.pdf")
dev.off()



## shock percent =20
## facet wrap by rounds
group.at.20  <- dat2g %>%
    filter(shockpercent==20)

out.at.20<- dat2o %>%
    filter(shockpercent==20)

both <- rbind(group.at.20,
              out.at.20)

table(group.at.20$round)
## 2:7 (all nodes at round 1 are "recruit")
summary(both[which(both$type=="out"),]$percent)

gg2 <- ggplot()+
    geom_density(data=both,
                 aes(x=percent, color=type))+
    facet_wrap(~round) +
    labs(title="Distribution of Recruit Nodes Two Rounds After Shock (ER-PA)",
         subtitle="Ideology Shock to 20% of Nodes")+
    xlab("Percentage of Recruit Nodes")+
    theme_bw()

gg2

ggsave(gg2, file="DistribER-PANodeAffiliation-20s-1k.pdf")
dev.off()


## shock percent =80
## facet wrap by rounds
group.at.80  <- dat2g %>%
    filter(shockpercent==80)

out.at.80<- dat2o %>%
    filter(shockpercent==80)

both <- rbind(group.at.80,
              out.at.80)

table(group.at.80$round)
## 2:7 (all nodes at round 1 are "recruit")
summary(both[which(both$type=="out"),]$percent)

gg2 <- ggplot()+
    geom_density(data=both,
                 aes(x=percent, color=type))+
    facet_wrap(~round) +
    labs(title="Distribution of Recruit Nodes Two Rounds After Shock (ER-PA)",
         subtitle="Ideology Shock to 80% of Nodes")+
    xlab("Percentage of Recruit Nodes")+
    theme_bw()

gg2

ggsave(gg2, file="DistribER-PANodeAffiliation-80s-1k.pdf")
dev.off()
 
