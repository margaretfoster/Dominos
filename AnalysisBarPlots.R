rm(list = ls()) 
setwd("~/Dropbox/DominosPaper/Dominos-Code/code2022/")

library(dplyr)
library(ggplot2)
library(igraph)


load("ER-EREvolutionSims.Rdata") ## 1k long, 500 netstype
netstype= "ER-ER"

length(bN) ## Should be: 10,000 networks
length(node.traj) ## 500 entries with 9 sub-entries. Each sub-entry is 
## a shock-percent
length(node.stats) ##500

save(bN, file=paste0("./results/bN_",netstype, "_", Sys.Date(), ".Rdata"))
save(edge.traj, file=paste0("./results/edgetraj_",netstype, "_", Sys.Date(), ".Rdata"))
save(node.traj, file=paste0("./results/nodetraj_",netstype, "_", Sys.Date(), ".Rdata"))
save(node.stats, file=paste0("./results/nodetraj_",netstype, "_", Sys.Date(), ".Rdata"))

## workspace cleanup now that we've saved
rm(bN)
rm(edge.traj)
rm(node.traj)


     ## Save most of the data objects separately:

## visualize just one round:
tmp <- bind_rows(node.traj[[1]])


tmp2 <- tmp %>%
  group_by(iteration, shockpercent, round) %>%
  count(type) %>%
  mutate(total.nodes = sum(n)) %>%
  mutate(rep.percent = round(n/total.nodes, 2))## sometimes there are too many
## when there are a lot of "out" nodes!

## quick ugly plot:
## But -- since this should be the same network
## there are at least a few cases in which 
## the group collapses at a medium (20-30) shock percent
## but holds onto a core croup at a higher 
## shock percent. 
## that's interesting

## Bar chart of percent recruit/group/out
## This is just for one network though!!
  
colnames(tmp2) <- stringr::str_to_title(colnames(tmp2))

p<-ggplot(data=tmp2, 
          aes(x=Round, y=Num, fill=Type)) +
  geom_bar(stat="identity") +
  scale_x_discrete(limits=c(paste0(1:7)))+
  facet_wrap(~Shockpercent, scales = "free_y")+
  labs(title= paste0("Attrition plot for ", netstype, " Networks"))+
  theme_bw()+
  theme(legend.position="bottom")
p

ggsave(p, file=paste0(netstype, "-barchart.png"))

## BN:
## 10,000 listm each of which has two entries:
## [[N]][1] Node information: X rows, 5 columns
## [[N]][2] Edge list with columns: from, to, tieID, from.type, to.type

#