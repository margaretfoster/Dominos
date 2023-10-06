rm(list = ls()) 
setwd("~/Dropbox/DominosPaper/Dominos-Code/code2022/")

##load("ER-PASims.Rdata")
##netstype= "ER-PA" ## code network types

load("PA-PAEvolutionSims.Rdata")

netstype= "PA-PA" ## code network types

ls()
View(node.stats)


length(bN) ## 1000 networks
length(node.traj) ## 1000 entries with 9 subentries. Each sub-entry is 
## a shock-percent
length(node.stats)

tmp <- node.stats[[1]]

table(tmp$round)
table(tmp$iteration)

## temporary fix for apparent indexing issue: 
length(node.traj) ## 1k
length(node.traj[[1]]) ## 9

for(i in 1:length(node.traj)){
  for(e in 1:length(node.traj[[i]])){
    node.traj[[i]][[e]]$assumednework <- i
    node.traj[[i]][[e]]$assumed.round <- e
  }
}

tmp <- bind_rows(node.traj[[1]])
table(tmp$round)
table(tmp$assumed.round) ## 1 - 9 x 60

## Create initial centrality measure
## and number of rounds to entry into group

exit= data.frame()
entry= data.frame()
alldat = data.frame()

for(r in 1:10){
  print(r)

  ## Recreate initial centrality values
  ## for group members (g nodes) & recruits (r nodes) 
  g1 <-  graph_from_edgelist(
    as.matrix(bN[[r]]$edgelist[grepl(pattern="g",
                                       x=bN[[r]]$edgelist$from),
                                  c("from", "to")]))

  gcent <- round(eigen_centrality(g1)$vector, 2)

  r1 <-  graph_from_edgelist(
    as.matrix(bN[[r]]$edgelist[grepl(pattern="r",
                                     x=bN[[r]]$edgelist$from),
                               c("from", "to")]))
  rcent <- round(eigen_centrality(r1)$vector, 2)

  ## Graph-level centralization:
   rcent.eigen <- centr_eigen(r1)$centralization
  rcent.degree <- centr_degree(r1)$centralization
  
  gcent.eigen <- centr_eigen(g1)$centralization
  gcent.degree <- centr_degree(g1)$centralization
  
  ## move to a dataframe:
  init.centrality <- as.data.frame(c(gcent, rcent))
  init.centrality$nodeID <- as.character(rownames(init.centrality))
  init.centrality$ggraph.ecent <- round(gcent.eigen, 3)
  init.centrality$ggraph.degcent <- round(gcent.degree, 3)
  init.centrality$rgraph.ecent <-   round(rcent.eigen, 3) 
  init.centrality$rgraph.degcent <- round(gcent.degree, 3)
  colnames(init.centrality)[1:2] <- c("initialcent", "nodeID")
  init.centrality$whichSim <- r

  ## get an initial metric centralization score:
  ## for recruits and for group members:

  ## Simulation data 
  ## Observe that I'm throwing away a lot of data in the second index;
  ##need to figure out what
  
  to.work.with <- node.traj[[r]] ## list of node attributes by round
  
  tww <- bind_rows(to.work.with,## list to dataframe
                 .id = "column_label")
  tww2 <- left_join(tww, ## add in the centrality info
            init.centrality, 
          by=c("nodeID"= "nodeID"))
  tww2$structure = netstype
  
  alldat = rbind(alldat, tww2)
}
  #if("belowT" %in% colnames(tww) ==FALSE){
  ##  print(paste0("passing r ", r))
  ##  next ## at least one network doesn't have belowT?
  ##  }
  
  ## Find round when a node "joins"
  ## operationalization: first round above Threshold
 ## tst.entry <- tww2 %>%
##  group_by(shockpercent, nodeID) %>%
##  filter(aboveT == 1) %>% ## 
##  slice(n=1) 

  ## add for all sims:
  ##entry <- rbind(entry, tst.entry)

  ## Find round when a node "leaves"
  ## operationalization:
  ## first round below Threshold & after shock
  
 ## tst.exit <- tww2 %>%
  ##group_by(shockpercent, nodeID) %>%
  ##filter(belowT == 1 & round >= 4) %>% ## 
  ##slice(n=1) 

##exit <- rbind(exit, tst.exit)


summary(exit$ggraph.degcent) ## .173- 1.346
summary(exit$rgraph.ecent) ##. 04- .42


## For the ER-ER , distribution of 
## graph-level centrality is roughtly normally distributed

hist(exit$ggraph.ecent)
hist(exit$rgraph.ecent)

table(exit$type)

table(exit$round) ## expect to see 1:9 x number of iterations tested
table(entry$round)

table(alldat$round)
table(alldat$assumed.round) ## 600 


### Visualizations:
library(ggplot2)

## 1-- Ideology score distribution
## According to type,
## across rounds of simulation:

gg0 <- ggplot(alldat,
              aes(x= assumed.round, 
                  y=nodeIdeo, 
                  color=type)) + 
  geom_point(size=.33, alpha=.33) +
  scale_x_continuous(breaks=c(1:10),
                     limits=c(1,10),
                     name=paste0("Simulation Rounds \n",
                                 netstype, " Graphs"))+
  scale_y_continuous(name="Node Initial Eigen Centrality", 
                     limits=c(0, 1))+
  theme_bw()+
  facet_wrap(~shockpercent)+
  theme(legend.position="bottom")+
  scale_color_discrete(name="Node Type")

gg0

## What is this plot showing me again?
## I'm not completely sure that it is doing
## what I think it's doing...
gg1 <- ggplot(exit,
              aes(x= round, 
                  y=initialcent, 
                  color=type)) + 
  geom_point(size=.33, alpha=.33) +
  scale_x_continuous(breaks=c(1:10),
                   limits=c(1,10),
                   name=paste0("Simulation Rounds \n",
                   netstype, " Graphs"))+
  scale_y_continuous(name="Node Initial Eigen Centrality", 
                     limits=c(0, 1))+
  theme_bw()+
  facet_wrap(~shockpercent)+
  theme(legend.position="bottom")+
  scale_color_discrete(name="Node Type")

gg1

ggsave(gg1, 
      file=paste0("exitPlot-", netstype, ".pdf"))


## Check the data:

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