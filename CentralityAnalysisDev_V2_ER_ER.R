rm(list = ls()) 
setwd("~/Dropbox/DominosPaper/Dominos-Code/code2022/")

library(dplyr)
library(ggplot2)
library(igraph)
##load("ER-PASims.Rdata")
##netstype= "ER-PA" ## code network types

##load("PA-PAEvolutionSims.Rdata")
##netstype= "PA-PA" ## code network types

load("ER-EREvolutionSims.Rdata")
netstype= "ER-ER"

ls()
View(node.stats)


length(bN) ## 1000 networks
length(node.traj) ## 500 entries with 9 sub-entries. Each sub-entry is 
## a shock-percent
length(node.stats)

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

ggplot(tmp2,
       aes(y= rep.percent,
           x=round,
           color=type)) +
  geom_point(size=.5)+
  theme_bw() +
  geom_jitter()+
  facet_wrap(~shockpercent)
## Create initial centrality measure
## and number of rounds to entry into group

exit= data.frame()
entry= data.frame()
alldat = data.frame()

## dev test on the first N iterations:
for(r in 1:length(node.traj)){
  print(r)
  tmp = bind_rows(node.traj[[r]])
  ## fallthrough if the node.traj object doesn't
  ## have 11 columns [indicates a corner case]
  if(!dim(tmp)[2]==11){
    print(paste0("skipping round ", r, "for dim incompatibility"))
    next}
  
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
  init.centrality$rgraph.degcent <- round(rcent.degree, 3)
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
  tww2$structure = netstype ## TWW2 is one round
  
  alldat = rbind(alldat, tww2) ## alldat is nodes for all rounds
}

plot(density(tww2$ggraph.ecent)) ## uniform
plot(density(tww2$rgraph.ecent)) ## uniform
table(tww2$iteration)
table(tww2$round)
table(tww2$nodeID) ##  63 per node, except g17(?)
table(tww2$shockpercent) ## 10-90

## with TWW2, I can look at node-level variation:
## network structure & rounds to leave. 
## indidivual threshold level x network centralization & rounds to leave

dev.alldat <-alldat %>%
  group_by(iteration, shockpercent, round) %>%
  count(type) %>%
  mutate(total.nodes = sum(n)) %>%
  mutate(rep.percent = round(n/total.nodes, 2))#

## add the graph centralization info:
## list the graph attribute columns:
## get the columns with "graph" in the name:
## (thes are graph attributes, and I don't feel like typing all)
gas <-colnames(alldat)[grep(colnames(alldat), pattern="graph")]

graph.atts <- c("iteration", "shockpercent", "round",
                gas)

## add original network centrality attributes:

dev.alldat2 <- dev.alldat %>%
  left_join(unique(alldat[,graph.atts]),
            by=c("iteration" = "iteration", ##reminder: iteration = which simulated network
                 "shockpercent" = "shockpercent",
                 "round" = "round"))

table(dev.alldat2$round) ## 1-7, not all end up at 7 because the simulation exits a round if it gets stuck

plot(density(dev.alldat2$ggraph.ecent)) ## more or less uniform
plot(density(dev.alldat2$ggraph.ecent)) ## more or less uniform

## Now, need underlying research questions:
   ## eg: size of "out" at R3; R5; R7 ~ original network centrality
  
r5 <- dev.alldat2[which(dev.alldat2$round==5 & 
                          dev.alldat2$type=="out"),]  

plot(density(r5[which(r5$type=="out"),]$rep.percent)) ## bimodal peaks at .2 and ~.75

## results consistently show that recruit eigenvector centrality important (p < .001)
## degree centrality not as important (p < .01)
fit1.r5 <- lm(r5$rep.percent ~ r5$rgraph.ecent + r5$shockpercent + r5$ggraph.ecent)

fit2.r5 <- lm(r5$rep.percent ~ r5$rgraph.degcent + r5$shockpercent + r5$ggraph.ecent)

fit3.r5 <- lm(r5$rep.percent ~ r5$rgraph.ecent + r5$shockpercent + r5$ggraph.degcent)


fit.all.r5 <- lm(r5$rep.percent ~ r5$rgraph.ecent + r5$shockpercent + r5$ggraph.degcent + r5$ggraph.ecent + r5$rgraph.degcent)

summary(fit1.r5)
summary(fit2.r5)
summary(fit3.r5)

summary(fit.all.r5)

## Round 6:
 
r6 <- dev.alldat2[which(dev.alldat2$round==6 & 
                          dev.alldat2$type=="out"),]  
 
plot(density(r6[which(r6$type=="out"),]$rep.percent)) ## bimodal peaks at .2 and ~.76

## results consistently show that recruit eigenvector centrality important (p < .001)
## degree centrality not as important (p < .01)
fit1.r6 <- lm(r6$rep.percent ~ r6$rgraph.ecent + r6$shockpercent + r6$ggraph.ecent)

fit2.r6 <- lm(r6$rep.percent ~ r6$rgraph.degcent + r6$shockpercent + r6$ggraph.ecent)

fit3.r6 <- lm(r6$rep.percent ~ r6$rgraph.ecent + r6$shockpercent + r6$ggraph.degcent)


fit.all.r6 <- lm(r6$rep.percent ~ r6$rgraph.ecent + r6$shockpercent + r6$ggraph.degcent + r6$ggraph.ecent + r6$rgraph.degcent)

summary(fit1.r6) ## note that R^2 goes up a lot when recruit eigenvector cent is included
summary(fit2.r6)
summary(fit3.r6)

summary(fit.all.r6)
round(fit.all.r6$coefficients, 3)

library(broom)
library(tidyr)
library(dplyr)
library(ggplot2)

reg_data <- 
  tidy(fit.all.r6) 

fit_cis_95 <- confint(fit.all.r6, level = 0.95)%>% 
  data.frame() %>%
  rename("conf.low_95" = "X2.5..",
         "conf.high_95" = "X97.5..")
fit_cis_90 <- confint(fit.all.r6, level = 0.90) %>% 
  data.frame() %>%
  rename("conf.low_90" = "X5..",
         "conf.high_90" = "X95..")

results <- bind_cols(reg_data, 
                     fit_cis_95, 
                     fit_cis_90) %>%
  rename(Variable = term,
         Coefficient = estimate,
         SE = std.error) 

   results <- results %>% select(-SE, 
                              -statistic,
                              -p.value)

results[which(results$Variable=="r6$rgraph.ecent"), "Variable"] <- "Recruit E. Centrality"
results[which(results$Variable=="r6$shockpercent"), "Variable"] <- "Extent of Ideology Shock"
results[which(results$Variable=="r6$ggraph.degcent"), "Variable"] <- "Group Deg. Centrality"
results[which(results$Variable=="r6$rgraph.degcent"), "Variable"] <- "Recruit Deg. Centrality"
results[which(results$Variable=="r6$ggraph.ecent"), "Variable"] <- "Group E. Centrality"

reg.plot.r6 <- ggplot(results, 
      aes(x = Variable, y = Coefficient)) +
  geom_hline(yintercept = 0, 
             colour = gray(1/2), lty = 2) +
  geom_point(aes(x = Variable, 
                 y = Coefficient)) + 
  geom_linerange(aes(x = Variable, 
                     ymin = conf.low_90,
                     ymax = conf.high_90),
                 lwd = 1) +
  geom_linerange(aes(x = Variable, 
                     ymin = conf.low_95,
                     ymax = conf.high_95),
                 lwd = 1/2) + 
  ggtitle("Outcome: Disaffiliations, R6") +
  xlab("Attributes")+
  coord_flip() + theme_bw()


ggsave(reg.plot.r6, file="r6regplot.pdf",
       width=6, height=4)

## Note that original degree centralization has no obvious effect
## but that eigenvector centrality really does
## then the higher the initial eigenvector centrality of the original graph
## then the most are out at R5

## in order to plot, need to average rep by round?

tst <- dev.alldat2 %>%
  filter(type== "out") %>%
  group_by(round, shockpercent) %>%
  mutate(round.mean.percout = mean(rep.percent))

gg1 <- ggplot(r5, 
              aes(y=rep.percent, ## rep percent = percent of nodes in 
                 ## "out" condition, given a round 
                  x=rgraph.ecent)) +
  geom_point()+
  theme_bw()+
  facet_wrap(~shockpercent)

gg1

### Percent in "out condition" under all rounds

gg2 <- ggplot(tst, 
              aes(y=round.mean.percout, ## rep percent = percent of nodes in 
                  ## "out" condition, given a round 
                  x=round,
                  fill=rgraph.ecent)) +
  geom_point()+
  theme_bw()+
  facet_wrap(~shockpercent)

gg2


## still no relationship, even when scaled up
gg2 <- ggplot(r5, 
              aes(y=rep.percent, 
                  x=rgraph.degcent)) +
  geom_point()+
  theme_bw()+
  facet_wrap(~shockpercent)

gg2

### Original group centrality?
## Summary: group degree centrality looks like recruit group centrality
## group ecentrality has no clear relationship 
gg3 <- ggplot(r5, 
              aes(y=rep.percent, 
                  x=ggraph.ecent)) +
  geom_point()+
  theme_bw()+
  facet_wrap(~shockpercent)+
  ggtitle("Percentage of defecting nodes \n round 5")

gg3

## Visualize networks with highest initial degree vs eigen recruit centrality

## correlation between degree and eigen centrality in the recruit graphs
## Recruit Networks 278, 123, 353 are high on distribution of ecent, moderate on dist of gcent
plot(dev.alldat2$rgraph.ecent~ dev.alldat2$rgraph.degcent)

ggr1 <- ggplot(data= dev.alldat2,
       aes(y=rgraph.ecent, 
           x= rgraph.degcent, 
           label=iteration)) +
  geom_point() +
  geom_text(size=3.5)

ggr1

ggsave(ggr1, file="offDiagonalRecruitNetworks.pdf")

summary(dev.alldat2$rgraph.ecent) ## max: .56, min .045
summary(dev.alldat2$rgraph.degcent) ## max: .76, min, .13


## Plot to find off-diagonal degree and eigenvector centrality 
## in recruit vs group networks
ggrg1 <- ggplot(data= dev.alldat2,
       aes(y=rgraph.ecent, 
           x= ggraph.ecent, 
           label=iteration)) +
  geom_point() +
  geom_text()
  
ggrg1

ggsave(ggrg1, file="offDiagonalRecruit-GroupNetworks.pdf")

## Plot large eigen vector centrality, moderate degree centrality networks

library(igraph)

gg278 <-  graph_from_edgelist(
  as.matrix(bN[[278]]$edgelist[grepl(pattern="g",
                                     x=bN[[278]]$edgelist$from),
                               c("from", "to")]))


gr278 <-  graph_from_edgelist(
  as.matrix(bN[[278]]$edgelist[grepl(pattern="r",
                                   x=bN[[278]]$edgelist$from),
                             c("from", "to")]))

eig1 <- eigen_centrality(gr278, directed = TRUE)
eiggroup <- eigen_centrality(gg278, directed = TRUE)

V(gr278)$size <- eig1$vector*10
V(gg278)$size <- eiggroup$vector*10
V(gr278)

pdf(file="groupbn278.pdf")
par(mar=c(0,0,0,0)+.1)
fr <- layout_with_fr(gg278)
igraph::plot.igraph(gg278,
                    layout=fr,
                    edge.arrow.size=.1, 
                    vertex.color="darkgreen", 
                    ##vertex.size= 10, 
                      vertex.frame.color="gray", 
                    vertex.label.color="black", 
                    vertex.label.cex=0.8, 
                    vertex.label.dist=2, edge.curved=0.2,
                    seed=6889) 

dev.off()
##

pdf(file="recruitbn278.pdf")

par(mar=c(0,0,0,0)+.1)
fr <- layout_with_fr(gr278)
igraph::plot.igraph(gr278,
                    layout=fr,
                    edge.arrow.size=.1, 
                    vertex.color="darkblue", 
                    ##vertex.size= 10, 
                    vertex.frame.color="gray", 
                    vertex.label.color="black", 
                    vertex.label.cex=0.5, 
                    vertex.label.dist=2, edge.curved=0.2,
                    seed=6889) 
dev.off()

###Looking at 278:

net278 <- alldat %>%
  filter(iteration==278)

View(net278)
