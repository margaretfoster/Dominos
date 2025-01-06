## Dominos exploratory results:

library(dplyr)
library(igraph)
library(ggplot2)

setwd("~/Dropbox/DominosPaper/Dominos-Code/code2022/")

datapath = "./results/"
## load: 
## node.stats
load(paste0(datapath, "nodetraj_ER-ER_2025-01-04.Rdata"))
## edge.traj
load(paste0(datapath, "edgetraj_ER-ER_2025-01-04.Rdata"))

sim_length = 7
  
## ER-ER summary of in group vs defected at the end of the rounds:
## grouped by shock percent
## shows how shock rates influence retention
## interesting result: U-shaped 
resilience_summary <- bind_rows(node.stats) %>%
  filter(round == sim.length) %>%  # Look at final states
  group_by(shockpercent) %>%
  summarize(
    mean_retention = mean(percent[type == "group"]),
    sd_retention = sd(percent[type == "group"]),
    mean_defection = mean(percent[type == "out"]),
    sd_defection = sd(percent[type == "out"]),
    n_networks = n_distinct(iteration)
  ) %>% round(2)


### Immediate versus long-term effects of shocks:

temporal_response <- bind_rows(lapply(node.traj, function(x) {
  bind_rows(x) %>%
    group_by(shockpercent, round) %>%
    summarize(
      prop_active = mean(active),
      prop_retained = mean(type == "group"),
      n_nodes = n_distinct(nodeID)
    )
})) %>%
  group_by(shockpercent, round) %>%
  summarize(
    mean_active = mean(prop_active),
    sd_active = sd(prop_active),
    mean_retained = mean(prop_retained),
    sd_retained = sd(prop_retained)
  )

## Initial structure:
## Linking initial structure to outcomes:

structure_effects <- lapply(1:length(edge.traj), function(i) {
  # First time point
  tmpmat <- as.matrix(edge.traj[[i]][[1]]$edgeSims[[1]])
  ## convert to igraph object:
  initial_network <- igraph::graph_from_edgelist(tmpmat[,1:2], directed=FALSE)
  final_states <- node.stats[[i]]
  
  # Calculate initial network properties
  # Combine with final outcomes
  data.frame(
    network_id = i,
    initial_density = edge_density(initial_network),
    initial_clustering = transitivity(initial_network),
    final_retention = final_states$percent[final_states$round == sim.length],
    shock_magnitude = final_states$shockpercent
  )
}) %>% bind_rows()
