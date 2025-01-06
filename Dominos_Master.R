## Script that runs all of the Dominos simulation work
## The script comes in two parts:
## the first runs the simulations for ER-PA, ER-ER, ER-PA networks
### Each network structure has two scripts: one to generate networks
## and one to run the simulation. 
## They produce .Rdata workspace files with simulation output and metadata

##%%%%%%%%%%%%%%%%%%%%%%
## ER-PA
##%%%%%%%%%%%%%%%%%%%%%%
source("01simER-PAWideRange.R")
source("ER-PA-SingleScript.R")
## In:ER-PASimDataWide.Rdata
## A workspace file with bN list, numsims, num.recruit, num.group
## seed, 
## Out: ER-PAEvolutionSims.Rdata
## Output is three lists:  node.traj, node.stats, edge.traj

##%%%%%%%%%%%%%%%%%%%%%%
## ER-ER
##%%%%%%%%%%%%%%%%%%%%%%

source("01simER-ERWide.R")
source("ER-ER-SingleScript.R")
## In:ER-ERSimDataWide.Rdata
## A workspace file with bN list, numsims, num.recruit, num.group
## seed, 
## Out: ER-EREvolutionSims.Rdata
## Output is three lists:  node.traj, node.stats, edge.traj
#
##%%%%%%%%%%%%%%%%%%%%%%
## Analysis
##%%%%%%%%%%%%%%%%%%%%%%
###

source("AnalysisBarPlots.R") ## mjf: 12/24-- did I do this?
## I think this is the scaffolding to clean up an exploratory script?
## In: simulation data; identification of types of network
## Out: Bar Plot Visualization of Recruit-Member-Out

source("CentralityStats.R")
## In: Simulation data
## Out:

source("CentralityRegression.R")
## In: centrality statistics produced by CentralityStats.R
## Out: regression models of decay ~ initial network endowments 
