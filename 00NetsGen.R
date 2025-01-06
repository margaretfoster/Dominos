
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
  
  
##%%%%%%%%%%%%%%%%%%%%%%
## PA-PA
##%%%%%%%%%%%%%%%%%%%%%%

source("01simPA-PAWide.R")
source("PA-PA-SingleScript.R")


##%%%%%%%%%%%%%%%%%%%%%%
## Analysis
##%%%%%%%%%%%%%%%%%%%%%%

source("AnalysisBarPlots.R")
