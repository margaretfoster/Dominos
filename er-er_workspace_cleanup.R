## Break up the huge .Rdata file into smaller items to work with:
setwd("~/Dropbox/DominosPaper/Dominos-Code/code2022/")

## ER-ER
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
rm(node.stats)
rm(out) ## need to check what this is...
## 
##load("ER-PAEvolutionSims.Rdata") ## 1k long
##netstype= "ER-PA" ## code network types

##load("PA-PAEvolutionSims.Rdata") ## 10k long
##netstype= "PA-PA" ## code network types

