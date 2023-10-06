## This script is for nodes that fall under the
## join threashold:

## This function to cut ties with the group;
## Idea: two layers of disaffiliation.
## Med threshold  = remove ties that are in the group
## very low threshold = cut all group ties

cut.to.leave <- function(edge.list, node.list, r.seed, round){
    ## For each node in node.list:                                                     
    ## (1) Find the number of ties to cut:                                             
    ## (Extension ideas: make # to reweight a function                                 
    ## of (1) group strength or (2) degree ot ideological proximit#y of node to group?
    to.rm <- NULL
    to.add <- data.frame(from=integer(),
                         to=integer())

    
    for(n in node.list){
        print(paste0("cutting ties for node ", n ))

        ## identify subgraph where the node is sending ties:                           
        sub.graph <- edge.list[which(
            edge.list$from == n),]

        print("sub.graph dimensions are:")
        print(dim(sub.graph))
        
        ## here: subset again for in/out group based                                   
        ## on to.node classification
        
        sub.graph.cut <- sub.graph[which(
            sub.graph$to.type=="group"),]
        
        ## count number of sending ties in correct zone:                               
        num.t <- dim(sub.graph.cut)[1]
        
        ## here if the sub.graph.cut ==0, return previous list
        if(num.t==0){
            print("The node is an isolate")
            return(Ties.To.Cut="none") ## return nothing
        }
        
        print(paste0("the node had ", num.t, " existing ties"))
        
        ## number of ties to cut:
	set.seed(r.seed)
        
        num.change <- ceiling(runif(min=1,
                                    max=num.t,
                                    n=1))
        print(paste0(num.change, " ties to cut"))

        ## Choose which ties to cut:                                                   
        set.seed(r.seed)
        
        to.cut <- sample(size=num.change,
                         x=1:num.t) ## ties to cut                                     
        
        ## list the ties for the node:                                                 
        ## remove and make selected ties:
        tmp.cut <- sub.graph.cut[to.cut,"tieID"] ## ties to cut
        
        ## Data to return                                                        
        to.rm <- c(to.rm, tmp.cut)
    }
    
    return(Ties.To.Cut=unique(to.rm))
}
