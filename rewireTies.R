## This is the "disassociate" and "reassociate" step in the Dominios simulation

## Then function to cut the nodes:                                                     
rewire.join <- function(edge.list, node.list, r.seed, round){
    ## For each node in node.list:                                                     
    ## (1) Find the number of ties to cut:                                             
    ## (Extension ideas: make # to reweight a function                                 
    ## of (1) group strength or (2) degree to ideological proximit#y of node to group?
    to.rm <- NULL
    to.add <- data.frame(from=integer(),
                         to=integer())
    
    for(n in node.list){
        print(paste0("cutting ties for node ", n ))

        ## identify subgraph where the node is sending ties:                           
        sub.graph <- edge.list[which(
            edge.list$from==n),]
        
        ## here: subset again for in/out group based                                   
        ## on to.node classification
        
        sub.graph.cut <- sub.graph[which(
            sub.graph$to.type=="recruit"),]

     ##   print(sub.graph.cut)
        ## New ties: (df of "group" nodes)                                             
        sub.graph.new <- edge.list[which(
            edge.list$to.type=="group"),]

       ### print(sub.graph.new)
        ## count number of sending ties in correct zone:                               
        num.t <- dim(sub.graph.cut)[1]
        num.g <- dim(sub.graph.new)[1] ## potential new ties

##        print(class(num.t))
##       print(num.t)
        print(paste0("the node had ", num.t, " ties to same nodes"))
        print(paste0("the node has ", num.g, " potential new ties"))

        if(num.t==0){
            next ##moves to next iteration
        } ## closes case num.t=0
        if(num.g==0){
            next ## move to next iteration
        } ## closes case num.g=0
        ## number of ties to cut:
        set.seed(r.seed)
        
        num.change <- ceiling(runif(min=1,
                                    max=num.t,
                                    n=1))
           
        print(paste0(num.change, " ties to rewire"))
        
        ## Choose which ties to cut:                                                   
        set.seed(r.seed)
        
        to.cut <- sample(size=num.change,
                         x=1:num.t) ## ties to cut                                     
           
        to.make <- sample(size=num.change,
                          x=1:num.g)
        
        
        ## list the ties for the node:                                                 
        ## remove and make selected ties:
        if(to.make>0){
            tmp.cut <- sub.graph.cut[to.cut,"tieID"] ## ties to cut
            
            tmp.make <- sub.graph.new[to.make, "to"]
            new.ties <-data.frame(from=n, to=tmp.make)
            
            ## Data to return                                                        
            to.rm <- c(to.rm, tmp.cut)
            to.add <- rbind(to.add, new.ties)

        }else{## close brace closes case to.make>0
               tmp.cut <- sub.graph.cut[to.cut,"tieID"] ## ties to cut
               to.rm <- c(to.rm, tmp.cut)
               to.add <- to.add ## no change in object, use previous
             ## no change in to.add object
           }## close brace closes case to.make !>0
           ## make a new tieID for the created ties
    } ##close brace closes for n in node list clause
    
    if(dim(to.add)[1]>0){
        to.add$tieID <- paste0("t", round, ".", 1:dim(to.add)[1])
    }else{
        to.add <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("to", "from", "tieID"))))
    }
    
    print(dim(to.add))
    print(head(to.add))

    return(list(Ties.To.Cut=unique(to.rm), Ties.To.Make=to.add))
}

