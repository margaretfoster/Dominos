makeGraphObjects <- function(simList, rounds, letter){

    graphsOut <- list()
    for(r in 1:rounds){
        print(r)
        ## edgelist:                                                                   
        rr.el <- as.data.frame(simList[[2]][[r]][,1:2]) %>%
            count(from, to, name="weight") ## edge weights                             

        rr.ndf <- simList[[1]][[r]] ## node-level data                                 

        g <- graph_from_data_frame(rr.el,
                                   directed = TRUE,
                                   vertices = rr.ndf)
        V(g)$infl.on = 100-as.numeric(V(g)$egoW)*100
        V(g)$size <- V(g)$infl.on*0.5
        E(g)$width = E(g)$weight
        E(g)$arrow.size = .2
	V(g)$color <-ifelse(V(g)$type=="group",
                            "tomato",
                            ifelse(V(g)$type=="recruit",
                                   "steel blue",
                                   "gray50"))
        graphsOut[[r]] <- assign(paste0(letter,r), g)
    }

    return(graphsOut)
}
