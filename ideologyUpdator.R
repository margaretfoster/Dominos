
## Ideology Updator for Dominos
## Algorithm:
## the ideology updator is a weighted average of the attention that a node pays to others (out degree)
## and their own tendency to take their own council (ego weight)
## Because the out-degree varies by node, the general implementation is:
## 1) use the ego weight proportion [exogenous] to figure out what the outward attention weight is (1-ego weight)
## 2) use the attention weight and the number of outgoing ties to figure out what the denominator of the scaling is

## general updator:
## For all ties: SUM [weight_i Ideo_i] + weight_ego Ideo_ego 

## The "Ego weight" is \in [0,1] and is actually a proportion of the overall weights to assign to the ego ideology

ideologyUpdator <- function(adjMatrix,
                            nodeIdeology,
                            egoWeights){
                                
 print("opening ideology updator")

    ## Figure out denominator on weights:
    attention.out <- (1-egoWeights)*10
    total.weights <- (rowSums(adjMatrix)*10)/attention.out
    node.ego.weight <- total.weights*egoWeights

    ## via matrix multiplication was returning me just the same output:
    ## so going back to the workhorse for-loop:
    i2 <- NULL
    
    print("passing through...")
    print(adjMatrix)
   
   for(d in 1:dim(adjMatrix)[1]){ ## for every row in the adjacency matrix
        s1 <-  sum(adjMatrix[d,]*nodeIdeology)
        s2 <- node.ego.weight[d]* nodeIdeology[d]
        u <- (s1+s2)/total.weights[d] ## updated
        i2 <- c(i2, u)
    }

    ## make into a dataframe to index to nodeID
   
    i2out <- i2
    i2out$nodeID <- rownames(i2)
    i2out$id <- round(i2out$i2, 2)

    return(i2out) ## return updated ideologies, round for aesthetics 
}
