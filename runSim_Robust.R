## This is a Claude.AI restructuring of the original 
## runSim.R script, which I designed with a for-loop.
## I asked for a more robust version of the script.
## It broke out sub-functions, reduced duplicated code

## results <- runSim(
#nodeInfo = your_node_data,
#edgeInfo = your_edge_data,
#sim.length = 10,
#rseed = 42,
#rshock = 5,
#pts = 0.5
#)

# Improved version of runSim with better error handling and structure
runSim <- function(nodeInfo, edgeInfo, sim.length, rseed, rshock = NULL, pts) {
  # Input validation
  validateInputs <- function(nodeInfo, edgeInfo, sim.length, rseed, pts) {
    if (!is.data.frame(nodeInfo)) stop("nodeInfo must be a data frame")
    if (!is.data.frame(edgeInfo)) stop("edgeInfo must be a data frame")
    if (!is.numeric(sim.length) || sim.length < 2) stop("sim.length must be numeric and >= 2")
    if (!is.numeric(rseed)) stop("rseed must be numeric")
    if (pts < 0 || pts > 1) stop("shock proportion must be in [0,1]")
    
    required_node_cols <- c("nodeID", "nodeIdeo", "nodeThresh", "egoW")
    if (!all(required_node_cols %in% colnames(nodeInfo))) {
      stop("nodeInfo missing required columns: ", 
           paste(setdiff(required_node_cols, colnames(nodeInfo)), collapse = ", "))
    }
  }
  
  # Helper function to convert columns to numeric
  ensureNumeric <- function(df, cols) {
    df[cols] <- lapply(df[cols], as.numeric)
    return(df)
  }
  
  # Helper function to process a single simulation step
  processSimStep <- function(step, allnodes, edge.list, rshock, pts, rseed) {
    tryCatch({
      # Create adjacency matrix
      gg <- graph_from_data_frame(edge.list[[step-1]], directed = TRUE)
      am <- as_adj(gg, sparse = FALSE)
      
      if (nrow(am) == 0) return(NULL)  # Early termination for empty networks
      
      # Handle different simulation cases
      if (step == 2) {
        return(handleStep2(am, allnodes, edge.list, step, rseed))
      } else if (step == (rshock + 1)) {
        return(handleShockStep(am, allnodes, edge.list, step, pts, rseed))
      } else {
        return(handleRegularStep(am, allnodes, edge.list, step, rseed))
      }
    }, error = function(e) {
      warning(sprintf("Error in simulation step %d: %s", step, e$message))
      return(NULL)
    })
  }
  
  # Helper function for step 2 processing
  handleStep2 <- function(am, allnodes, edge.list, step, rseed) {
    nodeIdeoUpdate <- ideologyUpdator(
      adjMatrix = am,
      nodeIdeology = as.numeric(allnodes[[step-1]]$nodeIdeo),
      egoWeights = as.numeric(allnodes[[step-1]]$egoW)
    )
    
    updated <- merge(allnodes[[step-1]], nodeIdeoUpdate, by = "nodeID")
    allnodes[[step]] <- updated
    
    # Update thresholds and edges
    results <- updateThreshsAndEdges(allnodes, edge.list, step, rseed)
    return(results)
  }
  
  # Helper function for shock step processing
  handleShockStep <- function(am, allnodes, edge.list, step, pts, rseed) {
    if (!"i2" %in% colnames(allnodes[[step-1]])) return(NULL)
    
    # Apply shock to selected nodes
    shocked_nodes <- applyShock(allnodes[[step-1]], pts)
    allnodes[[step-1]] <- shocked_nodes
    
    # Update ideology post-shock
    nodeIdeoUpdate <- ideologyUpdator(
      adjMatrix = am,
      nodeIdeology = as.numeric(allnodes[[step-1]]$i2),
      egoWeights = as.numeric(allnodes[[step-1]]$egoW)
    )
    
    updated <- merge(
      allnodes[[2]][, !names(allnodes[[2]]) %in% c("i2")],
      nodeIdeoUpdate,
      by = "nodeID"
    )
    
    allnodes[[step]] <- updated
    results <- updateThreshsAndEdges(allnodes, edge.list, step, rseed)
    return(results)
  }
  
  # Helper function for regular step processing
  handleRegularStep <- function(am, allnodes, edge.list, step, rseed) {
    if (!"i2" %in% colnames(allnodes[[step-1]])) return(NULL)
    
    nodeIdeoUpdate <- ideologyUpdator(
      adjMatrix = am,
      nodeIdeology = as.numeric(allnodes[[step-1]]$i2),
      egoWeights = as.numeric(allnodes[[step-1]]$egoW)
    )
    
    updated <- merge(
      allnodes[[step-1]][, !names(allnodes[[step-1]]) %in% c("i2")],
      nodeIdeoUpdate,
      by = "nodeID"
    )
    
    allnodes[[step]] <- updated
    results <- updateThreshsAndEdges(allnodes, edge.list, step, rseed)
    return(results)
  }
  
  # Helper function to update thresholds and edges
  updateThreshsAndEdges <- function(allnodes, edge.list, step, rseed) {
    threshs <- updateThreshs(
      nodedf = allnodes[[step]],
      ideologyvar = "i2",
      threshvar = "nodeThresh"
    )
    
    allnodes[[step]]$aboveT <- threshs$above
    allnodes[[step]]$belowT <- threshs$below
    
    results <- callEdges(allnodes = allnodes, edge.list = edge.list, r = step, rseed = rseed)
    return(results)
  }
  
  # Helper function to apply shock to nodes
  applyShock <- function(nodes_df, pts) {
    starting_r_nodes <- grep(x = nodes_df$nodeID, pattern = "r")
    num_to_shock <- floor(length(starting_r_nodes) * pts)
    
    if (num_to_shock > 0) {
      sel <- sample(starting_r_nodes, size = num_to_shock, replace = FALSE)
      nodes_to_shock <- nodes_df$nodeID[sel]
      nodes_df[which(nodes_df$nodeID %in% nodes_to_shock), "i2"] <- 
        nodes_df[which(nodes_df$nodeID %in% nodes_to_shock), "i2"] / 2
    }
    
    return(nodes_df)
  }
  
  # Main execution
  tryCatch({
    # Validate inputs
    validateInputs(nodeInfo, edgeInfo, sim.length, rseed, pts)
    
    # Source required files
    required_files <- c("ideologyUpdatorDebug.R", "updateThreshs.R", "callLoopEdges.R")
    for (file in required_files) {
      if (!file.exists(file)) stop(sprintf("Required file %s not found", file))
      source(file)
    }
    
    # Initialize data structures
    allnodes <- list()
    edge.list <- list()
    
    # Initialize first round
    allnodes[[1]] <- ensureNumeric(nodeInfo, c("nodeIdeo", "nodeThresh", "egoW"))
    allnodes[[1]]$round <- 1
    edge.list[[1]] <- edgeInfo
    
    # Run simulation steps
    for (step in 2:sim.length) {
      message(sprintf("Processing simulation step: %d", step))
      
      result <- processSimStep(step, allnodes, edge.list, rshock, pts, rseed)
      if (is.null(result)) break
      
      allnodes <- result$nodeSims
      edge.list <- result$edgeSims
    }
    
    return(list(nodeSims = allnodes, edgeSims = edge.list))
    
  }, error = function(e) {
    stop(sprintf("Simulation failed: %s", e$message))
  })
}