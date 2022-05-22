local.beam.search = function(problem,
                                max_iterations = 1000, 
                                count_print = 100, 
                                trace = FALSE,
                                beams = 3) {
  
  name_method      <- paste0("Local Beam Search")
  state_initial    <- problem$state_initial
  
  for(i in 1:beams){
    state_initial <- sample(c(1:problem$size),problem$p)
  }
  actions_possible <- problem$actions_possible
  
  # Get Start time
  start_time       <- Sys.time()
  current_nodes <- list()
  
  for (i in 1:beams) {
    node_current <- list(parent = c(),
                         state = state_initial,
                         actions = c(),
                         depth = 1,
                         cost = get.cost(state = state_initial, problem = problem),
                         evaluation = get.evaluation(state_initial, problem))
    
    current_nodes[[i]] <- node_current
  }
  
 
  
  count <- 1
  end_reason <- 0
  
  #Initialization of information for further analysis
  report <- data.frame(iteration = numeric(),
                       nodes_frontier = numeric(),
                       depth_of_expanded = numeric(),
                       nodes_added_frontier = numeric())
  
  #Perform "max_iterations" iterations of the expansion process of the first node in the frontier list
  while (count <= max_iterations) {
    # Print a search trace for each "count_print" iteration
    if (count %% count_print == 0) {
      print(paste0("Iteration: ", count, ", Current node=", node_current$cost, " / needed=", problem$needed_slices), quote = FALSE)
    }
    
    #If "trace" is on, the information of current node is displayed
    if (trace) {
      print(paste0("Current node=", current_nodes[[1]]$cost, " / needed=", problem$needed_slices), quote = FALSE)
      to.string(state = current_nodes[[1]]$state, problem = problem)
    }
    ##########################################
    siguientes_nodos_beam <- list()
    for(i in current_nodes){
      siguiente_nodos <- local.expand.node(i,actions_possible,problem)
      siguientes_nodos_beam <- append(siguientes_nodos_beam,siguiente_nodos)
      
    }
    
    
    siguientes_nodos_beam <- siguientes_nodos_beam[order(sapply(siguientes_nodos_beam,function (x) x$evaluation))]
    siguientes_nodos_beam <- siguientes_nodos_beam[1:beams]
    current_nodes <- current_nodes[order(sapply(current_nodes,function (x) x$evaluation))]
    best_nodo <- siguientes_nodos_beam[[1]]
    
    if(best_nodo$evaluation <= current_nodes[[1]]$evaluation){
      current_nodes <- siguientes_nodos_beam
      if (trace){
        print(paste0("New current node=", node_current$cost, " / needed=", problem$needed_slices), quote = FALSE)
        to.string(state = node_current$state, problem = problem)
      }
      
    #############################################
    } else {
      # Algorithm stops because a local best has been found
      end_reason <- "Local_Best"
      
      #Add of information for further analysis
      report <- rbind(report, data.frame(iteration = count,
                                         nodes_frontier = 1,
                                         depth_of_expanded = node_current$depth,
                                         nodes_added_frontier = 1))
      
      break
    }
    
    #Add of information for further analysis
    report <- rbind(report, data.frame(iteration = count,
                                       nodes_frontier = 1,
                                       depth_of_expanded = node_current$depth,
                                       nodes_added_frontier = 1))
    count <- count + 1
  }
  
  # Get runtime
  end_time <- Sys.time()
  
  result <- list()
  result$name    <- name_method
  result$runtime <- end_time - start_time
  
  # Print final result
  if (end_reason == "Local_Best") {
    print("Local best found!!", quote = FALSE)
  } else {
    print("Maximum iterations reached", quote = FALSE)
  }
  
  print(to.string(state = current_nodes[[1]], problem = problem))
  
  result$state_final <- current_nodes[[1]]
  result$report      <- report
  
  return(result)
}

