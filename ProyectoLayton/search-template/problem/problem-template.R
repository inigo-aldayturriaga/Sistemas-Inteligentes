# =======================================================================
# Group Name:17
# Students:Lander Aguirre e Iñigo Aldayturriaga
# =======================================================================
# You must implement the different functions according to your problem.
# You cannot modify the function headers because they are used by the 
# search algorithms. If you modify any headers the algorithms may not work.
# =======================================================================

# This function must return a list with the information needed to solve the problem.
# (Depending on the problem, it should receive or not parameters)
initialize.problem <- function(file) {
rm(list=ls())
cat("\014")
graphics.off()
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
file <- "../data/feet-maze-3b.txt"
  problem <- list() # Default value is an empty list.
  problem$size              <- read.csv(file,nrows =1 ,sep=";",header = FALSE)
  problem$filas             <- problem$size[[1]]
  problem$cols              <- problem$size[[2]]
  problem$matrix            <- read.csv(file,nrows = problem$filas,sep=";",header = FALSE,skip = 1)
  problem$state_initial     <- read.csv(file,nrows = 1,sep=",",header = FALSE,skip=problem$filas+1)
  problem$state_final       <- read.csv(file,nrows = 1,sep=",",header = FALSE,skip=problem$filas+2)
  problem$actions_possible  <- data.frame(direction = c("Up", "Down", "Left", "Right"), stringsAsFactors = FALSE)
  problem$wallsleft         <- as.character(read.csv(file,nrows = 1,sep = ";",header = FALSE,skip = problem$filas+3))
  problem$wallsright        <- as.character(read.csv(file,nrows = 1,sep = ";",header = FALSE,skip = problem$filas+4))
  problem$wallsdown         <- as.character(read.csv(file,nrows = 1,sep = ";",header = FALSE,skip = problem$filas+5))
  problem$wallsup           <- as.character(read.csv(file,nrows = 1,sep = ";",header = FALSE,skip = problem$filas+6))

  
  # You can add additional attributes
  # problem$<aditional_attribute>  <- <INSERT CODE HERE>
  
  return(problem)

}

is.applicable <- function (state, action, problem) {
  
  result <- FALSE
  fila <- as.integer(state[2])
  col <- as.integer(state[1])
  
  if (action == "Up") {
    if(fila>0){
      if(!paste0(col,",",fila) %in% problem$wallsup){
        if(problem$matrix[fila+1,col+1] != problem$matrix[fila,col+1]){
          result <- TRUE
        }
      }
    }
  }
  
  if (action == "Down") {
    if(fila<problem$filas-1){
      if(!paste0(col,",",fila) %in% problem$wallsdown){
        if(problem$matrix[fila+1,col+1] != problem$matrix[fila+2,col+1]){
          result <- TRUE
        }
      }
    }
  }
  
  if (action == "Left") {
    if(col>0){
      if(!paste0(col,",",fila) %in% problem$wallsleft){
        if(problem$matrix[fila+1,col+1] != problem$matrix[fila+1,col]){
          result <- TRUE
        }
      }
    }
  }
  
  if (action == "Right") {
    if(col<problem$cols-1){
      if(!paste0(col,",",fila) %in% problem$wallsright){
        if(problem$matrix[fila+1,col+1] != problem$matrix[fila+1,col+2]){
          result <- TRUE
        }
      }
    }
  }
  
  return(result)
}


# Returns the state resulting on applying the action over the state
effect <- function (state, action, problem) {
  result <- state # Default value is the current state.
  
  fila <- state[2]
  col <- state[1]
  
  if (action == "Up") {
    result[2] <- result[2]-1
  }
  
  if (action == "Down") {
    result[2] <- result[2]+1
  }
  
  if (action == "Left"){
    result[1] <- result[1] - 1
  }
  
  if (action == "Right"){
    result[1] <- result[1] + 1
  }
  
  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_state, problem) {
  result <- FALSE # Default value is FALSE.
  if(state[1] == final_state[1] && state[2] == final_state[2]){
    result <- TRUE
  } 
  return(result)

}

# Transforms a state into a string
to.string = function (state, problem) {
  
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  
  return(1) # Default value is 1.
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
  
	return(1) # Default value is 1.
}

