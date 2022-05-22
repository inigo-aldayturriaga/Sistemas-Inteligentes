# Clear environment and console
rm(list=ls())
cat("\014")
graphics.off()
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Import the libraries needed to display the results
library(kableExtra)
library(magrittr)

# Include algorithm functions
source("../algorithms/blind/expand-node.R")
source("../algorithms/informed/hill-climbing-search.R")
source("../algorithms/informed/random-restart-hill-climbing-search.R")
source("../algorithms/informed/local-beam-search.R")

# Include functions for data analysis and result plot
source("../algorithms/results-analysis/analyze-results.R")

# Include the problem
source("../problem/p-hub-problem.R")

execute.local.beam.search <- function(problem) {
  
  return(local.beam.search(problem = problem))
}

test.local.beam.search <- function(filename, p, beams) {
  results <- vector(mode = "list", length = 1)
  problem <- initialize.problem(filename = filename, p = p)
  results[[1]] <- execute.local.beam.search(problem)
  results_df <- local.analyze.results(results, problem)
  
  print(paste0("Best evaluation: ", round(min(results_df$Evaluation), 2),
               " - Mean: ", round(mean(results_df$Evaluation), 2),
               " - SD: ", round(sd(results_df$Evaluation), 2)), quote = FALSE)
  print(paste0("Best runtime: ", round(min(results_df$Runtime), 2),
               " - Mean: ", round(mean(results_df$Runtime), 2),
               " - SD: ", round(sd(results_df$Runtime), 2)), quote = FALSE)
  
  return(results_df)
}


execute.random.restart.hill.climbing <- function(filename, p, times){
  
  results <- vector(mode = "list", length = times)
  problems <- vector(mode = "list", length = times)
  
  for(i in 1:times){
    problems[[i]] <- initialize.problem(filename,p)
    results[[i]] <- random.restart.hill.climbing.search(problems[[i]])
  }
  
  results_df <- compare.analyze.results(results,problems)
  
  print(paste0("Best evaluation: ", round(min(results_df$Evaluation), 2),
               " - Mean: ", round(mean(results_df$Evaluation), 2),
               " - SD: ", round(sd(results_df$Evaluation), 2)), quote = FALSE)
  print(paste0("Best runtime: ", round(min(results_df$Runtime), 2),
               " - Mean: ", round(mean(results_df$Runtime), 2),
               " - SD: ", round(sd(results_df$Runtime), 2)), quote = FALSE)
  
  return(results_df)
  
}

test.random.restart.hill.climbing <- function(file, p, times) {
  results <- vector(mode = "list", length = times)
  for (i in 1:times) {
    results[[i]] <- execute.random.restart.hill.climbing(filename = file, p = p)
  }
}
  
best.solution <- function(results_df){
  best_results <- results_df[order(results_df$Evaluation),]
  return(best_results[1,])

}

# Executes hill climbing search and return the results
execute.hill.climbing <- function(filename, p) {
  # Initialize problem
  problem <- initialize.problem(p = p, filename = filename)
  # Execute hill climbing
  return(hill.climbing.search(problem = problem))
}

# Execute Hill Climbing several times and analyze results
test.hill.climbing <- function(file, p, times) {
  # Execute hill climbing 'n' times
  results <- vector(mode = "list", length = times)
  for (i in 1:times) {
    results[[i]] <- execute.hill.climbing(filename = file, p = p)
  }
  
  # Initialize a problem instance for the analysis
  problem <- initialize.problem(filename = file, p = p)
  
  # Analyze results
  results_df <- local.analyze.results(results, problem)
  
  print(paste0("Best evaluation: ", round(min(results_df$Evaluation), 2), 
               " - Mean: ", round(mean(results_df$Evaluation), 2), 
               " - SD: ", round(sd(results_df$Evaluation), 2)), quote = FALSE)
  print(paste0("Best runtime: ", round(min(results_df$Runtime), 2), 
               " - Mean: ", round(mean(results_df$Runtime), 2), 
               " - SD: ", round(sd(results_df$Runtime), 2)), quote = FALSE)
  
  return(results_df)
}

# Clear console
cat("\014")
graphics.off()

file        <- "../data/p-hub/AP100.txt"
p           <- 3
times       <- 10
results_df  <- test.hill.climbing(file, p, times)
hc1       <- best.solution(results_df)
#Print results in an HTML Table
kable_material(kbl(results_df, caption = "p-hub AP100"),  c("striped", "hover", "condensed", "responsive"))

# Clear console
cat("\014")
graphics.off()

file        <- "../data/p-hub/AP100.txt"
p           <- 3
times       <- 20
results_df  <- test.hill.climbing(file, p, times)
hc2       <- best.solution(results_df)
#Print results in an HTML Table
kable_material(kbl(results_df, caption = "p-hub AP100"),  c("striped", "hover", "condensed", "responsive"))

# Clear console
cat("\014")
graphics.off()

file        <- "../data/p-hub/AP100.txt"
p           <- 3
times       <- 50
results_df  <- test.hill.climbing(file, p, times)
hc3       <- best.solution(results_df)
#Print results in an HTML Table
kable_material(kbl(results_df, caption = "p-hub AP100"),  c("striped", "hover", "condensed", "responsive"))




# cat("\014")
# graphics.off()
# 
# file        <- "../data/p-hub/AP100.txt"
# p           <- 3
# beams       <- 3
# results_df  <- test.local.beam.search(file, p, beams)
# lcb1       <- best.solution(results_df)
# #Print results in an HTML Table
# kable_material(kbl(results_df, caption = "p-hub AP100"),  c("striped", "hover", "condensed", "responsive"))
# 
# cat("\014")
# graphics.off()
# 
# file        <- "../data/p-hub/AP100.txt"
# p           <- 3
# beams       <- 5
# results_df  <- test.local.beam.search(file, p, beams)
# lcb2       <- best.solution(results_df)
# #Print results in an HTML Table
# kable_material(kbl(results_df, caption = "p-hub AP100"),  c("striped", "hover", "condensed", "responsive"))
# 
# cat("\014")
# graphics.off()
# 
# file        <- "../data/p-hub/AP100.txt"
# p           <- 3
# beams       <- 10
# results_df  <- test.local.beam.search(file, p, beams)
# lcb3       <- best.solution(results_df)
# #Print results in an HTML Table
# kable_material(kbl(results_df, caption = "p-hub AP100"),  c("striped", "hover", "condensed", "responsive"))


 # file        <- "../data/p-hub/AP100.txt"
 # p           <- 3
 # times       <- 10
 # results_df  <- execute.random.restart.hill.climbing(file, p, times)
 # rshc1      <- best.solution(results_df)
 # 
 # # Print results in an HTML Table
 # kable_material(kbl(results_df, caption = "p-hub AP100"),  c("striped", "hover", "condensed", "responsive"))
 # 
 # file        <- "../data/p-hub/AP100.txt"
 # p           <- 3
 # times       <- 20
 # results_df  <- execute.random.restart.hill.climbing(file, p, times)
 # rshc2      <- best.solution(results_df)
 # 
 # 
 # # Print results in an HTML Table
 # kable_material(kbl(results_df, caption = "p-hub AP100"),  c("striped", "hover", "condensed", "responsive"))
 # 
 # file        <- "../data/p-hub/AP100.txt"
 # p           <- 3
 # times       <- 50
 # results_df  <- execute.random.restart.hill.climbing(file, p, times)
 # rshc3      <- best.solution(results_df)
 # 
 # 
 # # Print results in an HTML Table
 # kable_material(kbl(results_df, caption = "p-hub AP100"),  c("striped", "hover", "condensed", "responsive"))
 
