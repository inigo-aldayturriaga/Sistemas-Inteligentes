# Clear environment
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Import the libraries needed to display the results
library(kableExtra)
library(magrittr)

# Include algorithm functions
source("../algorithms/blind/expand-node.R")
source("../algorithms/blind/breadth-first-search.R")
source("../algorithms/blind/depth-first-search.R")
source("../algorithms/blind/depth-limited-search.R")
source("../algorithms/blind/iterative-deepening-search.R")
source("../algorithms/informed/greedy-best-first-search.R")
source("../algorithms/informed/uniform-cost-search.R")

# Include functions for data analysis and result plot
source("../algorithms/results-analysis/analyze-results.R")

# Include the problem
source("../problem/hanoi-tower.R")

# Clear console
cat("\014")
graphics.off()

# Initialize problem
problem <- initialize.problem(rods=6, disks=3, random_actions = FALSE)

bfs_ts <- breadth.first.search(problem, max_iterations = 2500, count_print = 1000)
bfs_gs <- breadth.first.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
dfs_ts <- depth.first.search(problem, max_iterations = 2500, count_print = 1000)
dfs_gs <- depth.first.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
dls_ts <- depth.limited.search(problem, max_iterations = 2500, count_print = 1000, depth_limit = 10)
dls_gs <- depth.limited.search(problem, max_iterations = 2500, count_print = 1000, depth_limit = 10, graph_search = TRUE)
ids_ts <- iterative.deepening.search(problem, count_print = 1000, max_depth = 10)
ids_gs <- iterative.deepening.search(problem, count_print = 1000, max_depth = 10, graph_search = TRUE)
ucs_ts <- uniform.cost.search(problem, max_iterations = 2500, count_print = 1000)
ucs_gs <- uniform.cost.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)
gbs_ts <- greedy.best.first.search(problem, max_iterations = 2500, count_print = 1000)
gbs_gs <- greedy.best.first.search(problem, max_iterations = 2500, count_print = 1000, graph_search = TRUE)

# Analyze the result of all the executions
results <- analyze.results(list(bfs_ts, bfs_gs, 
                                dfs_ts, dfs_gs, 
                                dls_ts, dls_gs, 
                                ids_ts, ids_gs, 
                                ucs_ts, ucs_gs, 
                                gbs_ts, gbs_gs), problem)
# Print results in an HTML Table
kable_material(kbl(results, caption = "Hanoi Tower"), c("striped", "hover", "condensed", "responsive"))
