# calculate the estimated memory consumption of the executed Java application

library(reshape2)
library(plyr)
library(ggplot2)

results = read.csv("../trainbenchmark/results/results.csv", header=FALSE)
colnames(results) = c("Scenario", "Tool", "Run", "Case", "Artifact", "Phase", "Iteration", "Metric", "Value")

# filter on the MaxMemory metric, throw away unused columns
memories = subset(results, Metric == "MaxMemory")
memories = subset(memories, select = -c(Phase, Iteration, Run, Metric))
memories = unique(memories)

# get the minimum value for each (scenario, tool, case, artifact)
minimum.memories = ddply(
  .data = memories,
  .variables = c("Scenario", "Tool", "Case", "Artifact"),
  .fun = colwise(min),
  .progress = "text"
)
minimum.memories
