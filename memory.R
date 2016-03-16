# calculate the memory consumption of the executed Java application

library(reshape2)
library(plyr)
library(ggplot2)
library(xtable)

results = read.csv("../trainbenchmark/results/results.csv", header=FALSE)
colnames(results) = c("Scenario", "Tool", "Run", "Case", "Artifact", "Phase", "Iteration", "Metric", "Value")

x = results[results$Tool == "EMF_API" & results$Metric == "MaxMemory", ]


# filter on the MaxMemory metric, throw away unused columns
memories = subset(results, Metric == "MaxMemory")
memories = subset(memories, select = -c(Phase, Iteration, Metric))
memories = unique(memories)


minimum.memories = ddply(
  .data = memories,
  .variables = c("Scenario", "Tool", "Case", "Artifact", "Run"),
  .fun = colwise(min),
  .progress = "text"
)

head(minimum.memories)
minimum.memories.count = ddply(
  .data = minimum.memories,
  .variables = c("Scenario", "Tool", "Case", "Artifact", "Value"),
  .fun = colwise(sum), #count does not work
  .progress = "text"
)

head(minimum.memories.count)

minimum.memories.count[minimum.memories.count$Case == "RouteSensor-ConnectedSegments-PosLength-SemaphoreNeighbor-SwitchSensor-SwitchSet" & minimum.memories.count$Run == 15, ]
head(minimum.memories.count)
