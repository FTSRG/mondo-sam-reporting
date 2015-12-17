# Sample script for processing and visualizing the benchmark results.
# 
# The script aggregates the data and transforms it to a wide table suited for visualization.
# The aggregration takes the **average** time required by the individual steps in the phases. 
# For multiple runs, the script takes the **minimum** value.
#
# The basic workflow for the script is the following:
# * load the file from the CSV
# * filter the data
# * convert the data from long table to wide table (better suited for processing)
# * aggregate the data
# * convert the data to long table which (better suited for visualization)
# * draw the plots

library(reshape2)
library(plyr)

source("plot.R")

results = read.csv("results.csv")

# filtering for time values
times = subset(results, Metric == "Time")

# convert nanoseconds to seconds
times$Value = times$Value / 10^9
# replace underscore with space in tool names
times$Tool = gsub('_', ' ', times$Tool)

# transform long table to wide table
times.wide = dcast(times,
                   Scenario + Tool + Run + Case + Artifact + Iteration + Metric ~ Phase,
                   value.var = "Value")

# calculate aggregated values
times.derived = times.wide
times.derived$Read.and.Check = times.derived$Read + times.derived$Check
times.derived$Transformation.and.Recheck = times.derived$Transformation + times.derived$Recheck

# summarize for each value (along the **Iteration** attribute) using a columnwise function
# there might be NAs as some phases (e.g. Read) are not executed repeatedly
times.aggregated.iterations = ddply(
  .data = times.derived,
  .variables = c("Scenario", "Tool", "Run", "Case", "Artifact", "Metric"),
  .fun = colwise(mean, na.rm = TRUE),
  .progress = "text"
)
# drop the **Iteration** attribute
times.aggregated.iterations = subset(times.aggregated.iterations, select=-c(Iteration))

# summarize for each value (along the **Run** attribute) using a columnwise function
times.aggregated.runs = ddply(
  .data = times.aggregated.iterations,
  .variables = c("Scenario", "Tool", "Case", "Artifact", "Metric"),
  .fun = colwise(median),
  .progress = "text"
)
# drop the **Run** attribute
times.aggregated.runs = subset(times.aggregated.runs, select=-c(Run))

# melt data to a wide table
times.plot = melt(
  data = times.aggregated.runs,
  id.vars = c("Scenario", "Tool", "Case", "Artifact", "Metric"),
  measure.vars = c("Read", "Check", "Read.and.Check", "Transformation", "Recheck", "Transformation.and.Recheck"),
  variable.name = "Phase",
  value.name = "Time"
)

# remove the . characters from the phasename
times.plot$Phase = gsub('\\.', ' ', times.plot$Phase)

# modelsizes
modelsize.batch  = data.frame(Scenario = "Batch",  Artifact = 2^(0:14), Triples = c("4.7k", "7.9k", "20.6k", "41k", "89.4k", "191.8k", "374.1k", "716.5k", "1.5M", "2.8M", "5.7M", "11.5M", "23M", "45.9M", "92.3M"))
modelsize.inject = data.frame(Scenario = "Inject", Artifact = 2^(0:14), Triples = c("5k", "9.3k", "19.9k", "44.6k", "85.7k", "191.6k", "373.1k", "752.8k", "1.5M", "3M", "5.8M", "11.6M", "23.3M", "46.5M", "93M"))
modelsize.repair = data.frame(Scenario = "Repair", Artifact = 2^(0:14), Triples = c("4.9k", "9.3k", "19.8k", "44.5k", "85.4k", "191.1k", "372.1k", "750.7k", "1.5M", "2.9M", "5.8M", "11.5M", "23.2M", "46.4M", "92.8M"))
modelsizes = do.call(rbind, list(modelsize.batch, modelsize.inject, modelsize.repair))

# levels for the facets in the plot
levels.cases = c("PosLength", "SwitchSensor", "RouteSensor", "SwitchSet", "ConnectedSegments", "SemaphoreNeighbor")

scenario = "Repair"
facet_cols = 2
legend_cols = 4

# draw plots
#benchmark.plot.by.case(times.plot, scenario, modelsizes, levels.cases, "Read", "read phase", facet_cols, legend_cols)
benchmark.plot.by.case(times.plot, scenario, modelsizes, levels.cases, "Check", "check phase", facet_cols, legend_cols)
#benchmark.plot.by.case(times.plot, scenario, modelsizes, levels.cases, "Read and Check", "read and check phase", facet_cols, legend_cols)
#benchmark.plot.by.case(times.plot, scenario, modelsizes, levels.cases, "Transformation", "transformation phase", facet_cols, legend_cols)
#benchmark.plot.by.case(times.plot, scenario, modelsizes, levels.cases, "Recheck", "recheck phase", facet_cols, legend_cols)
#benchmark.plot.by.case(times.plot, scenario, modelsizes, levels.cases, "Transformation and Recheck", "transformation and recheck phase", facet_cols, legend_cols)
