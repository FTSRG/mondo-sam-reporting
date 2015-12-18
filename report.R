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
library(R6)

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
times.derived$Init.and.Check = times.derived$Init + times.derived$Check

# summarize for each value (along the **Iteration** attribute) using a columnwise function
# there might be NAs as some phases (e.g. Init) are not executed repeatedly
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
  measure.vars = c("Init", "Check", "Init.and.Check", "Transformation"),
  variable.name = "Phase",
  value.name = "Time"
)

# remove the . characters from the phasename
times.plot$Phase = gsub('\\.', ' ', times.plot$Phase)

# modelsizes

# draw plots
tp = times.plot

## Query scenario

plot.format = PlotFormat$new(width=210, height=210, facet_cols=2, legend_cols=2)

df = tp[tp$Scenario == "Query" & tp$Phase == "Init", ]
benchmark.plot(df = df, title = "Query scenario, init phase", facet = "Case", plot.format = plot.format)

df = tp[tp$Scenario == "Query" & tp$Phase == "Check", ]
benchmark.plot(df = df, title = "Query scenario, check phase", facet = "Case", plot.format = plot.format)

## Transition scenario

plot.format = PlotFormat$new(width=110, height=110, legend_cols=2)

df = tp[tp$Scenario == "Transition" & tp$Phase == "Init", ]
benchmark.plot(df = df, title = "Transition scenario, init phase", facet = "Case", plot.format = plot.format)

df = tp[tp$Scenario == "Transition" & tp$Phase == "Transformation", ]
benchmark.plot(df = df, title = "Transition scenario, transformation phase", facet = "Case", plot.format = plot.format)


matches = subset(results, Scenario == "Query" & Metric == "Matches" & Tool == "Original" & Run == 1)
matches = subset(matches, select = c(Case, Artifact, Value))
matches = arrange(matches,Case,Artifact)

matches.wide = dcast(
  matches,
  Case ~ Artifact,
  value.var="Value"
)
matches.wide = rename(matches.wide, c("Case"="Query"))
write.csv(x = matches.wide, file = "matches.csv", quote = F, row.names = F)
