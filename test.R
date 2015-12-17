# Sample script for testing the benchmark results.
# 
# The script extracts the **Match** values from the data set.
# The goal is to check the following properties:
# * consistency -- if a tool always returns the same result between different runs
# * correctness -- if a tool returns the same result as the other tools


library(reshape2)
library(plyr)

results = read.csv("results.csv")

matches = subset(results, Metric == "Matches" & Phase == "Check")

tools = unique(matches$Tool)
artifacts = unique(matches$Artifact)
scenarios = unique(matches$Scenario)
cases = unique(matches$Case)
phases = unique(matches$Phase)

# Consistency
# For all
# - **scenarios**,
# - **artifacts**,
# - **cases**,
# - **phases**,
# a **tool** should return the same **value** between **runs**.

df = matches
for(tool in tools) {
  for(scenario in scenarios) {
    for(artifact in artifacts) {
      for(phase in phases) {
        for(case in cases) {
          sdf = df[df$Scenario == scenario & df$Artifact == artifact & df$Phase == phase & df$Case == case & df$Tool == tool, ]
          if (length(unique(sdf$Value)) > 1){
            print(paste("Results for ",
              "tool ", tool, ", ",
              "artifact ", artifact, ", ",
              "phase ", phase, ", ",
              "case ", case, " ",
              "are not consistent.", sep = ''))
          }
        }
      }
    }
  }
}
