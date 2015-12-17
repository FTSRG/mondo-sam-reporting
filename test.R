# Sample script for testing the benchmark results.
# 
# The script extracts the **Match** values from the data set.
# The goal is to check the following properties:
# * consistency -- if a tool always returns the same result between different runs
# * correctness -- if a tool returns the same result as the other tools


library(reshape2)
library(plyr)

results = read.csv("results.csv")

matches = subset(results, Metric == "Matches")

# Consistency
# For all
# - **scenarios**,
# - **artifacts**,
# - **cases**,
# - **phases**,
# a **tool** should return the same **value** between **runs**.

df = matches
head(df)
unique.results = ddply(
  .data = df,
  .variables = c("Tool", "Scenario", "Artifact", "Phase", "Case", "Iteration"),
  summarize,
  CountUniqueResults = length(unique(Value)),
  .progress = "text"
)
unique.results
inconsistent.results = subset(unique.results, unique.results$CountUniqueResults != 1)

print("In the following benchmark instances were inconsistent between runs")
print(inconsistent.results)
