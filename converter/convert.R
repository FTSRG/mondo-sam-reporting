library(plyr)

results = read.csv("converter/results.csv")

results
results = subset(results, select=-c(Sequence))
results = rename(results, c("CaseName"="Case", "RunIndex"="Run", "Size"="Artifact", "PhaseName"="Phase", "MetricName"="Metric", "MetricValue"="Value"))
results = results[c("Scenario","Tool","Run","Case","Artifact","Phase","Iteration","Metric","Value")]

write.csv(results, file = "converter/results-converted.csv", quote = FALSE, row.names = FALSE)
