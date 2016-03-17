# calculate the memory consumption of the executed Java application

library(reshape2)
library(plyr)
library(ggplot2)
library(ggrepel)
library(xtable)

results = read.csv("../trainbenchmark/results/results.csv", header=FALSE)

colnames(results) = c("Scenario", "Tool", "Run", "Case", "Artifact", "Phase", "Iteration", "Metric", "Value")

# filter on the MaxMemory metric, throw away unused columns
memories = subset(results, Metric == "MaxMemory")
memories = subset(memories, select = -c(Phase, Iteration, Metric))
memories = unique(memories)

# get the minimum values for the memory consumption
minimum.memories = ddply(
  .data = memories,
  .variables = c("Scenario", "Tool", "Case", "Artifact", "Run"),
  .fun = colwise(min),
  .progress = "text"
)

# drop results from less than 5 runs
memories.runs = ddply(
  .data = minimum.memories,
  .variables = c("Scenario", "Tool", "Case", "Artifact", "Value"),
  .fun = colwise(length),
  .progress = "text"
)
memories.finished = memories.runs[memories.runs$Run == 5, ]
memories.finished = subset(memories.finished, select = -c(Run))

# extract the tool names for the plots labels
toolnames = ddply(
  .data = memories.finished,
  .variables = c("Scenario", "Tool", "Case"),
  .fun = colwise(max),
  .progress = "text"
)
toolnames

scenario = "Batch"

legend_cols = 4

df = memories.finished
p = ggplot(df) +
  labs(title = "", x = "Model size\n#Triples", y = "Memory consumption [MB]") +
  geom_line(aes(x = as.factor(Artifact), y = Value, col = Tool, group = Tool), size = 0.5) +
  geom_point(aes(x = as.factor(Artifact), y = Value, col = Tool, shape = Tool), size = 1.5) +
  geom_label_repel(data = toolnames, aes(x = as.factor(Artifact), y = Value, label = Tool,  col = Tool), size = 1.6, show.legend = F, label.padding = unit(0.12, "lines")) +
  scale_shape_manual(values = seq(0,24)) +
  #scale_x_discrete(breaks = xbreaks, labels = xlabels) +
  scale_y_log10() +#breaks = ybreaks, labels = ylabels) +
  facet_wrap(~ Case, ncol = 2, scale = "fixed") +
  theme_bw() +
  theme(legend.key = element_blank(), legend.title = element_blank(), legend.position = "bottom") +
  guides(shape = guide_legend(ncol = legend_cols))
  
print(p)


width = 210
height = 297
ggsave(file = paste("diagrams/", scenario, "-memory.pdf", sep = ""), width = width, height = height, units = "mm")

