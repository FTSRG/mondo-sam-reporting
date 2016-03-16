# calculate the memory consumption of the executed Java application

library(reshape2)
library(plyr)
library(ggplot2)
library(ggrepel)
library(xtable)

results = read.csv("../trainbenchmark/results/results.csv", header=FALSE)
colnames(results) = c("Scenario", "Tool", "Run", "Case", "Artifact", "Phase", "Iteration", "Metric", "Value")

# filter on the MaxMemory metric, throw away unused columns
memories = subset(results, Tool != "EMF_API")
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
  .fun = colwise(length),
  .progress = "text"
)

head(minimum.memories.count)


#mm = minimum.memories.count[minimum.memories.count$Case == "RouteSensor-ConnectedSegments-PosLength-SemaphoreNeighbor-SwitchSensor-SwitchSet" & minimum.memories.count$Run == 15, ]

scenario = "Batch"
#scenario = "Inject"
#scenario = "Repair"

mmx = minimum.memories.count[minimum.memories.count$Case == "SwitchSet", ]
mmx

mm = minimum.memories.count
mm = mm[mm$Case != "RouteSensor-ConnectedSegments-PosLength-SemaphoreNeighbor-SwitchSensor-SwitchSet" & mm$Run == 5, ]
mm = mm[mm$Tool != "MySQL" & mm$Tool != "Virtuoso_(No_Inferencing)" & mm$Tool != "Blazegraph_(No_Inferencing)", ]
mm = mm[mm$Scenario == scenario, ]

mm = subset(mm, select = -c(Run))

df = mm

toolnames = df
toolnames = subset(toolnames, select = -c(Value))

toolnames = ddply(
  .data = df,
  .variables = c("Scenario", "Tool", "Case"),
  .fun = colwise(max),
  .progress = "text"
)

head(df)
head(toolnames)

legend_cols = 4

p = ggplot(df) +
  labs(title = "", x = "Model size\n#Triples", y = "Memory consumption [MB]") +
  geom_point(aes(x = as.factor(Artifact), y = Value, col = Tool, shape = Tool), size = 1.5) +
  geom_line(aes(x = as.factor(Artifact), y = Value, col = Tool, group = Tool), size = 0.5) +
  scale_shape_manual(values = seq(0,24)) +
  #scale_x_discrete(breaks = xbreaks, labels = xlabels) +
  scale_y_log10() +#breaks = ybreaks, labels = ylabels) +
  facet_wrap(~ Case, ncol = 2, scale = "fixed") +
  theme_bw() +
  theme(legend.key = element_blank(), legend.title = element_blank(), legend.position = "bottom") +
  geom_label_repel(data = toolnames, aes(x = as.factor(Artifact), y = Value, label = Tool,  col = Tool), size = 1.6) +
  guides(shape = guide_legend(ncol = legend_cols))
print(p)


width = 210
height = 297
ggsave(file = paste("diagrams/", scenario, "-memory.pdf", sep = ""), width = width, height = height, units = "mm")

