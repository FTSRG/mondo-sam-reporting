library("ggplot2")
library(grid)
df = read.csv("ls2.csv")
df$Value = df$Value / 10^9
ggplot(data=df, aes(x=Tool, y=Value, fill=Phase)) +
  geom_bar(stat="identity") +
  facet_wrap(~ Case, ncol=4) +
  theme_bw() +
  #theme(legend.key = element_blank(), legend.key.height = unit(8, "mm"),  legend.title = element_blank(), legend.position = "bottom", axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11), axis.text.y = element_text(angle = 0, hjust = 0.5, size = 12)) +
  theme(legend.key = element_blank(), legend.key.height = unit(3, "mm"),  legend.title = element_blank(), legend.position = "none", axis.text.x = element_text(angle = 35, hjust = 0.5, vjust=0.7, size = 11), axis.text.y = element_text(angle = 0, hjust = 0.5,size = 12)) +
  theme(strip.text.x = element_text(size=12)) +
  theme(legend.text=element_text(size=11)) +
  labs(title = "Total execution time on model 'Frinika'", x = "algorithm", y = "execution time [s]") +
  ggsave("diagrams/frinika_resized.pdf", width=210, height=94, units="mm")

