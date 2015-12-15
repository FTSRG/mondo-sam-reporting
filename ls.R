library("ggplot2")
library(grid)
df = read.csv("ls_hibernate.csv")
df$Value = df$Value / 10^9
ggplot(data=df, aes(x=Tool, y=Value, fill=Phase)) +
  geom_bar(stat="identity") +
  facet_wrap(~ Case, ncol=4) +
  theme_bw() +
  theme(legend.key = element_blank(), legend.key.height = unit(10, "mm"),  legend.title = element_blank(), legend.position = "bottom", axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12), axis.text.y = element_text(angle = 0, hjust = 0.5, size = 12)) +
  #theme(legend.key = element_blank(), legend.title = element_blank(), legend.position = "bottom", axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  theme(strip.text.x = element_text(size=12)) +
  theme(legend.text=element_text(size=12)) +
  labs(title = "Total execution time on model 'frinika'", x = "algorithm", y = "execution time [s]")
ggsave("diagrams/frinika.pdf", width=210, height=99, units="mm")

