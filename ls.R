df = read.csv("ls2.csv")
df$Value = df$Value / 10^9
ggplot(data=df, aes(x=Tool, y=Value, fill=Phase)) +
    geom_bar(stat="identity") +
    facet_wrap(~ Case, ncol=4) +
    theme_bw() +
    theme(legend.key = element_blank(), legend.title = element_blank(), legend.position = "bottom", axis.text.x = element_text(angle = 75, hjust = 1)) +
    labs(title = "Total execution time on model frinika", x = "algorithm", y = "execution time [s]")
ggsave("diagrams/frinika.pdf", width=210, height=297, units="mm")
