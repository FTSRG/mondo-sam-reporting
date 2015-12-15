library("ggplot2")

ys = -10:10
ybreaks = 10^ys
ylabels = parse(text = paste("10^", ys, sep = ""))

df = read.csv("ls2.csv")
#df$Value = df$Value / 10^9
df[df == 0] = NA

levels=c("model load", "engine creation", "search plan calculation", "check")
df$Phase = factor(df$Phase, levels = levels)

ggplot(data=df, aes(x=Tool, y=Value, fill=Phase,order=order)) +
    geom_bar(stat="identity", position="dodge") +
    facet_wrap(~ Case, ncol=4) +
    theme_bw() +
    theme(legend.key = element_blank(), legend.title = element_blank(), legend.position = "bottom", axis.text.x = element_text(angle = 75, hjust = 1)) +
    labs(title = "Total execution time on model frinika", x = "algorithm", y = "execution time [ns]") +
    scale_y_log10(breaks = ybreaks, labels = ylabels, limits = c(1e+0,1e+10)) +
    scale_fill_brewer(palette = "Spectral")
ggsave("diagrams/frinika.pdf", width=210, height=297, units="mm")
