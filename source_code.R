library(ggplot2)
library(plyr)
library(gdata)

df = read.csv("ls_codemodel.csv")
df$Value = df$Value / 10^9

#df[df == 0] = NA

df = arrange(df, Artifact, Case, Phase)

levels = c("Model load", "Engine creation", "Search plan calculation", "Check")
df$Phase = reorder.factor(df$Phase, new.order=levels)
df = arrange(df, Phase)

ggplot(data=df, aes(x=Tool, y=Value, fill=Phase)) +
    geom_bar(stat="identity") +
    #geom_bar(stat="identity", position="dodge") + scale_y_log10() +
    facet_grid(Artifact ~ Case, scales="free_y") +
    theme_bw() +
    theme(legend.key=element_blank(), legend.title=element_blank(), legend.position="bottom", axis.text.x=element_text(angle=45, hjust=1)) +
    labs(title="Total execution time on source code models", x="Algorithm", y="Execution time [s]") +
    scale_fill_brewer(palette="Spectral")
ggsave("diagrams/code_model.pdf", width=210, height=297, units="mm")
