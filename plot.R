library(ggplot2)
library(R6)

PlotFormat <- R6Class("PlotFormat",
                      public = list(
                        width = 210,
                        height = 297,
                        facet_cols = 1,
                        legend_cols = 1,
                        scale = "fixed",
                        initialize = function(width, height, facet_cols, legend_cols, scale) {
                          if (!missing(width)) self$width = width
                          if (!missing(height)) self$height = height
                          if (!missing(facet_cols)) self$facet_cols = facet_cols
                          if (!missing(legend_cols)) self$legend_cols = legend_cols
                          if (!missing(scale)) self$scale = scale
                        }
                      )
                      # TODO add font size, axis font size, sparser axes, ...
)

# TODO add optional argument for axis labels

benchmark.plot = function(df, title, facet, plot.format) {
  # for multicolumn layouts, we omit every second label on the x axis
  #if (plot.format$facet_cols > 1) {
  #  evens = seq(2, nrow(artifacts), by=2)
  #  artifacts = artifacts[-evens, ]
  #}
  
  # y axis labels
  ys = -10:10
  ybreaks = 10^ys
  ylabels = parse(text = paste("10^", ys, sep = ""))

  plot.filename = gsub("[ ,]", "-", title)

  facet = as.formula(paste("~", facet))

  p = ggplot(df) +
    labs(title = title, x = "Model size\n", y = "Execution time [s]") +
    geom_point(aes(x = as.factor(Artifact), y = Time, col = Tool, shape = Tool), size = 1.5) +
    geom_line(aes(x = as.factor(Artifact), y = Time, col = Tool, group = Tool), size = 0.5) +
    scale_shape_manual(values = seq(0,24)) +
    #scale_x_discrete(breaks = xbreaks, labels = xlabels) +
    scale_y_log10(breaks = ybreaks, labels = ylabels) +
    facet_wrap(facet, ncol = plot.format$facet_cols, scale = plot.format$scale) +
    theme_bw() +
    theme(legend.key = element_blank(), legend.title = element_blank(), legend.position = "bottom") +
    guides(shape = guide_legend(ncol = plot.format$legend_cols))
  print(p)
  
  ggsave(file = paste("diagrams/", plot.filename, ".pdf", sep = ""), width = plot.format$width, height = plot.format$height, units = "mm")
}

