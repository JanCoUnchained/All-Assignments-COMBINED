library(tidyverse)
library(ggthemes)

data <- read.csv("language_dev_asd_clean_2.csv")


# SPLIT VIIOLIN FUNCITON
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}


## THE PLOT
ggplot(data, aes(as.numeric(VISIT), CHI_MLU)) +
  geom_split_violin(alpha = 0.6, 
                    bw = "nrd0",
                    width = 1.2,
                    scale = "count",
                    aes(fill = Diagnosis, 
                        group = interaction(Diagnosis, VISIT))) +
  geom_point(aes(color = Diagnosis), 
             alpha = 0.45, 
             position = position_jitterdodge()) +
  stat_smooth(aes(color = Diagnosis, fill = Diagnosis), 
              method = "lm", 
              formula = y ~ poly(x, 2),
              alpha = 0.6) +
  theme_bw() +
  scale_fill_tableau() +
  scale_color_tableau() +
  labs(title = "fuck this was hard",
       subtitle = "Riccardo probably cheated using python", 
       x = "Visit number", 
       y = "Child's MLU")


