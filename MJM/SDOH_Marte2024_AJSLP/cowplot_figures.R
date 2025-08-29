library(cowplot)
plot_grid(
  Cit_plot,
  poverty_plot,
  WebUse_Plot,
  lany_plot,
  com_plot,
  nrow = 2,
  labels = "auto")

group <- plot_grid(
  Cit_plot +  theme(legend.position="none"),
  poverty_plot +  theme(legend.position="none"),
  WebUse_Plot +  theme(legend.position="none"),
  edu_plot + theme(legend.position = "none"),
  lany_plot +  theme(legend.position="none"),
  com_plot +  theme(legend.position="none"),
  nrow = 2,
  labels = "auto")

plot_grid(
  group,
  rel_widths = c(3, .4))

###
group2 <- plot_grid(
  lany_subsets +  theme(legend.position="none") +xlab(""),
  ses_comdiff2 +  theme(legend.position="none")+xlab(""),
  lany_webuse +  theme(legend.position="none")+xlab(""),
  webuse_comdiff2 +  theme(legend.position="none"),
  nrow = 4,
  align = "h",
  labels = "auto")

plot_grid(
  group2,
  legend,
  rel_widths = c(3, .4))

legend <- get_legend(
  # create some space to the left of the legend
  webuse_comdiff + theme(legend.box.margin = margin(0, 0, 0, 12))
)
