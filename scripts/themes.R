# ggplot themes


## right legend
theme_m.rl <- theme(
  # panel.grid.minor = element_blank(),
  # panel.grid.major = element_blank(),
  # panel.border = element_rect(size = 1,
  #                             color = "black"),
  axis.title.x = element_text(face = "bold",
                              size = 14,
                              vjust = 1.5),
  axis.text.x = element_text(
    angle = 0,
    vjust = 1,
    hjust = 0.5,
    size = 14
  ),
  axis.title.y = element_text(
    angle = 90,
    vjust = 2.5,
    hjust = 0.5,
    face = "bold",
    size = 14
  ),
  axis.text.y = element_text(size = 14),
  strip.text.x = element_text(size = 14),
  legend.position = "right",
  legend.title = element_text(size = 12, face = "bold"),
  legend.text = element_text(size = 12),
)

## no legend
theme_m.nl <- theme(
  # panel.grid.minor = element_blank(),
  # panel.grid.major = element_blank(),
  # panel.border = element_rect(size = 1,
  #                             color = "black"),
  axis.title.x = element_text(face = "bold",
                              size = 14,
                              vjust = 1.5),
  axis.text.x = element_text(
    angle = 0,
    vjust = 1,
    hjust = 0.5,
    size = 14
  ),
  axis.title.y = element_text(
    angle = 90,
    vjust = 2.5,
    hjust = 0.5,
    face = "bold",
    size = 14
  ),
  axis.text.y = element_text(size = 14),
  strip.text.x = element_text(size = 14),
  legend.position = "none",
  legend.title = element_text(size = 12, face = "bold"),
  legend.text = element_text(size = 12),
)