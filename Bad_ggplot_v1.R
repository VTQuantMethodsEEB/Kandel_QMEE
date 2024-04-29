library(ggplot2)
# sample data for plot ----
points <-
  data.frame(
    Soil_temp = rep(1:10,2),
    Soil_resp = rep(1:10,2),
    z = sort(rep(letters[1:2], 10)),
    w = rep(letters[3:4], 10)
  )
# ggplot using many theme options ----
ggplot(data = points,
       mapping = aes(x = Soil_temp, y = Soil_resp, col = factor(Soil_temp))) +
  geom_point(size = 5) +
  geom_line(aes(x = Soil_temp, y = Soil_resp, group = w), linewidth = 3) +
  facet_grid(w ~ z, switch = "y") +
  theme(

    plot.background = element_rect(fill = "lightpink"),
    plot.title = element_text(size = 30, hjust = 0.25),
    plot.subtitle = element_text(size = 20, hjust = 0.45, color = "mediumvioletred", family = "serif"),
    plot.caption = element_text(size = 13, face = "italic", angle = 35),

    panel.background = element_rect(fill = 'yellow', colour = 'darkred', size = 4),
    panel.border = element_rect(fill = NA, color = "green", size = 2),
    panel.grid.major.x = element_line(color = "purple", linetype = 2),
    panel.grid.minor.x = element_line(color = "orange", linetype = 3),
    panel.grid.minor.y = element_blank(),

    axis.title.x = element_text(face = "bold.italic", color = "blue", hjust = 0.5, vjust = 8),
    axis.title.y = element_text(family = "mono", face = "bold", size = 20, hjust = 0.25),
    axis.text = element_text(face = "italic", size = 25, angle = -35),
    axis.text.x.bottom = element_text(angle = -35), # note that axis.text options from above are inherited

    strip.background = element_rect(fill = "purple"),
    strip.text.y = element_text(color = "white"),
    strip.placement = "outside",

    legend.background = element_rect(fill = "red"), # generally will want to match w plot background
    legend.key = element_rect(fill = "orange"),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.justification = "left",
    legend.title = element_text(family = "serif", color = "white", size = 15),
    legend.text = element_text(family = "mono", face = "italic", color = "limegreen")

  ) +
  labs(title = "TITLE: Q10 MODEL PLOT",
       subtitle = "Subtitle: temperature affecting respiration",
       x = "my x axis and unit = degree celcius",
       y = "my y axis and unit = umol/m2/day",
       caption = "by bibek kandel",
       col = "Legend at bottom")

