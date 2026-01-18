library(ggplot2)
library(animint2)
library(palmerpenguins)
library(tidyr)
data(penguins)

# Dummy data for text panels
text_df <- data.frame(x = 1, y = 1)

# Title panel
title_plot <- ggplot(text_df, aes(x, y)) +
  geom_text(
    label = paste(
      "Penguins: Flipper Length vs Body Mass",
      "",
      "Each point represents one penguin",
      "Histograms show variable distributions",
      sep = "\n"
    ),
    size = 5,
    lineheight = 1.2
  ) +
  xlim(0.5, 1.5) +
  ylim(0.5, 1.5) +
  theme_minimal() +
  theme(
    axis.text  = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

# Source panel (required)
source_plot <- ggplot(text_df, aes(x, y)) +
  geom_text(
    label = "Source: palmerpenguins",
    size = 6
  ) +
  xlim(0.5, 1.5) +
  ylim(0.5, 1.5) +
  theme_minimal() +
  theme(
    axis.text  = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

# Scatter plot with visible axis numbers
scatter_plot <- ggplot(
  penguins,
  aes(
    flipper_length_mm,
    body_mass_g,
    color = species
  )
) +
  geom_point(size = 2, alpha = 0.7) +
  scale_x_continuous(
    limits = c(170, 230),
    breaks = seq(170, 230, by = 10)
  ) +
  scale_y_continuous(
    limits = c(2500, 6500),
    breaks = seq(2500, 6500, by = 500)
  ) +
  labs(
    x = "Flipper length (mm)",
    y = "Body mass (g)",
    color = "Species"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(face = "bold", size = 14),
    axis.ticks = element_line(color = "black"),
    axis.line = element_line(color = "black")
  )

# Prepare data for histograms
penguins_long <- penguins |>
  pivot_longer(
    cols = c(flipper_length_mm, body_mass_g),
    names_to = "variable",
    values_to = "value"
  )

# Combined histogram with visible axis numbers
hist_combined <- ggplot(
  penguins_long,
  aes(
    x = value,
    fill = species
  )
) +
  geom_histogram(
    bins = 30,
    alpha = 0.6,
    color = "white",
    position = "identity"
  ) +
  facet_wrap(
    ~ variable,
    scales = "free_x",
    labeller = as_labeller(c(
      body_mass_g = "Body Mass (g)",
      flipper_length_mm = "Flipper Length (mm)"
    ))
  ) +
  labs(
    x = "Value",
    y = "Count",
    fill = "Species"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 12),
    axis.ticks = element_line(color = "black"),
    axis.line = element_line(color = "black")
  )

# Combine into animint
viz <- animint(
  title   = title_plot,
  source  = source_plot,
  scatter = scatter_plot,
  hist    = hist_combined
)

# Export to GitHub Pages
animint2pages(
  viz,
  out.dir = "penguins_animint",
  github_repo = "animint2-pages-easy-Test"
)