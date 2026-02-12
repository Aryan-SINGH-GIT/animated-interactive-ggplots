library(animint2)
library(palmerpenguins)
library(tidyr)
data(penguins)

# 1. Scatter plot 
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
    limits = c(170, 235),
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
  theme_bw(base_size = 14) + 
  theme(
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

# 2. Prepare data for histograms
penguins_long <- penguins |>
  drop_na(flipper_length_mm, body_mass_g) |>
  pivot_longer(
    cols = c(flipper_length_mm, body_mass_g),
    names_to = "variable",
    values_to = "value"
  )

# Combined histogram 
hist_combined <- ggplot(
  penguins_long,
  aes(
    x = value,
    fill = species
  )
) +
  geom_histogram(
    bins = 25,
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
    x = "Measurement Value",
    y = "Count",
    fill = "Species"
  ) +
  theme_bw(base_size = 12) + 
  theme(
    # 1. Make 'body_mass_g' and 'flipper_length_mm' headers larger
    strip.text = element_text(face = "bold", size = 15, color = "black"),
    
    # 2. Make 'Species' section title larger
    legend.title = element_text(face = "bold", size = 15),
    legend.text = element_text(size = 12),
    
    # 3. Maintain readability for x-axis numbers
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title = element_text(face = "bold"),
    
    # 4. Use panel.margin to avoid the error and add space
    panel.margin = unit(1.5, "lines")
  )
# 4. Combine into animint
viz <- animint(
  title   = "Penguin Sizes by Species",
  scatter = scatter_plot,
  hist    = hist_combined,
  source  = "https://github.com/Aryan-SINGH-GIT/animint2-pages-easy-Test"
)

# Export
animint2pages(
  viz,
  out.dir = "penguins_animint",
  github_repo = "animint2-pages-easy-Test"
)