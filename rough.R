library(tidyverse)
rar_df <- read.csv("./q2_raw/observed_features.csv")

rar_df_long <- rar_df %>% 
  pivot_longer(
    cols = starts_with('depth'),
    names_to = "depth_iter",
    values_to = "Obs_features"
  ) %>% 
  separate_wider_delim(
    cols = depth_iter,
    delim = "_",
    names = c("depth", "iteration")
  ) %>% 
  mutate(
    depth = as.numeric(str_replace(depth, "depth.", "")),
    iteration = as.numeric(str_replace(iteration, "iter.", ""))
  ) %>% 
  drop_na(Obs_features)

rar_summary <- rar_df_long %>% 
  group_by(sample.id, depth, SampleName) %>% 
  summarise(mean_features = mean(Obs_features), .groups = "drop")

group_summary <- rar_summary %>% 
  group_by(depth, SampleName) %>% 
  summarise(
    avg_features = mean(mean_features),
    sd_features = sd(mean_features),
    .groups = "drop"
  )


plot_lines <- ggplot(group_summary, aes(x = depth, y = avg_features, color = SampleName)) +
  geom_line(linewidth = 1) +
  geom_ribbon(
    aes(ymin = avg_features - sd_features, ymax = avg_features + sd_features, fill = SampleName),
    alpha = 0.2, color = NA
  ) +
  labs(
    title = "Alpha Rarefaction Curve",
    subtitle = "Observed Features across sequencing depths",
    x = "Sequencing Depth",
    y = "Observed Features",
    color = "Sample",
    fill = "Sample"
  ) +
  theme_bw()

plot_lines

plot_box <- ggplot(rar_summary, aes(x = as.factor(depth), y = mean_features, fill = env_local_scale)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 1.5, alpha = 0.7) +
  labs(
    title = "Alpha Rarefaction (Distribution per Depth)",
    x = "Sequencing Depth",
    y = "Observed Features",
    fill = "Sample"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotates x-axis labels
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom"
  )

plot_box