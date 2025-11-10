
heritability_df <- tibble(
  Model = c("FT Genes", "Whole Genome"),
  Heritability = c(0.36, 0.67) 
)



Het_plot <- heritability_df |>
  ggplot(aes(x = Model, y = Heritability, fill = Model)) +
  geom_col(position = position_dodge(width = 0.8)) +
  coord_flip() +
  labs(x = NULL, y = "Heritability", fill = "Window Size") +
  theme_minimal(base_size = 14) +
  theme() +
  scale_fill_brewer(palette = "Reds") +
  geom_text(aes(label = round(Heritability, 2)), hjust = -0.3) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    # Keep only major grid lines in very light grey
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    # Center and bold the title
    plot.title = element_text(face = "bold", hjust = 0.5),
    # Ensure all text is in black
    legend.position = "top"
  )

Het_plot
