library(purrr)
library(rlang) # for .data # includes purrr, dplyr, ggplot2, stringr, forcats
library(dplyr)
library(ggplot2)
library(forcats)   # for fct_reorder, optional but handy
library(stringr)
library(viridis)


yld_plt_data

# named list of trait dataframes
blups_list <- list(
  MST   = blups_mst,
  TWT   = blups_twt,
  PLTHT = blups_pltht,
  EARHT = blups_earht
)

plots <- imap(blups_list, ~ {
  trait_col <- .y               # "MST", "TWT", ...
  trait_df  <- .x %>% select(HybID, all_of(trait_col))
  
  # join the trait values onto the yld selection (keeps exact same HybIDs and order)
  df <- yld_plt_data %>%
    left_join(trait_df, by = "HybID") %>%
    # ensure factor levels match the yld ordering (so x-axis is identical across traits)
    mutate(HybPed = factor(HybPed, levels = levels(yld_plt_data$HybPed)))
  
  # optional message if some selected hybrids don't exist in the trait df
  missing_n <- sum(is.na(df[[trait_col]]))
  if (missing_n > 0) message(sprintf("%s: %d missing values for the selected hybrids", trait_col, missing_n))
  
  # build plot using the joined column name (works because the column name is trait_col)
  ggplot(df, aes(x = HybPed, y = .data[[trait_col]], fill = HybClass)) +
    coord_flip() +
    geom_col() +
    scale_x_discrete(labels = function(lbls) {
      lbls %>%
        str_replace_all(" ", "\u00A0") %>%
        str_replace_all("\\+", "+\n") %>%
        str_wrap(width = 30)
    }) +
    theme_classic() +
    scale_fill_brewer(palette = "RdPu", direction = 1) +
    labs(title = trait_col, x = "Hybrids", y = "BLUPs", fill = "Hybrids") +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 18),
      legend.position = "top",
      legend.direction = "horizontal"
    )
})

# print all plots in the viewer
walk(plots, print)

# optionally save them
imap(plots, ~ ggsave(filename = paste0("top13_", .y, ".png"), plot = .x, dpi = 300))
