library(purrr)
library(rlang) # for .data # includes purrr, dplyr, ggplot2, stringr, forcats
library(dplyr)
library(ggplot2)
library(forcats)   # for fct_reorder, optional but handy
library(stringr)
library(viridis)
library(RColorBrewer)


# Create 18 interpolated "Reds" colors
#pal_18 <- colorRampPalette(brewer.pal(9, "Reds"))(18)
pal_20 <- colorRampPalette(brewer.pal(9, "RdPu"))(20)

# Drop the first two very light tints
my_pal <- pal_20[6:20]   # gives 11 good strong colors




# named list of trait data frames
g2f_blups_list <- list(
  NE_Yield   = blups_g2f_NE_yld,
  MN_Yield   = blups_g2f_MN_yld,
  IA_Yield   = blups_g2f_IA_yld,
  NE_Anth    = blups_g2f_NE_anth,
  MN_Anth    = blups_g2f_MN_anth,
  IA_Anth    = blups_g2f_IA_anth,
  NE_Silk    = blups_g2f_NE_silk,
  MN_Silk    = blups_g2f_NE_silk,
  IA_Silk    = blups_g2f_IA_silk,
  NE_Pltht   = blups_g2f_NE_pltht,
  MN_Pltht   = blups_g2f_NE_pltht,
  IA_Pltht   = blups_g2f_IA_pltht,
  NE_Earht   = blups_g2f_NE_earht,
  MN_Earht   = blups_g2f_NE_earht,
  IA_Earht   = blups_g2f_IA_earht,
  NE_Moist   = blups_g2f_NE_moist,
  MN_Moist   = blups_g2f_NE_moist,
  IA_Moist   = blups_g2f_IA_moist,
  NE_Stlk    = blups_g2f_NE_stlk,
  MN_Stlk    = blups_g2f_NE_stlk,
  IA_Stlk    = blups_g2f_IA_silk
)




g2f_blups_list$NE_Yield %>%
    filter(Pedigree != "W10004") %>%
    slice_max(order_by = NE_Yield, n = 15, with_ties = FALSE) %>%  
    mutate(Pedigree = fct_reorder(Pedigree, NE_Yield)) %>%
        ggplot(aes(x = Pedigree, y = NE_Yield, fill = Pedigree)) +
              coord_flip() +
              geom_col() +
              scale_x_discrete(labels = function(lbls) {
                lbls %>%
                  str_replace_all(" ", "\u00A0") %>%
                  str_replace_all("\\+", "+\n") %>%
                  str_wrap(width = 30)
              }) +
              theme_classic() +
              scale_fill_manual(values = my_pal) +
              labs(title = "NE Yield", x = "Hybrids", y = "BLUPs", fill = "Hybrids") +
              theme(
                axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 12),
                axis.text.y = element_text(size = 12),
                axis.title.x = element_text(size = 18),
                legend.position = "top",
                legend.direction = "horizontal"
              )
  

g2f_blups_list$MN_Yield %>%
  filter(Pedigree != "W10004") %>%
  slice_max(order_by = MN_Yield, n = 15, with_ties = FALSE) %>%  
  mutate(Pedigree = fct_reorder(Pedigree, MN_Yield)) %>%
  ggplot(aes(x = Pedigree, y = MN_Yield, fill = Pedigree)) +
  coord_flip() +
  geom_col() +
  scale_x_discrete(labels = function(lbls) {
    lbls %>%
      str_replace_all(" ", "\u00A0") %>%
      str_replace_all("\\+", "+\n") %>%
      str_wrap(width = 30)
  }) +
  theme_classic() +
  scale_fill_manual(values = my_pal) +
  labs(title = "MN Yield", x = "Hybrids", y = "BLUPs", fill = "Hybrids") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 18),
    legend.position = "top",
    legend.direction = "horizontal"
  )


g2f_blups_list$IA_Yield %>%
  filter(Pedigree != "W10004") %>%
  slice_max(order_by = IA_Yield, n = 15, with_ties = FALSE) %>%  
  mutate(Pedigree = fct_reorder(Pedigree, IA_Yield)) %>%
  ggplot(aes(x = Pedigree, y = IA_Yield, fill = Pedigree)) +
  coord_flip() +
  geom_col() +
  scale_x_discrete(labels = function(lbls) {
    lbls %>%
      str_replace_all(" ", "\u00A0") %>%
      str_replace_all("\\+", "+\n") %>%
      str_wrap(width = 30)
  }) +
  theme_classic() +
  scale_fill_manual(values = my_pal) +
  labs(title = "IA Yield", x = "Hybrids", y = "BLUPs", fill = "Hybrids") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 18),
    legend.position = "top",
    legend.direction = "horizontal"
  )




# print all plots in the viewer
walk(plots, print)

# optionally save them

imap(plots, ~ ggsave(filename = paste0("top13_", .y, ".png"), plot = .x, dpi = 300))