library(dplyr)
library(ggplot2)
library(forcats)   # for fct_reorder, optional but handy
library(stringr)
library(viridis)

#while (!is.null(dev.list())) dev.off()

blups_yld$HybID <- as.factor(blups_yld$HybID)
group3$HybID <- as.factor(group3$HybID)
group3$HybType <- as.factor(group3$HybType)
  
blups_yld$HybType <- group3$HybType[match(blups_yld$HybID, group3$HybID)]


blups_list <- list(blups_yld, blups_mst, blups_twt, blups_pltht, blups_earht)


yld_plt_data <- blups_yld %>%
                    mutate(
                      HybPed = group3$HybPed[match(HybID, group3$HybID)],
                      HybType = group3$HybType[match(HybID, group3$HybID)],
                      HybClass = ifelse(HybType == 1, "Experimental Hybrid", "Commercial Check")
                    ) %>%
                    filter(!is.na(HybPed)) %>%
                    select(HybPed, HybClass, HybID, YLD) %>%
                    arrange(desc(YLD)) %>%
                    slice_head(n = 13) %>%
                    mutate(HybPed = fct_reorder(HybPed, YLD))


yld_plt_data %>%  ggplot(aes(x = HybPed, y = YLD, fill = HybClass)) +
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
                      #scale_fill_brewer(palette = "Reds", direction = 1) +
                      labs(title = "Yield", x = "Hybrids", y = "BLUPs", fill = "Hybrids") +
                      theme(
                        axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 12),
                        axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 12),
                        axis.title.x = element_text(size = 18),
                        legend.position = "top",
                        legend.direction = "horizontal"
                      )









######
blups_yld %>%
      mutate(HybPed = group3$HybPed[match(blups_yld$HybID, group3$HybID)]) %>%
      mutate(HybType <- group3$HybType[match(blups_yld$HybID, group3$HybID)]) %>%
      mutate(HybClass = ifelse(HybType == 1, "Experimental Hybrid", "Commercial Check")) %>%
      filter(!is.na(HybPed)) %>%
      select(HybPed, HybClass, HybID, YLD) %>%
      slice_max(order_by = YLD, n = 13, with_ties = FALSE) %>%  
      mutate(HybPed = fct_reorder(HybPed, YLD)) %>%
            ggplot(aes(x = HybPed, y = YLD, fill = HybClass)) +
                  coord_flip() +
                  geom_col() +
                  scale_x_discrete(labels = function(lbls) {
                    lbls %>%
                      str_replace_all(" ", "\u00A0") %>%
                      str_replace_all("\\+", "+\n") %>%
                      str_wrap(width = 30)
                  }) +
                  theme_classic() +
                  scale_color_gradientn(colors = c("#3a528b", "#dd1c77")) +
                  labs(title = NULL, x = "Hyrids", y = "Blups", fill = "Hybrid type  ") +
                  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5,
                                       size = 12),
                        axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5,
                                                   size = 12),
                        axis.title.x = element_text(size = 18),
                        legend.position = "top",
                        legend.direction = "horizontal")



