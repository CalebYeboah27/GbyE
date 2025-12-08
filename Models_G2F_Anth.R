#install.packages("data.table")
#install.packages("ggplot2")
#install.packages("jsonlite")
#install.packages("lme4")
#install.packages("~/Downloads/Setups/asreml_4.2.0.355.tgz", 
#                 repos = NULL, type = "mac.binary")

library(asreml)
library(lme4)


############# Categorical variables to factors


g2f_anth_df$EnvID <- as.factor(g2f_anth_df$EnvID)
g2f_anth_df$Pedigree <- as.factor(g2f_anth_df$Pedigree)
g2f_anth_df$AnthesisDays <- as.numeric(g2f_anth_df$AnthesisDays)



###########################  Yield ########################

############### Dataframe
g2f_NE_anth_df <- g2f_anth_df %>% filter(State == "NE")
g2f_MN_anth_df <- g2f_anth_df %>% filter(State == "MN")
g2f_IA_anth_df <- g2f_anth_df %>% filter(State == "IA")


############## Model specification

model_g2f_NE_anth <- asreml(
  fixed    = AnthesisDays  ~ EnvID,            
  random   = ~ Pedigree + Pedigree:EnvID,         
  residual = ~ units,               
  data     = g2f_NE_anth_df
)

model_g2f_MN_anth <- asreml(
  fixed    = AnthesisDays  ~ EnvID,            
  random   = ~ Pedigree + Pedigree:EnvID,         
  residual = ~ units,               
  data     = g2f_MN_anth_df
)

model_g2f_IA_anth <- asreml(
  fixed    = AnthesisDays  ~ EnvID,            
  random   = ~ Pedigree + Pedigree:EnvID,         
  residual = ~ units,               
  data     = g2f_IA_anth_df
)


############## model summary
summ_g2f_NE_anth_model <- summary(model_g2f_NE_anth, coef = TRUE)
summ_g2f_MN_anth_model <- summary(model_g2f_MN_anth, coef = TRUE)
summ_g2f_IA_anth_model <- summary(model_g2f_IA_anth, coef = TRUE)


############### Fixed effects
summ_g2f_NE_anth_model$coef.fixed
summ_g2f_MN_anth_model$coef.fixed
summ_g2f_IA_anth_model$coef.fixed


################ Random Effects
summ_g2f_NE_anth_model$coef.random
summ_g2f_MN_anth_model$coef.random
summ_g2f_IA_anth_model$coef.random


################ Variance components
summ_g2f_NE_anth_model$varcomp
summ_g2f_MN_yld_model$varcomp
summ_g2f_IA_yld_model$varcomp


################ Extracting variance components
var_g <- summ_g2f_NE_anth_model$varcomp$component[1]
var_gxe <- summ_g2f_NE_anth_model$varcomp$component[2]
var_r <- summ_g2f_NE_anth_model$varcomp$component[3]
R_NE_anth <- var_g / (var_g + var_gxe + var_r)


var_g <- summ_g2f_MN_anth_model$varcomp$component[1]
var_gxe <- summ_g2f_MN_anth_model$varcomp$component[2]
var_r <- summ_g2f_MN_anth_model$varcomp$component[3]
R_MN_anth <- var_g / (var_g + var_gxe + var_r)


var_g <- summ_g2f_IA_anth_model$varcomp$component[1]
var_gxe <- summ_g2f_IA_anth_model$varcomp$component[2]
var_r <- summ_g2f_IA_anth_model$varcomp$component[3]
R_IA_anth <- var_g / (var_g + var_gxe + var_r)



################ BLUPs for random effects
################ NE
rn_anth <- rownames(coef(model_g2f_NE_anth)$random)
Pedigree <- sapply(strsplit(rn_anth, "_"), `[`, 2)


effect_sizes <- model_g2f_NE_anth$coefficients$random
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_anth <- effect_sizes_df$effect


blups_g2f_NE_anth <- data.frame(
  "Pedigree" = Pedigree,
  "NE_Anth" = effect_sizes_anth
)



################ MN
rn_anth <- rownames(coef(model_g2f_MN_anth)$random)
Pedigree <- sapply(strsplit(rn_anth, "_"), `[`, 2)


effect_sizes <- model_g2f_MN_anth$coefficients$random
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_anth <- effect_sizes_df$effect


blups_g2f_MN_anth <- data.frame(
  "Pedigree" = Pedigree,
  "MN_Anth" = effect_sizes_anth
)



################ IA
rn_anth <- rownames(coef(model_g2f_IA_anth)$random)
Pedigree <- sapply(strsplit(rn_anth, "_"), `[`, 2)


effect_sizes <- model_g2f_IA_anth$coefficients$random
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_anth <- effect_sizes_df$effect


blups_g2f_IA_anth <- data.frame(
  "Pedigree" = Pedigree,
  "IA_Anth" = effect_sizes_anth
)



