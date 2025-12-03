install.packages("data.table")
install.packages("ggplot2")
install.packages("jsonlite")
install.packages("lme4")
install.packages("~/Downloads/Setups/asreml_4.2.0.355.tgz", 
                 repos = NULL, type = "mac.binary")

library(asreml)
library(lme4)


############# Categorical variables to factors

g2f_silk_df$EnvID <- as.factor(g2f_silk_df$EnvID)
g2f_silk_df$Pedigree <- as.factor(g2f_silk_df$Pedigree)
g2f_silk_df$PlantHeight <- as.numeric(g2f_silk_df$PlantHeight)



###########################  Yield ########################

############### Dataframe
g2f_NE_silk_df <- g2f_silk_df %>% filter(State == "NE")
g2f_MN_silk_df <- g2f_silk_df %>% filter(State == "MN")
g2f_IA_silk_df <- g2f_silk_df %>% filter(State == "IA")


############## Model specification

model_g2f_NE_silk <- asreml(
  fixed    = PlantHeight ~ EnvID,            
  random   = ~ Pedigree + Pedigree:EnvID,         
  residual = ~ units,               
  data     = g2f_NE_silk_df
)

model_g2f_MN_silk <- asreml(
  fixed    = PlantHeight  ~ EnvID,            
  random   = ~ Pedigree + Pedigree:EnvID,         
  residual = ~ units,               
  data     = g2f_MN_silk_df
)

model_g2f_IA_silk <- asreml(
  fixed    = PlantHeight  ~ EnvID,            
  random   = ~ Pedigree + Pedigree:EnvID,         
  residual = ~ units,               
  data     = g2f_IA_silk_df
)


############## model summary
summ_g2f_NE_silk_model <- summary(model_g2f_NE_silk, coef = TRUE)
summ_g2f_MN_silk_model <- summary(model_g2f_MN_silk, coef = TRUE)
summ_g2f_IA_silk_model <- summary(model_g2f_IA_silk, coef = TRUE)


############### Fixed effects
summ_g2f_NE_silk_model$coef.fixed
summ_g2f_MN_silk_model$coef.fixed
summ_g2f_IA_silk_model$coef.fixed


################ Random Effects
summ_g2f_NE_silk_model$coef.random
summ_g2f_MN_silk_model$coef.random
summ_g2f_IA_silk_model$coef.random


################ Variance components
summ_g2f_NE_silk_model$varcomp
summ_g2f_MN_yld_model$varcomp
summ_g2f_IA_yld_model$varcomp


################ Extracting variance components
var_g <- summ_g2f_NE_silk_model$varcomp$component[1]
var_gxe <- summ_g2f_NE_silk_model$varcomp$component[2]
var_r <- summ_g2f_NE_silk_model$varcomp$component[3]
R_NE_silk <- var_g / (var_g + var_gxe + var_r)


var_g <- summ_g2f_MN_silk_model$varcomp$component[1]
var_gxe <- summ_g2f_MN_silk_model$varcomp$component[2]
var_r <- summ_g2f_MN_silk_model$varcomp$component[3]
R_MN_silk <- var_g / (var_g + var_gxe + var_r)


var_g <- summ_g2f_IA_silk_model$varcomp$component[1]
var_gxe <- summ_g2f_IA_silk_model$varcomp$component[2]
var_r <- summ_g2f_IA_silk_model$varcomp$component[3]
R_IA_silk <- var_g / (var_g + var_gxe + var_r)



################ BLUPs for random effects
################ NE
rn_silk <- rownames(coef(model_g2f_NE_silk)$random)
Pedigree <- sapply(strsplit(rn_silk, "_"), `[`, 2)


effect_sizes <- model_g2f_NE_silk$coefficients$random
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_silk <- effect_sizes_df$effect


blups_g2f_NE_silk <- data.frame(
  "Pedigree" = Pedigree,
  "Silk" = effect_sizes_silk
)



################ MN
rn_silk <- rownames(coef(model_g2f_MN_silk)$random)
Pedigree <- sapply(strsplit(rn_silk, "_"), `[`, 2)


effect_sizes <- model_g2f_MN_silk$coefficients$random
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_silk <- effect_sizes_df$effect


blups_g2f_MN_silk <- data.frame(
  "Pedigree" = Pedigree,
  "Silk" = effect_sizes_silk
)



################ IA
rn_silk <- rownames(coef(model_g2f_IA_silk)$random)
Pedigree <- sapply(strsplit(rn_silk, "_"), `[`, 2)


effect_sizes <- model_g2f_IA_silk$coefficients$random
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_silk <- effect_sizes_df$effect


blups_g2f_IA_silk <- data.frame(
  "Pedigree" = Pedigree,
  "Silk" = effect_sizes_silk
)



