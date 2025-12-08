#install.packages("data.table")
#install.packages("ggplot2")
#install.packages("jsonlite")
#install.packages("lme4")
#install.packages("~/Downloads/Setups/asreml_4.2.0.355.tgz", 
#                 repos = NULL, type = "mac.binary")

library(asreml)
library(lme4)


############# Categorical variables to factors

g2f_moist_df$EnvID <- as.factor(g2f_moist_df$EnvID)
g2f_moist_df$Pedigree <- as.factor(g2f_moist_df$Pedigree)
g2f_moist_df$GrainMoisture <- as.numeric(g2f_moist_df$GrainMoisture)



###########################  Yield ########################

############### Dataframe
g2f_NE_moist_df <- g2f_moist_df %>% filter(State == "NE")
g2f_MN_moist_df <- g2f_moist_df %>% filter(State == "MN")
g2f_IA_moist_df <- g2f_moist_df %>% filter(State == "IA")


############## Model specification

model_g2f_NE_moist <- asreml(
  fixed    = GrainMoisture ~ EnvID,            
  random   = ~ Pedigree + Pedigree:EnvID,         
  residual = ~ units,               
  data     = g2f_NE_moist_df
)

model_g2f_MN_moist <- asreml(
  fixed    = GrainMoisture  ~ EnvID,            
  random   = ~ Pedigree + Pedigree:EnvID,         
  residual = ~ units,               
  data     = g2f_MN_moist_df
)

model_g2f_IA_moist <- asreml(
  fixed    = GrainMoisture  ~ EnvID,            
  random   = ~ Pedigree + Pedigree:EnvID,         
  residual = ~ units,               
  data     = g2f_IA_moist_df
)


############## model summary
summ_g2f_NE_moist_model <- summary(model_g2f_NE_moist, coef = TRUE)
summ_g2f_MN_moist_model <- summary(model_g2f_MN_moist, coef = TRUE)
summ_g2f_IA_moist_model <- summary(model_g2f_IA_moist, coef = TRUE)


############### Fixed effects
summ_g2f_NE_moist_model$coef.fixed
summ_g2f_MN_moist_model$coef.fixed
summ_g2f_IA_moist_model$coef.fixed


################ Random Effects
summ_g2f_NE_moist_model$coef.random
summ_g2f_MN_moist_model$coef.random
summ_g2f_IA_moist_model$coef.random


################ Variance components
summ_g2f_NE_moist_model$varcomp
summ_g2f_MN_yld_model$varcomp
summ_g2f_IA_yld_model$varcomp


################ Extracting variance components
var_g <- summ_g2f_NE_moist_model$varcomp$component[1]
var_gxe <- summ_g2f_NE_moist_model$varcomp$component[2]
var_r <- summ_g2f_NE_moist_model$varcomp$component[3]
R_NE_moist <- var_g / (var_g + var_gxe + var_r)


var_g <- summ_g2f_MN_moist_model$varcomp$component[1]
var_gxe <- summ_g2f_MN_moist_model$varcomp$component[2]
var_r <- summ_g2f_MN_moist_model$varcomp$component[3]
R_MN_moist <- var_g / (var_g + var_gxe + var_r)


var_g <- summ_g2f_IA_moist_model$varcomp$component[1]
var_gxe <- summ_g2f_IA_moist_model$varcomp$component[2]
var_r <- summ_g2f_IA_moist_model$varcomp$component[3]
R_IA_moist <- var_g / (var_g + var_gxe + var_r)



################ BLUPs for random effects
################ NE
rn_moist <- rownames(coef(model_g2f_NE_moist)$random)
Pedigree <- sapply(strsplit(rn_moist, "_"), `[`, 2)


effect_sizes <- model_g2f_NE_moist$coefficients$random
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_moist <- effect_sizes_df$effect


blups_g2f_NE_moist <- data.frame(
  "Pedigree" = Pedigree,
  "NE_Moist" = effect_sizes_moist
)



################ MN
rn_moist <- rownames(coef(model_g2f_MN_moist)$random)
Pedigree <- sapply(strsplit(rn_moist, "_"), `[`, 2)


effect_sizes <- model_g2f_MN_moist$coefficients$random
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_moist <- effect_sizes_df$effect


blups_g2f_MN_moist <- data.frame(
  "Pedigree" = Pedigree,
  "MN_Moist" = effect_sizes_moist
)



################ IA
rn_moist <- rownames(coef(model_g2f_IA_moist)$random)
Pedigree <- sapply(strsplit(rn_moist, "_"), `[`, 2)


effect_sizes <- model_g2f_IA_moist$coefficients$random
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_moist <- effect_sizes_df$effect


blups_g2f_IA_moist <- data.frame(
  "Pedigree" = Pedigree,
  "IA_Moist" = effect_sizes_moist
)




