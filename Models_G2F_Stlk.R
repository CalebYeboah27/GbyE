#install.packages("data.table")
#install.packages("ggplot2")
#install.packages("jsonlite")
#install.packages("lme4")
#install.packages("~/Downloads/Setups/asreml_4.2.0.355.tgz", 
#                 repos = NULL, type = "mac.binary")

library(asreml)
library(lme4)


############# Categorical variables to factors

g2f_stlk_df$EnvID <- as.factor(g2f_stlk_df$EnvID)
g2f_stlk_df$Pedigree <- as.factor(g2f_stlk_df$Pedigree)
g2f_stlk_df$StalkLodging <- as.numeric(g2f_stlk_df$StalkLodging)



###########################  Yield ########################

############### Dataframe
g2f_NE_stlk_df <- g2f_stlk_df %>% filter(State == "NE")
g2f_MN_stlk_df <- g2f_stlk_df %>% filter(State == "MN")
g2f_IA_stlk_df <- g2f_stlk_df %>% filter(State == "IA")


############## Model specification

model_g2f_NE_stlk <- asreml(
  fixed    = StalkLodging ~ EnvID,            
  random   = ~ Pedigree + Pedigree:EnvID,         
  residual = ~ units,               
  data     = g2f_NE_stlk_df
)

model_g2f_MN_stlk <- asreml(
  fixed    = StalkLodging  ~ EnvID,            
  random   = ~ Pedigree + Pedigree:EnvID,         
  residual = ~ units,               
  data     = g2f_MN_stlk_df
)

model_g2f_IA_stlk <- asreml(
  fixed    = StalkLodging  ~ EnvID,            
  random   = ~ Pedigree + Pedigree:EnvID,         
  residual = ~ units,               
  data     = g2f_IA_stlk_df
)


############## model summary
summ_g2f_NE_stlk_model <- summary(model_g2f_NE_stlk, coef = TRUE)
summ_g2f_MN_stlk_model <- summary(model_g2f_MN_stlk, coef = TRUE)
summ_g2f_IA_stlk_model <- summary(model_g2f_IA_stlk, coef = TRUE)


############### Fixed effects
summ_g2f_NE_stlk_model$coef.fixed
summ_g2f_MN_stlk_model$coef.fixed
summ_g2f_IA_stlk_model$coef.fixed


################ Random Effects
summ_g2f_NE_stlk_model$coef.random
summ_g2f_MN_stlk_model$coef.random
summ_g2f_IA_stlk_model$coef.random


################ Variance components
summ_g2f_NE_stlk_model$varcomp
summ_g2f_MN_yld_model$varcomp
summ_g2f_IA_yld_model$varcomp


################ Extracting variance components
var_g <- summ_g2f_NE_stlk_model$varcomp$component[1]
var_gxe <- summ_g2f_NE_stlk_model$varcomp$component[2]
var_r <- summ_g2f_NE_stlk_model$varcomp$component[3]
R_NE_stlk <- var_g / (var_g + var_gxe + var_r)


var_g <- summ_g2f_MN_stlk_model$varcomp$component[1]
var_gxe <- summ_g2f_MN_stlk_model$varcomp$component[2]
var_r <- summ_g2f_MN_stlk_model$varcomp$component[3]
R_MN_stlk <- var_g / (var_g + var_gxe + var_r)


var_g <- summ_g2f_IA_stlk_model$varcomp$component[1]
var_gxe <- summ_g2f_IA_stlk_model$varcomp$component[2]
var_r <- summ_g2f_IA_stlk_model$varcomp$component[3]
R_IA_stlk <- var_g / (var_g + var_gxe + var_r)



################ BLUPs for random effects
################ NE
rn_stlk <- rownames(coef(model_g2f_NE_stlk)$random)
Pedigree <- sapply(strsplit(rn_stlk, "_"), `[`, 2)


effect_sizes <- model_g2f_NE_stlk$coefficients$random
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_stlk <- effect_sizes_df$effect


blups_g2f_NE_stlk <- data.frame(
  "Pedigree" = Pedigree,
  "Stlk" = effect_sizes_stlk
)



################ MN
rn_stlk <- rownames(coef(model_g2f_MN_stlk)$random)
Pedigree <- sapply(strsplit(rn_stlk, "_"), `[`, 2)


effect_sizes <- model_g2f_MN_stlk$coefficients$random
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_stlk <- effect_sizes_df$effect


blups_g2f_MN_stlk <- data.frame(
  "Pedigree" = Pedigree,
  "Stlk" = effect_sizes_stlk
)



################ IA
rn_stlk <- rownames(coef(model_g2f_IA_stlk)$random)
Pedigree <- sapply(strsplit(rn_stlk, "_"), `[`, 2)


effect_sizes <- model_g2f_IA_stlk$coefficients$random
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_stlk <- effect_sizes_df$effect


blups_g2f_IA_stlk <- data.frame(
  "Pedigree" = Pedigree,
  "Stlk" = effect_sizes_stlk
)




