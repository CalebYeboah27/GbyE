#install.packages("data.table")
#install.packages("ggplot2")
#install.packages("jsonlite")
#install.packages("lme4")
#install.packages("~/Downloads/Setups/asreml_4.2.0.355.tgz", 
#                 repos = NULL, type = "mac.binary")

library(asreml)
library(lme4)


############# Categorical variables to factors

g2f_earht_df$EnvID <- as.factor(g2f_earht_df$EnvID)
g2f_earht_df$Pedigree <- as.factor(g2f_earht_df$Pedigree)
g2f_earht_df$EarHeight <- as.numeric(g2f_earht_df$EarHeight)



###########################  Yield ########################

############### Dataframe
g2f_NE_earht_df <- g2f_earht_df %>% filter(State == "NE")
g2f_MN_earht_df <- g2f_earht_df %>% filter(State == "MN")
g2f_IA_earht_df <- g2f_earht_df %>% filter(State == "IA")


############## Model specification

model_g2f_NE_earht <- asreml(
  fixed    = EarHeight ~ EnvID,            
  random   = ~ Pedigree + Pedigree:EnvID,         
  residual = ~ units,               
  data     = g2f_NE_earht_df
)

model_g2f_MN_earht <- asreml(
  fixed    = EarHeight  ~ EnvID,            
  random   = ~ Pedigree + Pedigree:EnvID,         
  residual = ~ units,               
  data     = g2f_MN_earht_df
)

model_g2f_IA_earht <- asreml(
  fixed    = EarHeight  ~ EnvID,            
  random   = ~ Pedigree + Pedigree:EnvID,         
  residual = ~ units,               
  data     = g2f_IA_earht_df
)


############## model summary
summ_g2f_NE_earht_model <- summary(model_g2f_NE_earht, coef = TRUE)
summ_g2f_MN_earht_model <- summary(model_g2f_MN_earht, coef = TRUE)
summ_g2f_IA_earht_model <- summary(model_g2f_IA_earht, coef = TRUE)


############### Fixed effects
summ_g2f_NE_earht_model$coef.fixed
summ_g2f_MN_earht_model$coef.fixed
summ_g2f_IA_earht_model$coef.fixed


################ Random Effects
summ_g2f_NE_earht_model$coef.random
summ_g2f_MN_earht_model$coef.random
summ_g2f_IA_earht_model$coef.random


################ Variance components
summ_g2f_NE_earht_model$varcomp
summ_g2f_MN_yld_model$varcomp
summ_g2f_IA_yld_model$varcomp


################ Extracting variance components
var_g <- summ_g2f_NE_earht_model$varcomp$component[1]
var_gxe <- summ_g2f_NE_earht_model$varcomp$component[2]
var_r <- summ_g2f_NE_earht_model$varcomp$component[3]
R_NE_earht <- var_g / (var_g + var_gxe + var_r)


var_g <- summ_g2f_MN_earht_model$varcomp$component[1]
var_gxe <- summ_g2f_MN_earht_model$varcomp$component[2]
var_r <- summ_g2f_MN_earht_model$varcomp$component[3]
R_MN_earht <- var_g / (var_g + var_gxe + var_r)


var_g <- summ_g2f_IA_earht_model$varcomp$component[1]
var_gxe <- summ_g2f_IA_earht_model$varcomp$component[2]
var_r <- summ_g2f_IA_earht_model$varcomp$component[3]
R_IA_earht <- var_g / (var_g + var_gxe + var_r)



################ BLUPs for random effects
################ NE
rn_earht <- rownames(coef(model_g2f_NE_earht)$random)
Pedigree <- sapply(strsplit(rn_earht, "_"), `[`, 2)


effect_sizes <- model_g2f_NE_earht$coefficients$random
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_earht <- effect_sizes_df$effect


blups_g2f_NE_earht <- data.frame(
  "Pedigree" = Pedigree,
  "Earht" = effect_sizes_earht
)



################ MN
rn_earht <- rownames(coef(model_g2f_MN_earht)$random)
Pedigree <- sapply(strsplit(rn_earht, "_"), `[`, 2)


effect_sizes <- model_g2f_MN_earht$coefficients$random
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_earht <- effect_sizes_df$effect


blups_g2f_MN_earht <- data.frame(
  "Pedigree" = Pedigree,
  "Earht" = effect_sizes_earht
)



################ IA
rn_earht <- rownames(coef(model_g2f_IA_earht)$random)
Pedigree <- sapply(strsplit(rn_earht, "_"), `[`, 2)


effect_sizes <- model_g2f_IA_earht$coefficients$random
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_earht <- effect_sizes_df$effect


blups_g2f_IA_earht <- data.frame(
  "Pedigree" = Pedigree,
  "Earht" = effect_sizes_earht
)




