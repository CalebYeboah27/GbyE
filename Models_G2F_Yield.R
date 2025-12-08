#install.packages("data.table")
#install.packages("ggplot2")
#install.packages("jsonlite")
#install.packages("lme4")
#install.packages("~/Downloads/Setups/asreml_4.2.0.355.tgz", 
#                 repos = NULL, type = "mac.binary")

library(asreml)
library(lme4)

names(g2f_NE)
dim(g2f_NE)
str(g2f_NE)


############# Categorical variables to factors

g2f_NE$Pedigree <- as.factor(g2f_NE$Pedigree)
g2f_MN$Pedigree <- as.factor(g2f_MN$Pedigree)
g2f_IA$Pedigree <- as.factor(g2f_IA$Pedigree)



g2f_yld_df$EnvID <- as.factor(g2f_yld_df$EnvID)
g2f_yld_df$Pedigree <- as.factor(g2f_yld_df$Pedigree)
g2f_yld_df$GrainYield <- as.numeric(g2f_yld_df$GrainYield)



###########################  Yield ########################

############### Dataframe
g2f_NE_yld_df <- g2f_yld_df %>% filter(State == "NE")
g2f_MN_yld_df <- g2f_yld_df %>% filter(State == "MN")
g2f_IA_yld_df <- g2f_yld_df %>% filter(State == "IA")


############## Model specification

model_g2f_NE_yld <- asreml(
  fixed    = GrainYield  ~ EnvID,            
  random   = ~ Pedigree + Pedigree:EnvID,         
  residual = ~ units,               
  data     = g2f_NE_yld_df
)

model_g2f_MN_yld <- asreml(
  fixed    = GrainYield  ~ EnvID,            
  random   = ~ Pedigree + Pedigree:EnvID,         
  residual = ~ units,               
  data     = g2f_MN_yld_df
)

model_g2f_IA_yld <- asreml(
  fixed    = GrainYield  ~ EnvID,            
  random   = ~ Pedigree + Pedigree:EnvID,         
  residual = ~ units,               
  data     = g2f_IA_yld_df
)


############## model summary
summ_g2f_NE_yld_model <- summary(model_g2f_NE_yld, coef = TRUE)
summ_g2f_MN_yld_model <- summary(model_g2f_MN_yld, coef = TRUE)
summ_g2f_IA_yld_model <- summary(model_g2f_IA_yld, coef = TRUE)


############### Fixed effects
summ_g2f_NE_yld_model$coef.fixed
summ_g2f_MN_yld_model$coef.fixed
summ_g2f_IA_yld_model$coef.fixed


################ Random Effects
summ_g2f_NE_yld_model$coef.random
summ_g2f_MN_yld_model$coef.random
summ_g2f_IA_yld_model$coef.random


################ Variance components
summ_g2f_NE_yld_model$varcomp
summ_g2f_NE_yld_model$varcomp
summ_g2f_NE_yld_model$varcomp


################ Extracting variance components
var_g <- summ_g2f_NE_yld_model$varcomp$component[1]
var_gxe <- summ_g2f_NE_yld_model$varcomp$component[2]
var_r <- summ_g2f_NE_yld_model$varcomp$component[3]
R_NE_yield <- var_g / (var_g + var_gxe + var_r)


var_g <- summ_g2f_MN_yld_model$varcomp$component[1]
var_gxe <- summ_g2f_MN_yld_model$varcomp$component[2]
var_r <- summ_g2f_MN_yld_model$varcomp$component[3]
R_MN_yield <- var_g / (var_g + var_gxe + var_r)


var_g <- summ_g2f_IA_yld_model$varcomp$component[1]
var_gxe <- summ_g2f_IA_yld_model$varcomp$component[2]
var_r <- summ_g2f_IA_yld_model$varcomp$component[3]
R_IA_yield <- var_g / (var_g + var_gxe + var_r)



################ BLUPs for random effects
################ NE
rn_yld <- rownames(coef(model_g2f_NE_yld)$random)
Pedigree <- sapply(strsplit(rn_yld, "_"), `[`, 2)


effect_sizes <- model_g2f_NE_yld$coefficients$random
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_yld <- effect_sizes_df$effect

blups_g2f_NE_yld <- data.frame(
  "Pedigree" = Pedigree,
  "NE_Yield" = effect_sizes_yld
)


################ MN
rn_yld <- rownames(coef(model_g2f_NE_yld)$random)
Pedigree <- sapply(strsplit(rn_yld, "_"), `[`, 2)


effect_sizes <- model_g2f_NE_yld$coefficients$random
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_yld <- effect_sizes_df$effect

blups_g2f_MN_yld <- data.frame(
  "Pedigree" = Pedigree,
  "MN_Yield" = effect_sizes_yld
)


################ IA
rn_yld <- rownames(coef(model_g2f_NE_yld)$random)
Pedigree <- sapply(strsplit(rn_yld, "_"), `[`, 2)


effect_sizes <- model_g2f_NE_yld$coefficients$random
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_yld <- effect_sizes_df$effect

blups_g2f_IA_yld <- data.frame(
  "Pedigree" = Pedigree,
  "IA_Yield" = effect_sizes_yld
)


