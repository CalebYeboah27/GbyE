install.packages("data.table")
install.packages("ggplot2")
install.packages("jsonlite")
install.packages("lme4")
install.packages("~/Downloads/Setups/asreml_4.2.0.355.tgz", 
                 repos = NULL, type = "mac.binary")

library(asreml)
library(lme4)

names(g2f_NE)
dim(g2f_NE)
str(g2f_NE)

g2f_NE$Pedigree <- as.factor(g2f_NE$Pedigree)
g2f_MN$Pedigree <- as.factor(g2f_MN$Pedigree)
g2f_IA$Pedigree <- as.factor(g2f_IA$Pedigree)



g2f_2021_df_yld$EnvID <- as.factor(g2f_2021_df_yld$EnvID)
g2f_2021_df_yld$Pedigree <- as.factor(g2f_2021_df_yld$Pedigree)
g2f_2021_df_yld$GrainYield <- as.numeric(g2f_2021_df_yld$GrainYield)



###########################  Model specification ########################


g2f_NE_yld <- asreml(
  fixed    = GrainYield  ~ EnvID,            
  random   = ~ Pedigree + Pedigree:EnvID,         
  residual = ~ units,               
  data     = g2f_2021_df_yld
)

model_summ_NE_21_yld <- summary(g2f_NE_yld)


# Variance components
model_summ_NE_21_yld$varcomp


# Extracting variance components
var_g <- model_summ_NE_21_yld$varcomp$component[1]
var_gxe <- model_summ_NE_21_yld$varcomp$component[2]
var_r <- model_summ_NE_21_yld$varcomp$component[3]


# Repeatability
R_yield <- var_g / (var_g + var_gxe + var_r)





# Model Summary
model_summ_yld <- summary(model_yld, coef = TRUE)


# Fixed effects
model_summ_yld$coef.fixed

# Random Effects
model_summ_yld$coef.random


# Variance components
model_summ_yld$varcomp


# Extracting variance components
var_g <- model_summ_yld$varcomp$component[1]
var_gxe <- model_summ_yld$varcomp$component[2]
var_r <- model_summ_yld$varcomp$component[3]


# Repeatability
R_yld <- var_g / (var_g + var_gxe + var_r)

# ID names
rn_yld <- rownames(coef(model_yld)$random)
HybID <- sapply(strsplit(rn_yld, "_"), `[`, 2)



# Extract BLUPs for random effects
effect_sizes <- model_summ_yld$coef.random[, "solution"]
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_yld <- effect_sizes_df$effect_sizes



blups_yld <- data.frame(
  HybID = HybID,
  YLD = effect_sizes_yld
)


