install.packages("data.table")
install.packages("ggplot2")
install.packages("jsonlite")
install.packages("lme4")
install.packages("~/Downloads/Setups/asreml_4.2.0.355.tgz", 
                 repos = NULL, type = "mac.binary")

library(asreml)
library(lme4)


group3_yld$HybID <- as.factor(group3_yld$HybID)
group3_yld$EnvID <- as.factor(group3_yld$EnvID)

group3_yld$EnvID


# Model specification
model_yld <- asreml(
                  fixed    =  YLD ~ EnvID,            
                  random   = ~ HybID + HybID:EnvID,         
                  residual = ~ units,               
                  data     = group3_yld
                )


anova(lmer(YLD ~ EnvID + (1|HybID) + (1|HybID:EnvID), data = group3_yld))




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


