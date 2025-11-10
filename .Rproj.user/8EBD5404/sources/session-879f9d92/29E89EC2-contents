group3_twt

group3_twt$HybID <- as.factor(group3_twt$HybID)
group3_twt$EnvID <- as.factor(group3_twt$EnvID)
group3_twt$HybType <- as.factor(group3_twt$HybType)



# Model specification
model_twt <- asreml(
  fixed    =  TWT ~ EnvID,            
  random   = ~ HybID + HybID:EnvID,         
  residual = ~ units,               
  data     = group3_twt
)


# Model Summary
model_summ_twt <- summary(model_twt, coef = TRUE)


# Fixed effects
model_summ_twt$coef.fixed

# Random Effects
model_summ_twt$coef.random


# Variance components
model_summ_twt$varcomp


# Extracting variance components
var_g <- model_summ_twt$varcomp$component[1]
var_gxe <- model_summ_twt$varcomp$component[2]
var_r <- model_summ_twt$varcomp$component[3]


# Repeatability
R_twt <- var_g / (var_g + var_gxe + var_r)

# ID names
rn_twt <- rownames(coef(model_twt)$random)
HybID <- sapply(strsplit(rn_twt, "_"), `[`, 2)



# Extract BLUPs for random effects
effect_sizes <- model_summ_twt$coef.random[, "solution"]
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_twt <- effect_sizes_df$effect_sizes




blups_twt <- data.frame(
  HybID = HybID,
  TWT = effect_sizes_twt
)



