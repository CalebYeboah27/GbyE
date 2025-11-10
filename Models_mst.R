group3_mst

group3_mst$HybID <- as.factor(group3_mst$HybID)
group3_mst$EnvID <- as.factor(group3_mst$EnvID)
group3_mst$HybType <- as.factor(group3_mst$HybType)



# Model specification
model_mst <- asreml(
  fixed    =  MST ~ EnvID,            
  random   = ~ HybID + HybID:EnvID,         
  residual = ~ units,               
  data     = group3_mst
)


# Model Summary
model_summ_mst <- summary(model_mst, coef = TRUE)


# Fixed effects
model_summ_mst$coef.fixed

# Random Effects
model_summ_mst$coef.random


# Variance components
model_summ_mst$varcomp


# Extracting variance components
var_g <- model_summ_mst$varcomp$component[1]
var_gxe <- model_summ_mst$varcomp$component[2]
var_r <- model_summ_mst$varcomp$component[3]


# Repeatability
R_mst <- var_g / (var_g + var_gxe + var_r)

# ID names
rn_mst <- rownames(coef(model_mst)$random)
HybID <- sapply(strsplit(rn_mst, "_"), `[`, 2)



# Extract BLUPs for random effects
effect_sizes <- model_summ_mst$coef.random[, "solution"]
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_mst <- effect_sizes_df$effect_sizes




blups_mst <- data.frame(
  HybID = HybID,
  MST = effect_sizes_mst
)

