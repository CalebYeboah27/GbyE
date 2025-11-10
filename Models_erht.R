

group3_pltht$HybID <- as.factor(group3_pltht$HybID)
group3_pltht$EnvID <- as.factor(group3_pltht$EnvID)
group3_pltht$HybType <- as.factor(group3_pltht$HybType)

min(group3_pltht$PLTHT)
max(group3_pltht$PLTHT)


# Model specification
model_pltht <- asreml(
  fixed    =  PLTHT ~ EnvID,            
  random   = ~ HybID + HybID:EnvID,         
  residual = ~ units,               
  data     = group3_pltht
)


# Model Summary
model_summ_pltht <- summary(model_pltht, coef = TRUE)


# Fixed effects
model_summ_pltht$coef.fixed

# Random Effects
model_summ_pltht$coef.random


# Variance components
model_summ_pltht$varcomp




# Extracting variance components
var_g <- model_summ_pltht$varcomp$component[1]
var_gxe <- model_summ_pltht$varcomp$component[2]
var_r <- model_summ_pltht$varcomp$component[3]


# Repeatability
R_pltht <- var_g / (var_g + var_gxe + var_r)

# ID names
rn_pltht <- rownames(coef(model_pltht)$random)
HybID <- sapply(strsplit(rn_pltht, "_"), `[`, 2)



# Extract BLUPs for random effects
effect_sizes <- model_summ_pltht$coef.random[, "solution"]
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_pltht <- effect_sizes_df$effect_sizes




blups_pltht <- data.frame(
  HybID = HybID,
  PLTHT = effect_sizes_pltht
)



