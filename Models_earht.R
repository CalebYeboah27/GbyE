group3_earht

group3_earht$HybID <- as.factor(group3_earht$HybID)
group3_earht$EnvID <- as.factor(group3_earht$EnvID)
group3_earht$HybType <- as.factor(group3_earht$HybType)

min(group3_earht$EARHT)
max(group3_earht$EARHT)


# Model specification
model_earht <- asreml(
  fixed    =  EARHT ~ EnvID,            
  random   = ~ HybID + HybID:EnvID,         
  residual = ~ units,               
  data     = group3_earht
)


# Model Summary
model_summ_earht <- summary(model_earht, coef = TRUE)


# Fixed effects
model_summ_earht$coef.fixed

# Random Effects
model_summ_earht$coef.random


# Variance components
model_summ_earht$varcomp




# Extracting variance components
var_g <- model_summ_earht$varcomp$component[1]
var_gxe <- model_summ_earht$varcomp$component[2]
var_r <- model_summ_earht$varcomp$component[3]


# Repeatability
R_earht <- var_g / (var_g + var_gxe + var_r)

# ID names
rn_earht <- rownames(coef(model_earht)$random)
HybID <- sapply(strsplit(rn_earht, "_"), `[`, 2)



# Extract BLUPs for random effects
effect_sizes <- model_summ_earht$coef.random[, "solution"]
effect_sizes_df <- as.data.frame(effect_sizes)
effect_sizes_earht <- effect_sizes_df$effect_sizes




blups_earht <- data.frame(
  HybID = HybID,
  EARHT = effect_sizes_earht
)



