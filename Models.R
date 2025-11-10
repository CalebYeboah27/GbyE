install.packages("data.table")
install.packages("ggplot2")
install.packages("jsonlite")
install.packages("~/Downloads/Setups/asreml_4.2.0.355.tgz", 
                 repos = NULL, type = "mac.binary")

library(asreml)

group3_yld$HybID

model_yld <- asreml(
  fixed    =  YLD ~ EnvID,            
  random   = ~ PINumber,         
  residual = ~ units,               
  data     = group3_yld
)
