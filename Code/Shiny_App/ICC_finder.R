source("dgp_multi_ml.R")

icc <- function(ml_data){
  
  temp_model <- lmer(leistung ~ 1 + (1 | klasse), data = ml_data)
  icc_model <- VarCorr(temp_model)$klasse[1,1] / (VarCorr(temp_model)$klasse[1,1] + sigma(temp_model)^2)
  
  return(icc_model)
}



icc(gen_ml_data(sd_intercept = 0, sd_slope = 0, corr = 0))

