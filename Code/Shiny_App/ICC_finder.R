source("dgp_multi_ml.R")

find_icc <- function(ml_data){
  
  temp_model <- lmer(leistung ~ 1 + (1 | klasse), data = ml_data)
  icc_model <- round(VarCorr(temp_model)$klasse[1,1] / (VarCorr(temp_model)$klasse[1,1] + sigma(temp_model)^2), digits = 3)
  
  return(icc_model)
}

icc(gen_ml_data(sd_intercept = 90, sd_slope = 5, corr = 0.7))

