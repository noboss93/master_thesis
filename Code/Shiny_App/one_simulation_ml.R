one_simulation <- function(sd_intercept = 10, sd_slope = 0, corr = 0){
  source("dgp_multi_ml.R")
  beta1_mlm <- NA
  beta1_lm <- NA
  ml_data <- gen_ml_data(sd_intercept = sd_intercept, sd_slope = sd_slope, corr = corr)
  mlm_IS_model <- lmer(leistung ~ iq + math_lektionen + (iq | klasse), data = ml_data)
  mlm_I_model <- lmer(leistung ~ iq + math_lektionen + (1 | klasse), data = ml_data)
  lm_model <- lm(leistung ~ iq + math_lektionen, data = ml_data)
  beta_lm <- coef(lm_model)
  SE_lm <- coef(summary(lm_model))[,2]
  SE_I_mlm <- summary(mlm_I_model)$coefficient[,2]
  beta_I_mlm <- fixef(mlm_I_model)
  SE_IS_mlm <- summary(mlm_IS_model)$coefficient[,2]
  beta_IS_mlm <- fixef(mlm_IS_model)
  coefs <- matrix(c(beta_lm, SE_lm, "lm", beta_I_mlm, SE_I_mlm, "rim", beta_IS_mlm, SE_IS_mlm, "rism"), 
                  ncol = 7, byrow = TRUE)
  
  return(coefs)
}

