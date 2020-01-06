one_simulation <- function(sd_intercept = 10, sd_slope = 0, corr = 0, sd_error = 5){
  
  # loading dgp function
  source("dgp_multi_ml.R")

  # generating one data set
  ml_data <- gen_ml_data(sd_intercept = sd_intercept, sd_slope = sd_slope, 
                         corr = corr, sd_error = sd_error)
  
  # calculating model
  mlm_ICC_model <- lmer(leistung ~ 1 + (1 | klasse), data = ml_data)
  mlm_IS_model <- lmer(leistung ~ iq_centered + math_lektionen + 
                         (iq_centered | klasse), data = ml_data)
  mlm_I_model <- lmer(leistung ~ iq_centered + math_lektionen + 
                        (1 | klasse), data = ml_data)
  lm_model <- lm(leistung ~ iq_centered + math_lektionen, data = ml_data)
  
  # saving coefficients
  beta_lm <- coef(lm_model)
  SE_lm <- coef(summary(lm_model))[,2]
  p_lm <- coef(summary(lm_model))[,4]
  
  beta_I_mlm <- fixef(mlm_I_model)
  SE_I_mlm <- summary(mlm_I_model)$coefficient[,2]
  p_I_mlm <- coef(summary(mlm_I_model))[,5]
  
  beta_IS_mlm <- fixef(mlm_IS_model)
  SE_IS_mlm <- summary(mlm_IS_model)$coefficient[,2]
  p_IS_mlm <- coef(summary(mlm_IS_model))[,5]
  
  empirical_icc <- VarCorr(mlm_ICC_model)$klasse[1,1] / (VarCorr(mlm_ICC_model)$klasse[1,1] + sigma(mlm_ICC_model)^2)
  theoretical_icc <- sd_intercept^2 / (sd_intercept^2 + var(ml_data$leistung))
  
  coefs <- matrix(c(beta_lm, SE_lm, p_lm, empirical_icc, theoretical_icc, "lm", beta_I_mlm, 
                    SE_I_mlm, p_I_mlm, empirical_icc, theoretical_icc, "rim", beta_IS_mlm, 
                    SE_IS_mlm, p_IS_mlm, empirical_icc, theoretical_icc, "rism"), 
                ncol = 12, byrow = TRUE)
  
  return(coefs)
}


