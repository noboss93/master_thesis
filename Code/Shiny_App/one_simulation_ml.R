one_simulation <- function(nschueler = 50, 
                           nklassen = 300, 
                           sd_intercept = 3, 
                           sd_slope = 0, 
                           corr = 0, 
                           sd_error = 5,
                           y00 = 15, 
                           y10 = 0.3,
                           treatment_level1 = TRUE){
  
  # loading dgp function
  source("dgp_multi_ml.R")

  # generating one data set
  ml_data <- gen_ml_data(nschueler = nschueler, 
                         nklassen = nklassen, 
                         sd_intercept = sd_intercept, 
                         sd_slope = sd_slope,
                         corr = corr, 
                         sd_error = sd_error, 
                         y00 = y00, 
                         y10 = y10,
                         treatment_level1 = treatment_level1)
  
  # calculating model
  lm_model_0 <- lm(leistung ~ 1, data = ml_data)
  lm_model <- lm(leistung ~  uebung, data = ml_data)
  
  mlm_model_0 <- lmer(leistung ~ 1 + (1 | klasse), data = ml_data, REML = FALSE)
  
  if (treatment_level1 == TRUE){
    
    mlm_model <- lmer(leistung ~ uebung + (uebung || klasse), data = ml_data, REML = FALSE)
    
  } else{
    mlm_model <- lmer(leistung ~ uebung + (1 | klasse), data = ml_data, REML = FALSE)
 
  }
  
  # saving coefficients
  empirical_icc <- VarCorr(mlm_model_0)$klasse[1,1] / (VarCorr(mlm_model_0)$klasse[1,1] 
                                                       + sigma(mlm_model_0)^2)
  theoretical_icc <- sd_intercept^2 / (sd_intercept^2 + sd_error^2)
  
  
  
  beta_lm <- coef(lm_model)
  SE_lm <- coef(summary(lm_model))[,2]
  p_lm <- coef(summary(lm_model))[,4]
  
  lq_lm <- anova(lm_model_0, lm_model, test = "LRT")
  p_lq_lm <- lq_lm$`Pr(>Chi)`[2]
  

  
  beta_mlm <- fixef(mlm_model)
  SE_mlm <- summary(mlm_model)$coefficient[,2]
  p_mlm <- coef(summary(mlm_model))[,5]
  
  lq_mlm <- anova(mlm_model_0, mlm_model, test = "LRT")
  p_lq_mlm <- lq_mlm$`Pr(>Chisq)`[2]
  
  # saving all coefficients in a matrix
  coefs <- matrix(c(beta_lm, SE_lm, p_lm, p_lq_lm, empirical_icc, theoretical_icc, sd_intercept, sd_error, y10, "lm", 
                    beta_mlm, SE_mlm, p_mlm, p_lq_mlm, empirical_icc, theoretical_icc, sd_intercept, sd_error, y10, "mlm"
                    ), 
                ncol = 13, byrow = TRUE)
  
  return(coefs)
}
