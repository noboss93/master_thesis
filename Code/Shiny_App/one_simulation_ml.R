one_simulation <- function(n = 15000, nklassen = 300, sd_intercept = 2, sd_slope = 0, 
                           corr = 0, sd_error = 5, y00 = 15, y10 = 0.35){
  
  # loading dgp function
  source("dgp_multi_ml.R")

  # generating one data set
  ml_data <- gen_ml_data(n = n, nklassen = nklassen, sd_intercept = sd_intercept, 
                         sd_slope = sd_slope,
                         corr = corr, sd_error = sd_error, y00 = y00, y10 = y10)
  
  # calculating model
  lm_model_0 <- lm(leistung ~ 1, data = ml_data)
  lm_model <- lm(leistung ~  math_lektionen, data = ml_data)
  
  mlm_model_0 <- lmer(leistung ~ 1 + (1 | klasse), data = ml_data, REML = FALSE)
  mlm_I_model <- lmer(leistung ~ math_lektionen + 
                        (1 | klasse), data = ml_data, REML = FALSE)
  # mlm_IS_model <- lmer(leistung ~ math_lektionen + 
  #                        (math_lektionen | klasse), data = ml_data)
  
  # saving coefficients
  empirical_icc <- VarCorr(mlm_model_0)$klasse[1,1] / (VarCorr(mlm_model_0)$klasse[1,1] 
                                                         + sigma(mlm_model_0)^2)
  theoretical_icc <- var(ml_data$random_intercept) / var(ml_data$leistung)
  
  beta_lm <- coef(lm_model)
  SE_lm <- coef(summary(lm_model))[,2]
  p_lm <- coef(summary(lm_model))[,4]
  
  lq_lm <- anova(lm_model_0, lm_model, test = "LRT")
  p_lq_lm <- lq_lm$`Pr(>Chi)`[2]
  
  beta_I_mlm <- fixef(mlm_I_model)
  SE_I_mlm <- summary(mlm_I_model)$coefficient[,2]
  p_I_mlm <- coef(summary(mlm_I_model))[,5]
  
  lq_I_mlm <- anova(mlm_model_0, mlm_I_model, test = "LRT")
  p_lq_I_mlm <- lq_I_mlm$`Pr(>Chisq)`[2]
  
  # beta_IS_mlm <- fixef(mlm_IS_model)
  # SE_IS_mlm <- summary(mlm_IS_model)$coefficient[,2]
  # p_IS_mlm <- coef(summary(mlm_IS_model))[,5]
  # 
  # lq_IS_mlm <- anova(mlm_I_model, mlm_IS_model, test = "LRT")
  # p_lq_IS_mlm <- lq_IS_mlm$`Pr(>Chisq)`[2]
  
  # saving all coefficients in a matrix
  coefs <- matrix(c(beta_lm, SE_lm, p_lm, p_lq_lm, empirical_icc, theoretical_icc, sd_intercept, y10, "lm", 
                    beta_I_mlm, SE_I_mlm, p_I_mlm, p_lq_I_mlm, empirical_icc, theoretical_icc, sd_intercept, y10, "rim"#, 
                    # beta_IS_mlm, SE_IS_mlm, p_IS_mlm, p_lq_IS_mlm, empirical_icc, theoretical_icc, "rism"
                    ), 
                ncol = 12, byrow = TRUE)
  
  return(coefs)
}

