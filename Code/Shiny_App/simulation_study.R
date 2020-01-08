simulation_study <- function(niter = 100, sd_intercept = 10, sd_slope = 0, 
                             corr = 0, sd_error = 5){
  
  # loading one simulation function
  source("one_simulation_ml.R")
  
  # creating data frame for coefs
  coef_models <- data.frame()
  
  # adding coefs for n iterations
  for(i in 1:niter){
    coef_models <- rbind(coef_models, one_simulation(sd_intercept = sd_intercept, 
                                      sd_slope = sd_slope, corr = corr, sd_error))
  }
  
  # formatting data frame
  colnames(coef_models) <- c("beta_0", "beta_iq", "beta_treatment", 
                             "SE_beta_0", "SE_beta_iq", "SE_beta_treatment", "p_value_0",
                             "p_value_iq", "p_value_treatment","empirical_icc",
                             "theoretical_icc", "method")
  
  coef_models[,1:11] <- apply(coef_models[,1:11], 2, as.numeric)
  coef_models$sample_nr <- rep(1:niter, each = 3)
  
  return(coef_models)
}