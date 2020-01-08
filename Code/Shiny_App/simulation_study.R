simulation_study <- function(n = 15000, nklassen = 300, niter = 100, sd_intercept = 10, 
                             sd_slope = 0, corr = 0, sd_error = 5, y00 = 15, y10 = 1.5){
  
  # loading one simulation function
  source("one_simulation_ml.R")
  
  # creating data frame for coefs
  coef_models <- data.frame()
  
  # adding coefs for n iterations
  for(num in 1:length(sd_intercept)){
    for(fix_eff in 1:length(y10)){
      for(i in 1:niter){
        coef_models <- rbind(coef_models, one_simulation(n = n, nklassen = nklassen,
                                                     sd_intercept = sd_intercept[num], 
                                                     sd_slope = sd_slope, corr = corr, 
                                                     sd_error = sd_error, 
                                                     y00 = y00, y10 = y10[fix_eff]))
      }
      print(fix_eff)
    }
    print(num)
  }
  
  # formatting data frame
  colnames(coef_models) <- c("beta_0", "beta_treatment", 
                             "SE_beta_0", "SE_beta_treatment", "p_value_0",
                             "p_value_treatment", "p_value_likelihood", "empirical_icc",
                             "theoretical_icc", "sd_intercept", "effect_treatment", "method")
  
  coef_models[,1:9] <- apply(coef_models[,1:9], 2, as.numeric)
  return(coef_models)
}
