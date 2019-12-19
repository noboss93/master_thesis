simulation_study <- function(niter = 100, sd_intercept = 10, sd_slope = 0, corr = 0){
  
  # loading one simulation function
  source("one_simulation_ml.R")
  
  # creating data frame for coefs
  coef_models <- data.frame()
  
  # adding coefs for n iterations
  for(i in 1:niter){
    coef_models <- rbind(coef_models, one_simulation(sd_intercept = sd_intercept, 
                                      sd_slope = sd_slope, corr = corr))
  }
  
  # formatting data frame
  colnames(coef_models) <- c("beta_0", "beta_1", "beta_2", 
                             "SE_beta_0", "SE_beta_1", "SE_beta_2",
                             "method")
  
  coef_models[,1:6] <- apply(coef_models[,1:6], 2, as.numeric)
  
  return(coef_models)
}

test <- simulation_study(sd_intercept = 10, sd_slope = 5, corr = 0.5)

boxplot(beta_1 ~ method, data = test)
boxplot(beta_2 ~ method, data = test)
boxplot(SE_beta_1 ~ method, data = test)

summary(aov(SE_beta_1 ~ method, data = test))
