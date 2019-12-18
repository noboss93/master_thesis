simulation_study <- function(niter = 100, sd_intercept = 10, sd_slope = 0, corr = 0){
  source("one_simulation_ml.R")
  coef_models <- data.frame()
  
  for(i in 1:niter){
    coef_models <- rbind(coef_models, one_simulation(sd_intercept = sd_intercept, 
                                      sd_slope = sd_slope, corr = corr))
  }
  colnames(coef_models) <- c("beta_0", "beta_1", "beta_2", 
                             "SE_beta_0", "SE_beta_1", "SE_beta_2",
                             "method")
  coef_models$method <- as.factor(coef_models$method)
  return(coef_models)
}

test_10 <- simulation_study(sd_intercept = 10)
test_0 <- simulation_study(sd_intercept = 0)

boxplot(beta_1 ~ method, data = test_10)

test_10$beta1_lm <- test_10$beta1_lm - 1.5
test_10$beta1_mlm <- test_10$beta1_mlm - 1.5

t.test(test_10$beta1_lm, test_10$beta1_mlm)

test_0$beta1_lm <- test_0$beta1_lm - 1.5
test_0$beta1_mlm <- test_0$beta1_mlm - 1.5

t.test(test_0$beta1_lm, test_0$beta1_mlm)
