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


# testing function
library(MASS)
library(lme4)
library(lmerTest)
test <- simulation_study(sd_intercept = 1)

saveRDS(simulation_study(sd_intercept = 0), file = "study_sd_0")
saveRDS(simulation_study(sd_intercept = 1), file = "study_sd_1")
saveRDS(simulation_study(sd_intercept = 2), file = "study_sd_2")
saveRDS(simulation_study(sd_intercept = 5), file = "study_sd_5")
saveRDS(simulation_study(sd_intercept = 10), file = "study_sd_10")
saveRDS(simulation_study(sd_intercept = 20), file = "study_sd_20")

study_sd_0 <- readRDS(file = "study_sd_0")
study_sd_1 <- readRDS(file = "study_sd_1")
study_sd_2 <- readRDS(file = "study_sd_2")
study_sd_5 <- readRDS(file =  "study_sd_5")
study_sd_10 <- readRDS(file = "study_sd_10")
study_sd_20 <- readRDS(file = "study_sd_20")

analyse(study_sd_0)
analyse(study_sd_1)
analyse(study_sd_2)
analyse(study_sd_5)
analyse(study_sd_10)
analyse(study_sd_20)

# testing plots and comparing coefs
analyse <- function(x){
boxplot(beta_0 ~ method, data = x)
boxplot(beta_iq ~ method, data = x)
boxplot(beta_treatment ~ method, data = x)
boxplot(SE_beta_0 ~ method, data = x)
boxplot(SE_beta_iq ~ method, data = x)
boxplot(SE_beta_treatment ~ method, data = x)
boxplot(p_value_0 ~ method, data = x)
boxplot(p_value_iq ~ method, data = x)
boxplot(p_value_treatment ~ method, data = x)

power_lm <- sum(ifelse(x$p_value_treatment[x$method == "lm"] < 0.05, 1, 0))/100
power_rim <- sum(ifelse(x$p_value_treatment[x$method == "rim"] < 0.05, 1, 0))/100
power_rism <- sum(ifelse(x$p_value_treatment[x$method == "rism"] < 0.05, 1, 0))/100

eicc <- mean(x$empirical_icc)
ticc <- mean(x$theoretical_icc)

return(print(round(c(power_lm, power_rim, power_rism, eicc, ticc), digits = 3)))
}


