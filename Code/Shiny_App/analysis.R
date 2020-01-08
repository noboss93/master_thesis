library(MASS)
library(lme4)
library(lmerTest)

# function for analysis
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

source("simulation_study.R")

# test simulations with / without effect
saveRDS(simulation_study(sd_intercept = 0), file = "study_sd_0")
saveRDS(simulation_study(sd_intercept = 1), file = "study_sd_1")
saveRDS(simulation_study(sd_intercept = 2), file = "study_sd_2")
saveRDS(simulation_study(sd_intercept = 5), file = "study_sd_5")
saveRDS(simulation_study(sd_intercept = 10), file = "study_sd_10")
saveRDS(simulation_study(sd_intercept = 20), file = "study_sd_20")

saveRDS(simulation_study(sd_intercept = 0), file = "study_sd_0_no_eff")
saveRDS(simulation_study(sd_intercept = 1), file = "study_sd_1_no_eff")
saveRDS(simulation_study(sd_intercept = 2), file = "study_sd_2_no_eff")
saveRDS(simulation_study(sd_intercept = 5), file = "study_sd_5_no_eff")
saveRDS(simulation_study(sd_intercept = 10), file = "study_sd_10_no_eff")
saveRDS(simulation_study(sd_intercept = 20), file = "study_sd_20_no_eff")

# loading test simulations
study_sd_0 <- readRDS(file = "study_sd_0")
study_sd_1 <- readRDS(file = "study_sd_1")
study_sd_2 <- readRDS(file = "study_sd_2")
study_sd_5 <- readRDS(file =  "study_sd_5")
study_sd_10 <- readRDS(file = "study_sd_10")
study_sd_20 <- readRDS(file = "study_sd_20")

study_sd_0_no_eff <- readRDS(file = "study_sd_0_no_eff")
study_sd_1_no_eff <- readRDS(file = "study_sd_1_no_eff")
study_sd_2_no_eff <- readRDS(file = "study_sd_2_no_eff")
study_sd_5_no_eff <- readRDS(file =  "study_sd_5_no_eff")
study_sd_10_no_eff <- readRDS(file = "study_sd_10_no_eff")
study_sd_20_no_eff <- readRDS(file = "study_sd_20_no_eff")

# analysis of test simulations
analyse(study_sd_0)
analyse(study_sd_1)
analyse(study_sd_2)
analyse(study_sd_5)
analyse(study_sd_10)
analyse(study_sd_20)

analyse(study_sd_0_no_eff)
analyse(study_sd_1_no_eff)
analyse(study_sd_2_no_eff)
analyse(study_sd_5_no_eff)
analyse(study_sd_10_no_eff)
analyse(study_sd_20_no_eff)
