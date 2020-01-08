library(MASS)
library(lme4)
library(lmerTest)

# function for analysis
analyse <- function(x){
  boxplot(beta_0 ~ method, data = x)
  boxplot(beta_treatment ~ method, data = x)
  boxplot(SE_beta_0 ~ method, data = x)
  boxplot(SE_beta_treatment ~ method, data = x)
  boxplot(p_value_0 ~ method, data = x)
  boxplot(p_value_treatment ~ method, data = x)
  boxplot(p_value_likelihood ~ method, data = x)
  
  power_lm <- sum(ifelse(x$p_value_likelihood[x$method == "lm"] < 0.05, 1, 0))/
    length(x$p_value_likelihood[x$method == "lm"])
  power_rim <- sum(ifelse(x$p_value_likelihood[x$method == "rim"] < 0.05, 1, 0))/
    length(x$p_value_likelihood[x$method == "rim"])
  # power_rism <- sum(ifelse(x$p_value_likelihood[x$method == "rism"] < 0.05, 1, 0))/
  #   length(x$p_value_likelihood[x$method == "rism"])
  
  eicc <- mean(x$empirical_icc)
  ticc <- mean(x$theoretical_icc)
  
  return(print(round(c(power_lm, 
                       power_rim, 
                       # power_rism, 
                       eicc, ticc), digits = 3)))
}

source("simulation_study.R")

test <- simulation_study(sd_intercept = 10, y10 = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5))


# test simulations with / without effect
saveRDS(simulation_study(sd_intercept = 0, y10 = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)), file = "study_sd_0")
saveRDS(simulation_study(sd_intercept = 1, y10 = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)), file = "study_sd_1")
saveRDS(simulation_study(sd_intercept = 2, y10 = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)), file = "study_sd_2")
saveRDS(simulation_study(sd_intercept = 3, y10 = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)), file = "study_sd_3")
saveRDS(simulation_study(sd_intercept = 4, y10 = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)), file = "study_sd_4")
saveRDS(simulation_study(sd_intercept = 5, y10 = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)), file = "study_sd_5")
saveRDS(simulation_study(sd_intercept = 10, y10 = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)), file = "study_sd_10")

# loading test simulations
study_sd_0 <- readRDS(file = "study_sd_0")
study_sd_1 <- readRDS(file = "study_sd_1")
study_sd_2 <- readRDS(file = "study_sd_2")
study_sd_3 <- readRDS(file =  "study_sd_3")
study_sd_4 <- readRDS(file = "study_sd_4")
study_sd_5 <- readRDS(file = "study_sd_5")
study_sd_10 <- readRDS(file = "study_sd_10")

# analysis of test simulations
analyse(study_sd_0)
analyse(study_sd_1)
analyse(study_sd_2)
analyse(study_sd_3)
analyse(study_sd_4)
analyse(study_sd_5)
analyse(study_sd_6)

analyse(study_sd_0_no_eff)
analyse(study_sd_1_no_eff)
analyse(study_sd_2_no_eff)
analyse(study_sd_5_no_eff)
analyse(study_sd_10_no_eff)
analyse(study_sd_20_no_eff)


