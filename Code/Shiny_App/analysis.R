library(MASS)
library(lme4)
library(lmerTest)

asdf <- simulation_study(niter = 1)

source("simulation_study.R")
test <- simulation_study(sd_intercept = 10, y10 = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5))
saveRDS(test, file = "test_sim_lvl1")


test_lvl2 <- simulation_study(sd_intercept = c(0,1,2,3,4,5,10), y10 = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5))
saveRDS(test_lvl2, file = "test_sim_lvl2")

boxplot(SE_beta_0 ~ effect_treatment + method, data = test)
boxplot(SE_beta_treatment ~ effect_treatment + method, data = test)
boxplot(p_value_0 ~ effect_treatment + method, data = test)
boxplot(p_value_treatment ~ effect_treatment + method, data = test)
boxplot(p_value_likelihood ~ effect_treatment + method, data = test)

boxplot(theoretical_icc ~ effect_treatment, data = test)
boxplot(empirical_icc~ effect_treatment, data = test)

y10 = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)
for (i in 1:7){
power_lm <- sum(ifelse(test$p_value_likelihood[test$method == "rim" & test$effect_treatment == y10[i]] < 0.05, 1, 0))/
  length(test$p_value_likelihood[test$method == "rim" & test$effect_treatment == y10[i]])
print(power_lm)
}

power_rim <- sum(ifelse(test$p_value_likelihood[test$method == "rim"] < 0.05, 1, 0))/
  length(test$p_value_likelihood[test$method == "rim"])

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


