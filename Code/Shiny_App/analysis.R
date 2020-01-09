library(MASS)
library(lme4)
library(lmerTest)
library(ggplot2)

analyse <- function(x){
  boxplot(SE_beta_0 ~ effect_treatment + method + sd_intercept, data = x)
  boxplot(SE_beta_treatment ~ effect_treatment + method + sd_intercept, data = x)
  boxplot(p_value_0 ~ effect_treatment + method + sd_intercept, data = x)
  boxplot(p_value_treatment ~ effect_treatment + method + sd_intercept, data = x)
  boxplot(p_value_likelihood ~ effect_treatment + method + sd_intercept, data = x)
  
  boxplot(theoretical_icc ~ effect_treatment + sd_intercept, data = x)
  boxplot(empirical_icc~ effect_treatment + sd_intercept, data = x)
}

source("simulation_study.R")
test_lvl1 <- simulation_study(niter = 1000, sd_intercept = c(0,1,2,3,4,5), 
                              y10 = c(0, 0.25, 0.5, 0.75, 1, 1.5))
saveRDS(test, file = "test_sim_lvl1")


test_lvl2 <- simulation_study(niter = 1000, sd_intercept = c(0,1,2,3,4,5), 
                              y10 = c(0, 0.25, 0.5, 0.75, 1, 1.5))
saveRDS(test_lvl2, file = "test_sim_lvl2")


test_sim_lvl1 <- readRDS("test_sim_lvl1")
test_sim_lvl2 <- readRDS("test_sim_lvl2")

analyse(test_sim_lvl2)


ggplot(data = test_sim_lvl2, mapping = aes(y = p_value_likelihood, x = theoretical_icc, fill = method)) +
  geom_boxplot()+
  facet_wrap(~ effect_treatment)

ggplot(data = test_sim_lvl2, mapping = aes(y = p_value_likelihood, x = method, fill = sd_intercept)) +
  geom_boxplot()+
  facet_wrap(~ effect_treatment)








y10 = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)
for (i in 1:7){
power_lm <- sum(ifelse(test_lv1$p_value_likelihood[test_lvl1$method == "rim" & test_lvl1$effect_treatment == y10[i]] < 0.05, 1, 0))/
  length(test_lvl1$p_value_likelihood[test_lvl1$method == "rim" & test_lvl1$effect_treatment == y10[i]])
print(power_lm)
}

power_rim <- sum(ifelse(test$p_value_likelihood[test$method == "rim"] < 0.05, 1, 0))/
  length(test$p_value_likelihood[test$method == "rim"])


