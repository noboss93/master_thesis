library(MASS)
library(lme4)
library(lmerTest)
library(ggplot2)

# Simulating test study
source("simulation_study.R")
test_lvl1 <- simulation_study(niter = 10, sd_intercept = c(0,1,2,3,4,5), 
                              y10 = c(0, 0.25, 0.5, 0.75, 1, 1.5))

power_analyze(test_lvl1)
saveRDS(test_lvl1, file = "test_sim_lvl1")


test_lvl2 <- simulation_study(niter = 1000, sd_intercept = c(0,1,2,3,4,5), 
                              y10 = c(0, 0.25, 0.5, 0.75, 1, 1.5))
saveRDS(test_lvl2, file = "test_sim_lvl2")

# loading files
test_sim_lvl1 <- readRDS("test_sim_lvl1")
test_sim_lvl2 <- readRDS("test_sim_lvl2")

# plotting first results
ggplot(data = test_sim_lvl1, mapping = aes(y = p_value_likelihood, x = method, fill = sd_intercept)) +
  geom_boxplot()+
  facet_wrap(~ effect_treatment)

ggplot(data = test_sim_lvl2, mapping = aes(y = p_value_likelihood, x = method, fill = sd_intercept)) +
  geom_boxplot()+
  facet_wrap(~ effect_treatment)

# function to analyse data
power_analyze <- function(x){
  y10 = c(0, 0.25, 0.5, 0.75, 1, 1.5)
  sd_i = c(0,1,2,3,4,5)
  meth = c("lm", "rim")
  
  a <- matrix(ncol = 6, nrow = 6)
  b <- matrix(ncol = 6, nrow = 6)
  for (m in 1){
    for (n in 1:6){
      for (i in 1:6){
        a[n,i] <- sum(ifelse(x$p_value_likelihood[x$method == meth[m] & x$effect_treatment == y10[i] & x$sd_intercept == sd_i[n]] < 0.05, 1, 0))/
          length(x$p_value_likelihood[x$method == meth[m] & x$effect_treatment == y10[i] & x$sd_intercept == sd_i[n]])
      }
    }
  }
  
  for (m in 2){
    for (n in 1:6){
      for (i in 1:6){
        b[n,i] <- sum(ifelse(x$p_value_likelihood[x$method == meth[m] & x$effect_treatment == y10[i] & x$sd_intercept == sd_i[n]] < 0.05, 1, 0))/
          length(x$p_value_likelihood[x$method == meth[m] & x$effect_treatment == y10[i] & x$sd_intercept == sd_i[n]])
      }
    }
  }
  
  power <- rbind(a, b)
  
  sd_i2 <- sd_i^2
  icc <- sd_i2 / (sd_i2 + 25)
  
  icc <- rep(icc, each = 6, times = 2)
  eff <- rep(y10, times = 12)
  methods <- rep(meth, each = 36)
  
  pwr <- c()
  for (i in 1: 12){
    pwr <- append(pwr, power[i,])
  }
  
  power_matrix <- matrix(c(pwr, eff, round(icc, digits = 2), methods), ncol = 4, nrow = 72)
  power_dataframe <- data.frame(power_matrix)
  
  power_dataframe[,1] <- as.numeric(as.character(power_dataframe[,1]))
  colnames(power_dataframe) <- c("power", "effectsize_treatment", "theoretical_icc", "method")
  
  ggplot(data = power_dataframe, mapping = aes(x = theoretical_icc , y = power, group = method)) +
    geom_line(aes(color = method)) +
    labs(title = "Veränderung von Power / Typ-1 Fehler in versch. Einflussstärken eines Treatments") +
    facet_wrap(~ effectsize_treatment)
}

power_lvl1 <- power_analyze(test_sim_lvl1)
power_lvl2 <- power_analyze(test_sim_lvl2)



