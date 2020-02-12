simulation_study <- function(nschueler = 50, 
                             nklassen = 300, 
                             sd_intercept = 2, 
                             sd_slope = 1, 
                             corr = 0, 
                             sd_error = 5, 
                             y00 = 15, 
                             y10 = 0.35,
                             treatment_level1 = TRUE,
                             niter = 100){
  
  # loading one simulation function
  source("one_simulation_ml.R")
  
  # creating data frame for coefs
  coef_models <- data.frame()
  
  # adding coefs for n iterations
  for(num in 1:length(sd_intercept)){
    for(fix_eff in 1:length(y10)){
      for(i in 1:niter){
        coef_models <- rbind(coef_models, one_simulation(nschueler = nschueler, 
                                                         nklassen = nklassen,
                                                         sd_intercept = sd_intercept[num],
                                                         sd_slope = sd_slope, 
                                                         corr = corr, 
                                                         sd_error = sd_error, 
                                                         y00 = y00, 
                                                         y10 = y10[fix_eff],
                                                         treatment_level1 = treatment_level1))
      }
      print(paste(fix_eff, "out of", length(y10), "fixed effects simulated"))
    }
    print(paste(num, "out of", length(sd_intercept), "SDs simulated"))
  }
  print("*** Simulation Complete ***")
  
  # formatting data frame
  colnames(coef_models) <- c("beta_0", "beta_treatment", 
                             "SE_beta_0", "SE_beta_treatment", "p_value_0",
                             "p_value_treatment", "p_value_likelihood", "empirical_icc",
                             "theoretical_icc", "sd_intercept", "sd_error", "effect_treatment", 
                             "method")
  
  coef_models[,1:9] <- apply(coef_models[,1:9], 2, as.character)
  coef_models[,1:9] <- apply(coef_models[,1:9], 2, as.numeric)
  return(coef_models)
}

test_lvl1 <- simulation_study(sd_intercept = sqrt(var_i), sd_slope = 1, y00 = 15, y10 = 0.35)

test_lvl2 <- simulation_study(sd_intercept = sqrt(var_i), y00 = 15, y10 = 0.35, treatment_level1 = FALSE)

par(mfrow = c(2,2))
hist(test_lvl1$p_value_treatment[test_lvl1$method == "lm"], main = "Level 1 LM")
hist(test_lvl1$p_value_treatment[test_lvl1$method == "mlm"], main = "Level 1 MLM")
hist(test_lvl2$p_value_treatment[test_lvl2$method == "lm"], main = "Level 2 LM")
hist(test_lvl2$p_value_treatment[test_lvl2$method == "mlm"], main = "Level 2 MLM")

sum(test_lvl1$p_value_treatment[test_lvl1$method == "lm" & test_lvl1$theoretical_icc == 0.05] < .05) / 100
sum(test_lvl1$p_value_treatment[test_lvl1$method == "mlm"& test_lvl1$theoretical_icc == 0.05] < .05) / 100
sum(test_lvl2$p_value_treatment[test_lvl2$method == "lm"& test_lvl1$theoretical_icc == 0.05] < .05) / 100
sum(test_lvl2$p_value_treatment[test_lvl2$method == "mlm"& test_lvl1$theoretical_icc == 0.05] < .05) / 100


icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
var_i <- c()

for(i in 1:length(icc)){
  var_i[i] <- (icc[i] * 25) / (1 - icc[i])
}


mean(test_lvl1$SE_beta_treatment[test_lvl1$method == "lm" & test_lvl1$theoretical_icc == 0.1]) / 
 sd(test_lvl1$beta_treatment[test_lvl1$method == "lm" & test_lvl1$theoretical_icc == 0.1])


efficacy_lm_lvl1 <- c()
for(i in 1:length(icc)){
efficacy_lm_lvl1[i] <- mean(test_lvl1$SE_beta_treatment[test_lvl1$method == "lm" & test_lvl1$theoretical_icc == icc[i]]) / 
  sd(test_lvl1$beta_treatment[test_lvl1$method == "lm" & test_lvl1$theoretical_icc == icc[i]])
}

efficacy_mlm_lvl1 <- c()
for(i in 1:length(icc)){
  efficacy_mlm_lvl1[i] <- mean(test_lvl1$SE_beta_treatment[test_lvl1$method == "mlm" & test_lvl1$theoretical_icc == icc[i]]) / 
    sd(test_lvl1$beta_treatment[test_lvl1$method == "mlm" & test_lvl1$theoretical_icc == icc[i]])
}

efficacy_lm_lvl2 <- c()
for(i in 1:length(icc)){
  efficacy_lm_lvl2[i] <- mean(test_lvl2$SE_beta_treatment[test_lvl2$method == "lm" & test_lvl2$theoretical_icc == icc[i]]) / 
    sd(test_lvl2$beta_treatment[test_lvl2$method == "lm" & test_lvl2$theoretical_icc == icc[i]])
}

efficacy_mlm_lvl2 <- c()
for(i in 1:length(icc)){
  efficacy_mlm_lvl2[i] <- mean(test_lvl2$SE_beta_treatment[test_lvl2$method == "mlm" & test_lvl1$theoretical_icc == icc[i]]) / 
    sd(test_lvl2$beta_treatment[test_lvl2$method == "mlm" & test_lvl2$theoretical_icc == icc[i]])
}

efficacy_lm_lvl1
efficacy_mlm_lvl1
efficacy_lm_lvl2
efficacy_mlm_lvl2

ggplot(data = test_lvl1, mapping = aes(x = SE_beta_treatment))+
  geom_histogram(bins = 20) +
  facet_wrap(theoretical_icc ~ method)

ggplot(data = test_lvl1, mapping = aes(y = SE_beta_treatment))+
  geom_boxplot() +
  facet_wrap(theoretical_icc ~ method)

ggplot(data = test_lvl2, mapping = aes(x = SE_beta_treatment))+
  geom_histogram(bins = 20) +
  facet_wrap(theoretical_icc ~ method)

ggplot(data = test_lvl2, mapping = aes(y = SE_beta_treatment))+
  geom_boxplot() +
  facet_wrap(theoretical_icc ~ method)



