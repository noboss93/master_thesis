### Simulation Study Multilevel Analysis ###
# Libraries ---------------------------------------------------------------
library(MASS)
library(lme4)
library(lmerTest)
library(ggplot2)

# Data Generating Process -------------------------------------------------
gen_ml_data <- function(nschueler = 50, 
                        nklassen = 300, 
                        sd_intercept = 3, 
                        sd_slope = 0, 
                        corr = 0, 
                        sd_error = 5,
                        y00 = 15, 
                        y10 = 0.35,
                        treatment_level1 = TRUE){
  
  # Creating Variables
  n <- nschueler * nklassen
  error <- rnorm(n, mean = 0, sd = sd_error)
  klasse <- rep(1:nklassen, each = nschueler)
  
  # Creating random effects of klassen
  covar01 <- corr * sqrt(sd_intercept^2 * sd_slope^2)
  
  effekte_cov_matrix <- matrix(c(sd_intercept^2,
                                 covar01,
                                 covar01,
                                 sd_slope^2
                                 ), 2, 2)
  
  effekte <- mvrnorm(n = nklassen, mu = c(0,0), 
                     Sigma = effekte_cov_matrix, empirical = TRUE)
  
  random_intercept <- rep(effekte[,1], each = nschueler)
  random_slope <- rep(effekte[,2], each = nschueler)
  
  if (treatment_level1 == TRUE){
    uebung <- c()
    for(i in 1:nklassen){
      uebung <- append(uebung, sample(rep(c(-1,1), each = nschueler/2), size = nschueler), 
                             after = length(uebung))
    }
    
    # Calculating individual leistung score
    leistung <- numeric(n)
    for (i in 1:n){
      leistung[i] <- y00 +  
        y10 * uebung[i] + 
        random_intercept[i] + 
        random_slope[i] * uebung[i] + 
        error[i]
    }
    
  } else{
    uebung <- rep(sample(rep(c(-1,1), each = nklassen/2), size = nklassen), 
                  each = nschueler)
    
    leistung <- numeric(n)
    for (i in 1:n){
      leistung[i] <- y00 +  
        y10 * uebung[i] + 
        random_intercept[i] + 
        error[i]
    }
  }
  
  # Creating dataframe
  klasse <- as.factor(klasse)
  
  ml_data <- data.frame(klasse, uebung, leistung, random_intercept, random_slope)
  
  return(ml_data)
}



# One Simulation ----------------------------------------------------------
one_simulation <- function(nschueler = 50, 
                           nklassen = 300, 
                           sd_intercept = 3, 
                           sd_slope = 0, 
                           corr = 0, 
                           sd_error = 5,
                           y00 = 15, 
                           y10 = 0.3,
                           treatment_level1 = TRUE){
  
  # generating one data set
  ml_data <- gen_ml_data(nschueler = nschueler, 
                         nklassen = nklassen, 
                         sd_intercept = sd_intercept, 
                         sd_slope = sd_slope,
                         corr = corr, 
                         sd_error = sd_error, 
                         y00 = y00, 
                         y10 = y10,
                         treatment_level1 = treatment_level1)
  
  # calculating model
  # lm_model_0 <- lm(leistung ~ 1, data = ml_data)
  lm_model <- lm(leistung ~  uebung, data = ml_data)
  
  mlm_model_0 <- lmer(leistung ~ 1 + (1 | klasse), data = ml_data)
  
  if (treatment_level1 == TRUE){
    
    mlm_model <- lmer(leistung ~ uebung + (uebung || klasse), data = ml_data)
    
  } else{
    mlm_model <- lmer(leistung ~ uebung + (1 | klasse), data = ml_data)
    
  }
  
  # saving coefficients
  empirical_icc <- VarCorr(mlm_model_0)$klasse[1,1] / (VarCorr(mlm_model_0)$klasse[1,1] 
                                                       + sigma(mlm_model_0)^2)
  theoretical_icc <- sd_intercept^2 / (sd_intercept^2 + sd_error^2)
  
  
  
  beta_lm <- coef(lm_model)
  SE_lm <- coef(summary(lm_model))[,2]
  p_lm <- coef(summary(lm_model))[,4]
  
  # lq_lm <- anova(lm_model_0, lm_model, test = "LRT")
  # p_lq_lm <- lq_lm$`Pr(>Chi)`[2]
  
  
  
  beta_mlm <- fixef(mlm_model)
  SE_mlm <- coef(summary(mlm_model))[,2]
  p_mlm <- coef(summary(mlm_model))[,5]
  
  # lq_mlm <- anova(mlm_model_0, mlm_model, test = "LRT")
  # p_lq_mlm <- lq_mlm$`Pr(>Chisq)`[2]
  
  # saving all coefficients in a matrix
  coefs <- matrix(c(beta_lm, SE_lm, p_lm, empirical_icc, theoretical_icc, sd_intercept, sd_error, y10, "lm", 
                    beta_mlm, SE_mlm, p_mlm, empirical_icc, theoretical_icc, sd_intercept, sd_error, y10, "mlm"
  ), 
  ncol = 12, byrow = TRUE)
  
  return(coefs)
}


# Complete Simulation Studiy ----------------------------------------------
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
                             "p_value_treatment", "empirical_icc",
                             "theoretical_icc", "sd_intercept", "sd_error", "effect_treatment", 
                             "method")
  
  coef_models[,1:8] <- apply(coef_models[,1:8], 2, as.character)
  coef_models[,1:8] <- apply(coef_models[,1:8], 2, as.numeric)
  return(coef_models)
}

# Simulating Data ---------------------------------------------------------
icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
var_i <- c()

for(i in 1:length(icc)){
  var_i[i] <- (icc[i] * 1.72) / (1 - icc[i])
}

#test_lvl1_noeff <- simulation_study(sd_intercept = sqrt(var_i), sd_slope = 1, y00 = 15, y10 = 0, niter = 1000)
#test_lvl2_noeff <- simulation_study(sd_intercept = sqrt(var_i), sd_slope = 1, y00 = 15, y10 = 0, treatment_level1 = FALSE, niter = 1000)

test_lvl1 <- simulation_study(sd_intercept = sqrt(var_i), sd_slope = sqrt(0.06), sd_error = sqrt(1.72), 
                              y00 = 2.34, y10 = 0.12, 
                              niter = 1000)
test_lvl2 <- simulation_study(sd_intercept = sqrt(var_i), sd_slope = sqrt(0.06), sd_error = sqrt(1.72), 
                              y00 = 2.34, y10 = 0.12, 
                              treatment_level1 = FALSE, 
                              niter = 1000)

#saveRDS(test_lvl1_noeff, file = "test_lvl1_noeff")
#saveRDS(test_lvl2_noeff, file = "test_lvl2_noeff")
#saveRDS(test_lvl1, file = "test_lvl1")
#saveRDS(test_lvl2, file = "test_lvl2")

# Analysing Study ---------------------------------------------------------

test_lvl1 <- readRDS(file = "test_lvl1")
test_lvl2 <- readRDS(file = "test_lvl2")
test_lvl1_noeff <- readRDS(file = "test_lvl1_noeff")
test_lvl2_noeff <- readRDS(file = "test_lvl2_noeff")


# Boxplots for Coefficients for Treatment at both Levels
ggplot(data = test_lvl1, mapping = aes(y = beta_0, fill = method))+
  geom_boxplot() +
  facet_wrap(~ theoretical_icc) +
  labs(title = "Intercept Level 1")

ggplot(data = test_lvl1, mapping = aes(y = beta_treatment, fill = method))+
  geom_boxplot() +
  facet_wrap( ~ theoretical_icc) +
  labs(title = "Treatment Level 1")

ggplot(data = test_lvl2, mapping = aes(y = beta_0, fill = method))+
  geom_boxplot() +
  facet_wrap(~ theoretical_icc) +
  labs(title = "Intercept Level 2")

ggplot(data = test_lvl2, mapping = aes(y = beta_treatment, fill = method))+
  geom_boxplot() +
  facet_wrap(~ theoretical_icc) +
  labs(title = "Treatment Level 2")

# Boxplots for Standard Errors of Coefs for Treatment at both Levels

ggplot(data = test_lvl1, mapping = aes(y = SE_beta_0, fill = method))+
  geom_boxplot() +
  facet_wrap(~ theoretical_icc) +
  labs(title = "SE Intercept Level 1")

ggplot(data = test_lvl1, mapping = aes(y = SE_beta_treatment, fill = method))+
  geom_boxplot() +
  facet_wrap( ~ theoretical_icc) +
  labs(title = "SE Treatment Level 1")

ggplot(data = test_lvl2, mapping = aes(y = SE_beta_0, fill = method))+
  geom_boxplot() +
  facet_wrap(~ theoretical_icc) +
  labs(title = "SE Intercept Level 2")

ggplot(data = test_lvl2, mapping = aes(y = SE_beta_treatment, fill = method))+
  geom_boxplot() +
  facet_wrap( ~ theoretical_icc) +
  labs(title = "SE Treatment Level 2")


# Parameter Efficacy for Treatment at bot levels and for every ICC
mean_parameters <- function(df){
  icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
  intercept_mean_lm <- c()
  intercept_mean_mlm <- c()
  treatment_mean_lm <- c()
  treatment_mean_mlm <- c()
  
  for(i in 1:length(icc)){
    intercept_mean_lm[i] <- mean(df$beta_0[test_lvl1$method == "lm" & test_lvl1$theoretical_icc == icc[i]])
    intercept_mean_mlm[i] <- mean(df$beta_0[test_lvl1$method == "mlm" & test_lvl1$theoretical_icc == icc[i]])
    treatment_mean_lm[i] <- mean(df$beta_treatment[test_lvl1$method == "lm" & test_lvl1$theoretical_icc == icc[i]])
    treatment_mean_mlm[i] <- mean(df$beta_treatment[test_lvl1$method == "mlm" & test_lvl1$theoretical_icc == icc[i]])
  }
  
  methods <- rep(c("lm", "mlm"), each = 9)
  icc_df <- rep(icc, times = 2)
  
  temp_m <- matrix(c(intercept_mean_lm, intercept_mean_mlm, treatment_mean_lm, treatment_mean_mlm, methods, icc_df), ncol = 4)
  mean_dataframe <- data.frame(temp_m)
  colnames(mean_dataframe) <- c("intercept_mean", "treatment_mean", "method", "icc")
  mean_dataframe[,1:2] <- apply(mean_dataframe[,1:2], 2, as.character)
  mean_dataframe[,1:2] <- apply(mean_dataframe[,1:2], 2, as.numeric)
  
  return(mean_dataframe)
}

parameter_efficacy <- function(df){
  icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
  mean_par <- mean_parameters(df)
  
  intercept_efficacy <- mean_par$intercept_mean / 2.34
  treatment_efficacy <- mean_par$treatment_mean / 0.12
  
  methods <- rep(c("lm", "mlm"), each = 9)
  icc_df <- rep(icc, times = 2)
  
  temp_m <- matrix(c(intercept_efficacy, treatment_efficacy, methods, icc_df), ncol = 4)
  
  par_efficacy_df <- data.frame(temp_m)
  colnames(par_efficacy_df) <- c("intercept_efficacy", "treatment_efficacy", "method", "icc")
  par_efficacy_df[,1:2] <- apply(par_efficacy_df[,1:2], 2, as.character)
  par_efficacy_df[,1:2] <- apply(par_efficacy_df[,1:2], 2, as.numeric)
  
  return(par_efficacy_df)
}

parameter_efficacy_lvl1 <- parameter_efficacy(test_lvl1)
parameter_efficacy_lvl2 <- parameter_efficacy(test_lvl2)

ggplot(data = parameter_efficacy_lvl1, aes(y = intercept_efficacy, x = method, fill = method))+
  geom_col() +
  geom_hline(yintercept = 1, size = 1) +
  facet_grid(~ icc) + 
  labs(title = "Parameter Efficacy Intercept fo Treatment at Level 1")

ggplot(data = parameter_efficacy_lvl1, aes(y = treatment_efficacy, x = method, fill = method))+
  geom_col() +
  geom_hline(yintercept = 1, size = 1) +
  facet_grid(~ icc) +
  labs(title = "Parameter Efficacy Treatment fo Treatment at Level 1")

ggplot(data = parameter_efficacy_lvl2, aes(y = intercept_efficacy, x = method, fill = method))+
  geom_col() +
  geom_hline(yintercept = 1, size = 1) +
  facet_grid(~ icc) + 
  labs(title = "Parameter Efficacy Intercept fo Treatment at Level 2")

ggplot(data = parameter_efficacy_lvl2, aes(y = treatment_efficacy, x = method, fill = method))+
  geom_col() +
  geom_hline(yintercept = 1, size = 1) +
  facet_grid(~ icc) +
  labs(title = "Parameter Efficacy Treatment fo Treatment at Level 2")


# SE Efficacy for Treatment at both levels and for every ICC
mean_se <- function(df){
  icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
  se_0_mean_lm <- c()
  se_0_mean_mlm <- c()
  se_treatment_mean_lm <- c()
  se_treatment_mean_mlm <- c()
  
  for(i in 1:length(icc)){
  se_0_mean_lm[i] <-  mean(df$SE_beta_0[df$method == "lm"  & df$theoretical_icc == icc[i]])
  se_0_mean_mlm[i] <- mean(df$SE_beta_0[df$method == "mlm" & df$theoretical_icc == icc[i]])
  se_treatment_mean_lm[i] <-  mean(df$SE_beta_treatment[df$method == "lm"  & df$theoretical_icc == icc[i]])
  se_treatment_mean_mlm[i] <- mean(df$SE_beta_treatment[df$method == "mlm" & df$theoretical_icc == icc[i]])
  }
  
  methods <- rep(c("lm", "mlm"), each = 9)
  icc_df <- rep(icc, times = 2)
  
  temp_m <- matrix(c(se_0_mean_lm, se_0_mean_mlm, se_treatment_mean_lm, se_treatment_mean_mlm, methods, icc_df), ncol = 4)
  mean_se_dataframe <- data.frame(temp_m)
  colnames(mean_se_dataframe) <- c("mean_se_intercept", "mean_se_treatment", "method", "icc")
  mean_se_dataframe[,1:2] <- apply(mean_se_dataframe[,1:2], 2, as.character)
  mean_se_dataframe[,1:2] <- apply(mean_se_dataframe[,1:2], 2, as.numeric)
  
  return(mean_se_dataframe)
}

sd_coefs <- function(df){
  icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
  sd_0_lm <- c()
  sd_0_mlm <- c()
  sd_treatment_lm <- c()
  sd_treatment_mlm <- c()
  
  for(i in 1:length(icc)){
    sd_0_lm[i] <- sd(df$beta_0[df$method == "lm" & df$theoretical_icc == icc[i]])
    sd_0_mlm[i] <- sd(df$beta_0[df$method == "mlm" & df$theoretical_icc == icc[i]])
    sd_treatment_lm[i] <- sd(df$beta_treatment[df$method == "lm" & df$theoretical_icc == icc[i]])
    sd_treatment_mlm[i] <- sd(df$beta_treatment[df$method == "mlm" & df$theoretical_icc == icc[i]])
  }
  
  methods <- rep(c("lm", "mlm"), each = 9)
  icc_df <- rep(icc, times = 2)
  
  temp_m <- matrix(c(sd_0_lm, sd_0_mlm, sd_treatment_lm, sd_treatment_mlm, methods, icc_df), ncol = 4)
  mean_sd_dataframe <- data.frame(temp_m)
  colnames(mean_sd_dataframe) <- c("sd_intercept", "sd_treatment", "method", "icc")
  mean_sd_dataframe[,1:2] <- apply(mean_sd_dataframe[,1:2], 2, as.character)
  mean_sd_dataframe[,1:2] <- apply(mean_sd_dataframe[,1:2], 2, as.numeric)
  
  return(mean_sd_dataframe)
}

se_efficacy <- function(df){
  icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)

se_means <- mean_se(df)
coefs_sd <- sd_coefs(df)

se_efficacy_intercept <- se_means$mean_se_intercept / coefs_sd$sd_intercept
se_efficacy_treatment <- se_means$mean_se_treatment / coefs_sd$sd_treatment

methods <- rep(c("lm", "mlm"), each = 9)
icc_df <- rep(icc, times = 2)

temp_m <- matrix(c(se_efficacy_intercept, se_efficacy_treatment, methods, icc_df), ncol = 4)

se_efficacy_df <- data.frame(temp_m)
colnames(se_efficacy_df) <- c("intercept_efficacy", "treatment_efficacy", "method", "icc")
se_efficacy_df[,1:2] <- apply(se_efficacy_df[,1:2], 2, as.character)
se_efficacy_df[,1:2] <- apply(se_efficacy_df[,1:2], 2, as.numeric)

return(se_efficacy_df)
}


mean_se_lvl1 <- mean_se(test_lvl1)
mean_se_lvl2 <- mean_se(test_lvl2)
sd_lvl1 <- sd_coefs(test_lvl1)
sd_lvl2 <- sd_coefs(test_lvl2)

se_efficacy_lvl1 <- se_efficacy(test_lvl1)
se_efficacy_lvl2 <- se_efficacy(test_lvl2)

ggplot(data = se_efficacy_lvl1, aes(y = intercept_efficacy, x = method, fill = method))+
  geom_col() +
  geom_hline(yintercept = 1, size = 1) +
  facet_grid(~ icc) + 
  labs(title = "SE Efficacy Intercept fo Treatment at Level 1")

ggplot(data = se_efficacy_lvl1, aes(y = treatment_efficacy, x = method, fill = method))+
  geom_col() +
  geom_hline(yintercept = 1, size = 1) +
  facet_grid(~ icc) +
  labs(title = "SE Efficacy Treatment fo Treatment at Level 1")

ggplot(data = se_efficacy_lvl2, aes(y = intercept_efficacy, x = method, fill = method))+
  geom_col() +
  geom_hline(yintercept = 1, size = 1) +
  facet_grid(~ icc) +
  labs(title = "SE Efficacy Intercept fo Treatment at Level 2")

ggplot(data = se_efficacy_lvl2, aes(y = treatment_efficacy, x = method, fill = method))+
  geom_col() +
  geom_hline(yintercept = 1, size = 1) +
  facet_grid(~ icc) +
  labs(title = "SE Efficacy Treatment fo Treatment at Level 2")

# Power of Models
power_model <- function(df){
  icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
  power_intercept_lm <- c()
  power_intercept_mlm <- c()
  power_treatment_lm <- c()
  power_treatment_mlm <- c()
  
  for(i in 1:length(icc)){
    power_intercept_lm[i] <- length(df$p_value_0[df$method == "lm" & df$theoretical_icc == icc[i] &
                                                           df$p_value_0 < .05])/1000
    power_intercept_mlm[i] <- length(df$p_value_0[df$method == "mlm" & df$theoretical_icc == icc[i] &
                                                            df$p_value_0 < .05])/1000
    power_treatment_lm[i] <- length(df$p_value_treatment[df$method == "lm" & df$theoretical_icc == icc[i] &
                                              df$p_value_treatment < .05])/1000
    power_treatment_mlm[i] <- length(df$p_value_treatment[df$method == "mlm" & df$theoretical_icc == icc[i] &
                                                  df$p_value_treatment < .05])/1000
    
  }
  
  methods <- rep(c("lm", "mlm"), each = 9)
  icc_df <- rep(icc, times = 2)
  
  temp_m <- matrix(c(power_intercept_lm, power_intercept_mlm, power_treatment_lm, power_treatment_mlm, methods, icc_df), ncol = 4)
  power_dataframe <- data.frame(temp_m)
  colnames(power_dataframe) <- c("power_intercept", "power_treatment", "method", "icc")
  power_dataframe[,1:2] <- apply(power_dataframe[,1:2], 2, as.character)
  power_dataframe[,1:2] <- apply(power_dataframe[,1:2], 2, as.numeric)
  
  return(power_dataframe)
}

power_lvl1 <- power_model(test_lvl1)
power_lvl2 <- power_model(test_lvl2)

ggplot(data = power_lvl1, mapping = aes(y = power_intercept, x = method,  fill = method))+
  geom_col() +
  facet_grid(~ icc) +
  labs(title = "Power Intercept Level 1")

ggplot(data = power_lvl1, mapping = aes(y = power_treatment, x = method, fill = method))+
  geom_col() +
  facet_grid(~ icc) +
  labs(title = "Power Treatment Level 1")

ggplot(data = power_lvl2, mapping = aes(y = power_intercept, x = method, fill = method))+
  geom_col() +
  facet_grid(~ icc) +
  labs(title = "Power Intercept Level 2")

ggplot(data = power_lvl2, mapping = aes(y = power_treatment, x = method, fill = method))+
  geom_col() +
  facet_grid(~ icc) +
  labs(title = "Power Treatment Level 2")


                                    
