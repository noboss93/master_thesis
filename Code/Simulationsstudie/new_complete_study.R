### Simulation Study Multilevel Analysis ###
# Libraries ---------------------------------------------------------------
library(lme4)
library(lmerTest)
library(ggplot2)

# Data Generating Process -------------------------------------------------
dgp <- function(nschueler = 50, 
                        nklassen = 300, 
                        sd_intercept = 3, 
                        sd_error = 5,
                        y00 = 15, 
                        y10 = 0.35,
                        treatment_level1 = TRUE){
  
  # Creating Variables
  n <- nschueler * nklassen
  error <- rnorm(n, mean = 0, sd = sd_error)
  klasse <- rep(1:nklassen, each = nschueler)
  eff_klasse <- rnorm(nklassen, mean = 0, sd = sd_intercept)
  random_intercept <- rep(eff_klasse, each = nschueler)
  
  # Calculating individual leistung score
  if (treatment_level1 == TRUE){
    uebung <- rep(c(-1,1), each = nschueler/2, times = nklassen)
    
    leistung <- numeric(n)
    for (i in 1:n){
      leistung[i] <- y00 + y10 * uebung[i] + random_intercept[i] + error[i]
    }
    
  } else{
    uebung <- rep(c(-1,1), each = nschueler, times = nklassen/2)
    
    leistung <- numeric(n)
    for (i in 1:n){
      leistung[i] <- y00 + y10 * uebung[i] + random_intercept[i] + error[i]
    }
  }
  
  # Creating dataframe
  klasse <- as.factor(klasse)
  
  ml_data <- data.frame(klasse, uebung, leistung)
  
  return(ml_data)
}

# One Simulation ----------------------------------------------------------
one_simulation <- function(nschueler = 50, 
                           nklassen = 300, 
                           sd_intercept = 3, 
                           sd_error = 5,
                           y00 = 15, 
                           y10 = 0.3,
                           treatment_level1 = TRUE){
  
  # generating one data set
  ml_data <- dgp(nschueler = nschueler, 
                         nklassen = nklassen, 
                         sd_intercept = sd_intercept, 
                         sd_error = sd_error, 
                         y00 = y00, 
                         y10 = y10,
                         treatment_level1 = treatment_level1)
  
  # calculating model
  lm_model <- lm(leistung ~  uebung, data = ml_data)
  
  mlm_model <- lmer(leistung ~ uebung + (1 | klasse), data = ml_data)
    
  # saving coefficients
  icc <- sd_intercept^2 / (sd_intercept^2 + sd_error^2)
  
  beta0_lm <- summary(lm_model)$coefficients[1,1]
  beta1_lm <- summary(lm_model)$coefficients[2,1]
  SE_beta0_lm <- summary(lm_model)$coefficients[1,2]
  SE_beta1_lm <- summary(lm_model)$coefficients[2,2]
  p_beta0_lm <- summary(lm_model)$coefficients[1,4]
  p_beta1_lm <- summary(lm_model)$coefficients[2,4]
  
  lm_coefs <- c(beta0_lm, SE_beta0_lm, p_beta0_lm, beta1_lm, SE_beta1_lm, p_beta1_lm, icc, "lm")

  beta0_mlm <- summary(mlm_model)$coefficients[1,1]
  beta1_mlm <- summary(mlm_model)$coefficients[2,1]
  SE_beta0_mlm <- summary(mlm_model)$coefficients[1,2]
  SE_beta1_mlm <- summary(mlm_model)$coefficients[2,2]
  p_beta0_mlm <- summary(mlm_model)$coefficients[1,5]
  p_beta1_mlm <- summary(mlm_model)$coefficients[2,5]
  
  mlm_coefs <- c(beta0_mlm, SE_beta0_mlm, p_beta0_mlm, beta1_mlm, SE_beta1_mlm, p_beta1_mlm, icc, "mlm")
  
  # saving all coefficients in a matrix
  coefs <- matrix(c(lm_coefs, mlm_coefs), byrow = TRUE, ncol = length(mlm_coefs))
  
  return(coefs)
}

# Complete Simulation Studiy ----------------------------------------------
simulation_study <- function(nschueler = 50, 
                             nklassen = 300, 
                             sd_intercept = 2, 
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
  colnames(coef_models) <- c("beta_intercept", "SE_beta_intercept", "p_value_intercept", 
                             "beta_treatment", "SE_beta_treatment", "p_value_treatment", 
                             "icc", "method")
  
  coef_models[,1:6] <- apply(coef_models[,1:6], 2, as.character)
  coef_models[,1:6] <- apply(coef_models[,1:6], 2, as.numeric)
  return(coef_models)
}

# Simulating Data ---------------------------------------------------------
icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
var_i <- c()

for(i in 1:length(icc)){
  var_i[i] <- (icc[i] * 1.72) / (1 - icc[i])
}

# simstudy_lvl1 <- simulation_study(nschueler = 8, nklassen = 10, sd_intercept = sqrt(var_i), sd_error = sqrt(1.72), 
#                               y00 = 2.34, y10 = 0.12, 
#                               niter = 100)
# simstudy_lvl2_small <- simulation_study(nschueler = 12, nklassen = 70, sd_intercept = sqrt(var_i), sd_error = sqrt(1.72), 
#                              y00 = 2.34, y10 = 0.12, 
#                              treatment_level1 = FALSE, 
#                              niter = 1000)

#saveRDS(simstudy_lvl1, file = "simstudy_lvl1")
#saveRDS(simstudy_lvl2, file = "simstudy_lvl2")
#saveRDS(simstudy_lvl1_small, file = "simstudy_lvl1_small")
#saveRDS(simstudy_lvl2_small, file = "simstudy_lvl2_small")


# Analysing Study ---------------------------------------------------------

test_lvl1 <- readRDS(file = "simstudy_lvl1") # nschueler = 50, nklassen = 300
test_lvl2 <- readRDS(file = "simstudy_lvl2")
test_lvl1_small <- readRDS(file = "simstudy_lvl1_small") # nschueler = 12, nklassen = 70
test_lvl2_small <- readRDS(file = "simstudy_lvl2_small")

# Parameter Efficacy for Treatment at bot levels and for every ICC
parameter_spread <- function(df){
  intercept_spread_lm <- c()
  intercept_spread_mlm <- c()
  treatment_spread_lm <- c()
  treatment_spread_mlm <- c()
  icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
  
  for(i in 1:length(icc)){
    intercept_spread_lm[i] <- var((df$beta_0[df$method == "lm" & df$icc == icc[i]] - 2.23)/2.23)
    intercept_spread_mlm[i] <- var((df$beta_0[df$method == "mlm" & df$icc == icc[i]] - 2.23)/2.23)
    treatment_spread_lm[i] <- var((df$beta_treatment[df$method == "lm" & df$icc == icc[i]] - 0.12)/0.12)
    treatment_spread_mlm[i] <- var((df$beta_treatment[df$method == "mlm" & df$icc == icc[i]] - 0.12)/0.12)
  }
  methods <- rep(c("lm", "mlm"), each = length(icc))
  icc_df <- rep(icc, times = 2)
  
  temp_m <- matrix(c(intercept_spread_lm, intercept_spread_mlm, treatment_spread_lm, treatment_spread_mlm, methods, icc_df), ncol = 4)
  spread_dataframe <- data.frame(temp_m)
  colnames(spread_dataframe) <- c("intercept_var", "treatment_var", "method", "icc")
  spread_dataframe[,1:2] <- apply(spread_dataframe[,1:2], 2, as.character)
  spread_dataframe[,1:2] <- apply(spread_dataframe[,1:2], 2, as.numeric)
  
  return(spread_dataframe)
}

parameter_spread(test_lvl1)

mean_parameters <- function(df){
  intercept_mean_lm <- c()
  intercept_mean_mlm <- c()
  treatment_mean_lm <- c()
  treatment_mean_mlm <- c()
  icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
  
  for(i in 1:length(icc)){
    intercept_mean_lm[i] <- mean(df$beta_0[df$method == "lm" & df$icc == icc[i]])
    intercept_mean_mlm[i] <- mean(df$beta_0[df$method == "mlm" & df$icc == icc[i]])
    treatment_mean_lm[i] <- mean(df$beta_treatment[df$method == "lm" & df$icc == icc[i]])
    treatment_mean_mlm[i] <- mean(df$beta_treatment[df$method == "mlm" & df$icc == icc[i]])
  }
  
  methods <- rep(c("lm", "mlm"), each = length(icc))
  icc_df <- rep(icc, times = 2)
  
  temp_m <- matrix(c(intercept_mean_lm, intercept_mean_mlm, treatment_mean_lm, treatment_mean_mlm, methods, icc_df), ncol = 4)
  mean_dataframe <- data.frame(temp_m)
  colnames(mean_dataframe) <- c("intercept_mean", "treatment_mean", "method", "icc")
  mean_dataframe[,1:2] <- apply(mean_dataframe[,1:2], 2, as.character)
  mean_dataframe[,1:2] <- apply(mean_dataframe[,1:2], 2, as.numeric)
  
  return(mean_dataframe)
}

mean_parameters(test_lvl1)

parameter_efficacy <- function(df){
  icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
  mean_par <- mean_parameters(df)
  par_var <- parameter_spread(df)
  
  intercept_efficacy <- (mean_par$intercept_mean - 2.34) / 2.34
  treatment_efficacy <- (mean_par$treatment_mean - 0.12) / 0.12
  
  methods <- rep(c("lm", "mlm"), each = length(icc))
  icc_df <- rep(icc, times = 2)
  
  temp_m <- matrix(c(icc_df, intercept_efficacy, par_var$intercept_var, treatment_efficacy, 
                     par_var$treatment_var, methods), ncol = 6)
  
  par_efficacy_df <- data.frame(temp_m)
  colnames(par_efficacy_df) <- c("icc", "intercept_efficacy", "intercept_var", "treatment_efficacy", "treatment_var", "method")
  par_efficacy_df[,2:5] <- apply(par_efficacy_df[,2:5], 2, as.character)
  par_efficacy_df[,2:5] <- apply(par_efficacy_df[,2:5], 2, as.numeric)
  
  return(par_efficacy_df)
}
# Preparing Dataframe for table
parameter_efficacy_lvl1 <- parameter_efficacy(test_lvl1)
parameter_efficacy_lvl2 <- parameter_efficacy(test_lvl2)
parameter_lvl1 <- data.frame(parameter_efficacy_lvl1[1:9,], parameter_efficacy_lvl1[10:18,2:6])
parameter_lvl2 <- data.frame(parameter_efficacy_lvl2[1:9,], parameter_efficacy_lvl2[10:18,2:6])
parameter_full <- data.frame(parameter_lvl1, parameter_lvl2[,2:11])
xtable(parameter_full)


ggplot(data = parameter_efficacy_lvl1, aes(y = intercept_efficacy, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 1)+
  geom_hline(yintercept = 1.1, linetype = "dashed")+
  geom_hline(yintercept = 0.9, linetype = "dashed")+
  labs(title = "Parameter Efficacy Intercept fo Treatment at Level 1")

ggplot(data = parameter_efficacy_lvl1, aes(y = treatment_efficacy, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 1)+
  geom_hline(yintercept = 1.1, linetype = "dashed")+
  geom_hline(yintercept = 0.9, linetype = "dashed")+
  labs(title = "Parameter Efficacy Treatment fo Treatment at Level 1")

ggplot(data = parameter_efficacy_lvl2, aes(y = intercept_efficacy, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 1)+
  geom_hline(yintercept = 1.1, linetype = "dashed")+
  geom_hline(yintercept = 0.9, linetype = "dashed")+
  labs(title = "Parameter Efficacy Intercept fo Treatment at Level 2")

ggplot(data = parameter_efficacy_lvl2, aes(y = treatment_efficacy, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 1)+
  geom_hline(yintercept = 1.1, linetype = "dashed")+
  geom_hline(yintercept = 0.9, linetype = "dashed")+
  labs(title = "Parameter Efficacy Treatment fo Treatment at Level 2")


# SE Efficacy for Treatment at both levels and for every ICC
mean_se <- function(df){
  icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
  se_0_mean_lm <- c()
  se_0_mean_mlm <- c()
  se_treatment_mean_lm <- c()
  se_treatment_mean_mlm <- c()
  
  for(i in 1:length(icc)){
  se_0_mean_lm[i] <-  mean(df$SE_beta_0[df$method == "lm"  & df$icc == icc[i]])
  se_0_mean_mlm[i] <- mean(df$SE_beta_0[df$method == "mlm" & df$icc == icc[i]])
  se_treatment_mean_lm[i] <-  mean(df$SE_beta_treatment[df$method == "lm"  & df$icc == icc[i]])
  se_treatment_mean_mlm[i] <- mean(df$SE_beta_treatment[df$method == "mlm" & df$icc == icc[i]])
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
    sd_0_lm[i] <- sd(df$beta_0[df$method == "lm" & df$icc == icc[i]])
    sd_0_mlm[i] <- sd(df$beta_0[df$method == "mlm" & df$icc == icc[i]])
    sd_treatment_lm[i] <- sd(df$beta_treatment[df$method == "lm" & df$icc == icc[i]])
    sd_treatment_mlm[i] <- sd(df$beta_treatment[df$method == "mlm" & df$icc == icc[i]])
  }
  
  methods <- rep(c("lm", "mlm"), each = length(icc))
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

methods <- rep(c("lm", "mlm"), each = length(icc))
icc_df <- rep(icc, times = 2)

temp_m <- matrix(c(se_efficacy_intercept, se_efficacy_treatment, methods, icc_df), ncol = 4)

se_efficacy_df <- data.frame(temp_m)
colnames(se_efficacy_df) <- c("intercept_efficacy", "treatment_efficacy", "method", "icc")
se_efficacy_df[,1:2] <- apply(se_efficacy_df[,1:2], 2, as.character)
se_efficacy_df[,1:2] <- apply(se_efficacy_df[,1:2], 2, as.numeric)

return(se_efficacy_df)
}

se_efficacy_lvl1 <- se_efficacy(test_lvl1)
se_efficacy_lvl2 <- se_efficacy(test_lvl2)

uzh_colors <- c("#3353B7", "#E38052")


ggplot(data = se_efficacy_lvl1, aes(y = intercept_efficacy, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 1)+
  geom_hline(yintercept = 1.1, linetype = "dashed")+
  geom_hline(yintercept = 0.9, linetype = "dashed")+
  scale_fill_manual(values = uzh_colors) + 
  scale_y_continuous(breaks=seq(0,1.2, 0.1)) + 
  labs(title = "SE Efficacy Intercept fo Treatment at Level 1")

ggplot(data = se_efficacy_lvl1, aes(y = treatment_efficacy, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 1)+
  geom_hline(yintercept = 1.1, linetype = "dashed")+
  geom_hline(yintercept = 0.9, linetype = "dashed")+
  scale_fill_manual(values = uzh_colors) + 
  scale_y_continuous(breaks=seq(0,2, 0.1)) +
  labs(title = "SE Efficacy Treatment fo Treatment at Level 1")

ggplot(data = se_efficacy_lvl2, aes(y = intercept_efficacy, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 1)+
  geom_hline(yintercept = 1.1, linetype = "dashed")+
  geom_hline(yintercept = 0.9, linetype = "dashed")+
  scale_fill_manual(values = uzh_colors) + 
  scale_y_continuous(breaks=seq(0,1.2, 0.1)) +
  labs(title = "SE Efficacy Intercept fo Treatment at Level 2")

ggplot(data = se_efficacy_lvl2, aes(y = treatment_efficacy, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 1)+
  geom_hline(yintercept = 1.1, linetype = "dashed")+
  geom_hline(yintercept = 0.9, linetype = "dashed")+
  scale_fill_manual(values = uzh_colors) + 
  scale_y_continuous(breaks=seq(0,1.2, 0.1)) +
  labs(title = "SE Efficacy Treatment fo Treatment at Level 2")

# Power of Models
power_model <- function(df){
  icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
  power_intercept_lm <- c()
  power_intercept_mlm <- c()
  power_treatment_lm <- c()
  power_treatment_mlm <- c()
  
  for(i in 1:length(icc)){
    power_intercept_lm[i] <- length(df$p_value_0[df$method == "lm" & df$icc == icc[i] & df$p_value_0 < .05])/1000
    power_intercept_mlm[i] <- length(df$p_value_0[df$method == "mlm" & df$icc == icc[i] & df$p_value_0 < .05])/1000
    power_treatment_lm[i] <- length(df$p_value_treatment[df$method == "lm" & df$icc == icc[i] & df$p_value_treatment < .05])/1000
    power_treatment_mlm[i] <- length(df$p_value_treatment[df$method == "mlm" & df$icc == icc[i] & df$p_value_treatment < .05])/1000
    
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

se_eff_lvl1_small <- se_efficacy(test_lvl1_small)
se_eff_lvl2_small <- se_efficacy(test_lvl2_small)

power_lvl1_small <- power_model(test_lvl1_small)
power_lvl2_small <- power_model(test_lvl2_small)


ggplot(data = se_eff_lvl1_small, mapping = aes(y = treatment_efficacy, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 1)+
  geom_hline(yintercept = 1.1, linetype = "dashed")+
  geom_hline(yintercept = 0.9, linetype = "dashed")+
  scale_fill_manual(values = uzh_colors) +
  scale_y_continuous(breaks=seq(0,2, 0.1)) +
  labs(title = "SE Efficacy Treatment Level 1 Small")

ggplot(data = power_lvl1_small, mapping = aes(y = power_treatment, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  scale_fill_manual(values = uzh_colors) +
  scale_y_continuous(breaks=seq(0,2, 0.1)) +
  labs(title = "Power Treatment Level 1")

ggplot(data = se_eff_lvl2_small, mapping = aes(y = treatment_efficacy, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 1)+
  geom_hline(yintercept = 1.1, linetype = "dashed")+
  geom_hline(yintercept = 0.9, linetype = "dashed")+
  scale_fill_manual(values = uzh_colors) + 
  scale_y_continuous(breaks=seq(0,2, 0.1)) +
  labs(title = "SE Efficacy Treatment Level 2 Small")

ggplot(data = power_lvl2_small, mapping = aes(y = power_treatment, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  scale_fill_manual(values = uzh_colors) +
  scale_y_continuous(breaks=seq(0,2, 0.1)) +
  labs(title = "Power Treatment Level 2 ")

