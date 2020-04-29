library(xtable)
library(MASS)
library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(gridExtra)
library(gridBase)
library(extrafont)
library(viridis)
uzh_colors <- c("#3353B7", "#E38052")
paper_colors <- c("darkgrey", "#B01111")

# SE Plots ----------------------------------------------------------------
simstudy_lvl1 <- readRDS(file = "../Code/Simulationsstudie/simstudy_lvl1")
simstudy_lvl2 <- readRDS(file = "../Code/Simulationsstudie/simstudy_lvl2")
simstudy_lvl1_small <- readRDS(file = "../Code/Simulationsstudie/simstudy_lvl1_small")
simstudy_lvl2_small <- readRDS(file = "../Code/Simulationsstudie/simstudy_lvl2_small")
simstudy_lvl1_small_h0 <- readRDS(file = "../Code/Simulationsstudie/simstudy_lvl1_small_h0")
simstudy_lvl2_small_h0 <- readRDS(file = "../Code/Simulationsstudie/simstudy_lvl2_small_h0")



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
  
  se_efficacy_intercept <- (se_means$mean_se_intercept - coefs_sd$sd_intercept) / coefs_sd$sd_intercept
  se_efficacy_treatment <- (se_means$mean_se_treatment - coefs_sd$sd_treatment) / coefs_sd$sd_treatment
  
  methods <- rep(c("lm", "mlm"), each = length(icc))
  icc_df <- rep(icc, times = 2)
  
  temp_m <- matrix(c(se_efficacy_intercept, se_efficacy_treatment, methods, icc_df), ncol = 4)
  
  se_efficacy_df <- data.frame(temp_m)
  colnames(se_efficacy_df) <- c("intercept_efficacy", "treatment_efficacy", "method", "icc")
  se_efficacy_df[,1:2] <- apply(se_efficacy_df[,1:2], 2, as.character)
  se_efficacy_df[,1:2] <- apply(se_efficacy_df[,1:2], 2, as.numeric)
  
  return(se_efficacy_df)
}

se_efficacy_lvl1 <- se_efficacy(simstudy_lvl1)
se_efficacy_lvl2 <- se_efficacy(simstudy_lvl2)

se_table <- data.frame("icc" = c(0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.40, 0.50),
                         "LM" = se_efficacy_lvl1$intercept_efficacy[1:9],
                         "HLM" = se_efficacy_lvl1$intercept_efficacy[10:18],
                         "LM" = se_efficacy_lvl2$intercept_efficacy[1:9],
                         "HLM" = se_efficacy_lvl2$intercept_efficacy[10:18],
                       "LM" = se_efficacy_lvl1$treatment_efficacy[1:9],
                       "HLM" = se_efficacy_lvl1$treatment_efficacy[10:18],
                       "LM" = se_efficacy_lvl2$treatment_efficacy[1:9],
                       "HLM" = se_efficacy_lvl2$treatment_efficacy[10:18])




uzh_colors <- c("#3353B7", "#E38052")
paper_colors <- c("darkgrey", "#B01111")


# scale to 0
ggplot(data = se_efficacy_lvl1, aes(y = treatment_efficacy, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.1, linetype = "dashed")+
  geom_hline(yintercept = -0.1, linetype = "dashed")+
  geom_hline(yintercept = 0.05, linetype = "dotted")+
  geom_hline(yintercept = -0.05, linetype = "dotted")+
  scale_fill_manual(values = paper_colors, name = "Methode", labels = c("LM", "HLM")) + 
  scale_y_continuous(breaks=seq(-1,1, 0.05)) +
  theme_gray(base_size = 15) +
  labs(title = "SE Genauigkeit Design 1")+
  xlab("IKK")+
  ylab("SE Genauigkeit")

ggplot(data = se_efficacy_lvl2, aes(y = treatment_efficacy, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.1, linetype = "dashed")+
  geom_hline(yintercept = -0.1, linetype = "dashed")+
  geom_hline(yintercept = 0.05, linetype = "dotted")+
  geom_hline(yintercept = -0.05, linetype = "dotted")+
  scale_fill_manual(values = paper_colors, name = "Methode", labels = c("LM", "HLM")) + 
  scale_y_continuous(breaks=seq(-1,1, 0.05)) +
  theme_gray(base_size = 15) +
  labs(title = "SE Genauigkeit Design 2")+
  xlab("IKK")+
  ylab("SE Genauigkeit")

# scale to 1
se_efficacy_1 <- function(df){
  icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
  
  se_means <- mean_se(df)
  coefs_sd <- sd_coefs(df)
  
  se_efficacy_intercept <- (se_means$mean_se_intercept) / coefs_sd$sd_intercept
  se_efficacy_treatment <- (se_means$mean_se_treatment) / coefs_sd$sd_treatment
  
  methods <- rep(c("lm", "mlm"), each = length(icc))
  icc_df <- rep(icc, times = 2)
  
  temp_m <- matrix(c(se_efficacy_intercept, se_efficacy_treatment, methods, icc_df), ncol = 4)
  
  se_efficacy_df <- data.frame(temp_m)
  colnames(se_efficacy_df) <- c("intercept_efficacy", "treatment_efficacy", "method", "icc")
  se_efficacy_df[,1:2] <- apply(se_efficacy_df[,1:2], 2, as.character)
  se_efficacy_df[,1:2] <- apply(se_efficacy_df[,1:2], 2, as.numeric)
  
  return(se_efficacy_df)
}
se_efficacy_lvl1 <- se_efficacy_1(simstudy_lvl1)
se_efficacy_lvl2 <- se_efficacy_1(simstudy_lvl2)

ggplot(data = se_efficacy_lvl1, aes(y = treatment_efficacy, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 1)+
  geom_hline(yintercept = 1.1, linetype = "dashed")+
  geom_hline(yintercept = 0.9, linetype = "dashed")+
  geom_hline(yintercept = 1.05, linetype = "dotted")+
  geom_hline(yintercept = 0.95, linetype = "dotted")+
  scale_fill_manual(values = paper_colors, name = "Methode", labels = c("LM", "HLM")) + 
  scale_y_continuous(breaks=seq(0,2, 0.05)) +
  theme_gray(base_size = 15) +
  labs(title = "SE Genauigkeit Design 1")+
  xlab("IKK")+
  ylab("SE Genauigkeit")

ggplot(data = se_efficacy_lvl2, aes(y = treatment_efficacy, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 1)+
  geom_hline(yintercept = 1.1, linetype = "dashed")+
  geom_hline(yintercept = 0.9, linetype = "dashed")+
  geom_hline(yintercept = 1.05, linetype = "dotted")+
  geom_hline(yintercept = 0.95, linetype = "dotted")+
  scale_fill_manual(values = paper_colors, name = "Methode", labels = c("LM", "HLM")) + 
  scale_y_continuous(breaks=seq(0,2, 0.05)) +
  theme_gray(base_size = 15) +
  labs(title = "SE Genauigkeit Design 2")+
  xlab("IKK")+
  ylab("SE Genauigkeit")

# power
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

power_lvl1_small <- power_model(simstudy_lvl1_small)
power_lvl2_small <- power_model(simstudy_lvl2_small)

plvl1 <- ggplot(data = power_lvl1_small, mapping = aes(y = power_treatment, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  scale_fill_manual(values = paper_colors, name = "Methode", labels = c("LM", "HLM")) +
  scale_y_continuous(breaks=seq(0,1, 0.05), limits = c(0,1)) +
  geom_hline(yintercept = 0.8, linetype = "dashed")+
  theme_gray(base_size = 15) +
  theme(legend.position = "none")+
  labs(title = "Power bei einer Intervention auf Level-1")+
  xlab("IKK")+
  ylab("Power")

plvl2 <- ggplot(data = power_lvl2_small, mapping = aes(y = power_treatment, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  scale_fill_manual(values = paper_colors, name = "Methode", labels = c("LM", "HLM")) +
  scale_y_continuous(breaks=seq(0,1, 0.05), limits = c(0,1)) +
  geom_hline(yintercept = 0.8, linetype = "dashed")+
  theme_gray(base_size = 15) +
  theme(axis.title.y = element_blank(), legend.position = c(0.89,0.89))+
  labs(title = "Power bei einer Intervention auf Level-2")+
  xlab("IKK")

grid.arrange(plvl1, plvl2, nrow = 1)

type1error_lvl1 <- power_model(simstudy_lvl1_small_h0)
type1error_lvl2 <- power_model(simstudy_lvl2_small_h0)

ggplot(data = type1error_lvl1, mapping = aes(y = power_treatment, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  scale_fill_manual(values = paper_colors, name = "Methode", labels = c("LM", "HLM")) +
  scale_y_continuous(breaks=seq(0,1, 0.05), limits = c(0,1)) +
  geom_hline(yintercept = 0.05, linetype = "dashed")+
  theme_gray(base_size = 15) +
  theme(legend.position = "none")+
  labs(title = "Typ 1 Fehler Rate bei einer Intervention auf Level-1")+
  xlab("IKK")+
  ylab("Typ 1 Fehler")

ggplot(data = type1error_lvl2, mapping = aes(y = power_treatment, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  scale_fill_manual(values = paper_colors, name = "Methode", labels = c("LM", "HLM")) +
  scale_y_continuous(breaks=seq(0,1, 0.05), limits = c(0,1)) +
  geom_hline(yintercept = 0.05, linetype = "dashed")+
  theme_gray(base_size = 15) +
  labs(title = "Typ 1 Fehler Rate bei einer Intervention auf Level-2")+
  xlab("IKK")+
  ylab("Typ 1 Fehler")


#table
powertable <- data.frame("icc" = c(0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.40, 0.50),
                          "LM" = power_lvl1_small$power_treatment[1:9],
                          "HLM" = power_lvl1_small$power_treatment[10:18],
                          "LM" = power_lvl2_small$power_treatment[1:9],
                          "HLM" = power_lvl2_small$power_treatment[10:18],
                         "LM" = type1error_lvl1$power_treatment[1:9],
                         "HLM" = type1error_lvl1$power_treatment[10:18],
                         "LM" = type1error_lvl2$power_treatment[1:9],
                         "HLM" = type1error_lvl2$power_treatment[10:18])

powertable <- round(powertable, digits = 2)


sd(powertable$LM)
sd(powertable$HLM)
