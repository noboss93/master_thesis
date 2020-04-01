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


# Table Old ---------------------------------------------------------------


# \begin{table}[ht!]
# \centering
# \caption{Relative Abweichung der geschätzten Regressionskoeffizienten und deren Varianz für lineare und hierarchisch lineare Modelle in allen Simulationsbedingungen.}
# \begin{tabular}{ccccccccc}
# \toprule 
# & \multicolumn{4}{c}{Design 1} \\
# \cmidrule(lr){2-5} 
# & \multicolumn{2}{c}{LM} & \multicolumn{2}{c}{HLM}  \\
# \cmidrule(lr){2-3} \cmidrule(lr){4-5} 
# IKK 
# & $\Delta\widehat{\gamma}_{00}$ ($\sigma^2_{\widehat{\gamma}_{00}}$) 
# & $\Delta\widehat{\gamma}_{10}$ ($\sigma^2_{\widehat{\gamma}_{10}}$)
# & $\Delta\widehat{\gamma}_{00}$ ($\sigma^2_{\widehat{\gamma}_{00}}$)
# & $\Delta\widehat{\gamma}_{10}$ ($\sigma^2_{\widehat{\gamma}_{10}}$) \\ 
# \midrule
# 0 & -0.00032 (0.00013) & -0.00475 (0.00012) & -0.00032 (0.00013) & -0.00475 (0.00012)    	\\ 
# 0.05 & -0.00021 (0.00041) & 0.00255 (0.00011) & -0.00021 (0.00041) & 0.00255 (0.00011)    \\ 
# 0.1 & -0.00078 (0.00078) & 0.00145 (0.00011) & -0.00078 (0.00078) & 0.00145 (0.00011)  	 	\\ 
# 0.15 & -0.00031 (0.00111) & 0.00491 (0.00012) & -0.00031 (0.00111) & 0.00491 (0.00012)    \\ 
# 0.2 & 0.00007 (0.00148) & 0.00059 (0.00011)  & 0.00007 (0.00148) & 0.00059 (0.00011)   	 	\\ 
# 0.25 & -0.00008 (0.00203) & -0.00269 (0.00011) & -0.00008 (0.00203) & -0.00269 (0.00011)     \\ 
# 0.3 & -0.00033 (0.00263) & -0.00006 (0.00012)  & -0.00033 (0.00263) & -0.00006 (0.00012)    \\ 
# 0.4 & -0.00010 (0.00388) & -0.00188 (0.00011) & -0.00010 (0.00388) & -0.00188 (0.00011)    \\ 
# 0.5 & 0.00050 (0.00588) & -0.00557 (0.00011)  & 0.00050 (0.00588) & -0.00557 (0.00011)     \\  
# \bottomrule
# \vspace{1mm}
# \end{tabular}
# \begin{tabular}{ccccccccc}
# \toprule 
# & \multicolumn{4}{c}{Design 2}\\
# \cmidrule(lr){2-5}  
# & \multicolumn{2}{c}{LM} & \multicolumn{2}{c}{HLM} \\
# \cmidrule(lr){2-3} \cmidrule(lr){4-5} 
# 
# 
# & $\Delta\widehat{\gamma}_{00}$ ($\sigma^2_{\widehat{\gamma}_{00}}$) 
# & $\Delta\widehat{\gamma}_{10}$ ($\sigma^2_{\widehat{\gamma}_{10}}$)
# & $\Delta\widehat{\gamma}_{00}$ ($\sigma^2_{\widehat{\gamma}_{00}}$)
# & $\Delta\widehat{\gamma}_{10}$ ($\sigma^2_{\widehat{\gamma}_{10}}$) \\ 
# \midrule
# ... & -0.00023 (0.00012) & 0.00195 (0.00011)  & -0.00023 (0.00012) & 0.00195 (0.00011)  \\ 
# ... & 0.00028 (0.00040) & 0.00539 (0.00043)  & 0.00028 (0.00040) & 0.00539 (0.00043)  \\ 
# ... & 0.00024 (0.00076) & 0.00219 (0.00072)  & 0.00024 (0.00076) & 0.00219 (0.00072)  \\ 
# ... & 0.00150 (0.00118) & 0.01088 (0.00112)  & 0.00150 (0.00118) & 0.01088 (0.00112)  \\ 
# ... & -0.00033 (0.00143) & -0.01366 (0.00149)  & -0.00033 (0.00143) & -0.01366 (0.00149)  \\ 
# ... & 0.00052 (0.00192) & -0.01280 (0.00213)  & 0.00052 (0.00192) & -0.01280 (0.00213)  \\ 
# ... & 0.00121 (0.00258) & -0.01838 (0.00256)  & 0.00121 (0.00258) & -0.01838 (0.00256) \\ 
# ... & 0.00144 (0.00390) & -0.00562 (0.00379)  & 0.00144 (0.00390) & -0.00562 (0.00379)  \\ 
# ... & -0.00168 (0.00580) & 0.02722 (0.00540)  & -0.00168 (0.00580) & 0.02722 (0.00540)  \\  
# \bottomrule
# \end{tabular}
# \label{tab:rel_abw}
# \end{table}
# 
# 














# SE Plots ----------------------------------------------------------------
simistudy_lvl1 <- readRDS(file = "../Code/Simulationsstudie/simstudy_lvl1")
simistudy_lvl2 <- readRDS(file = "../Code/Simulationsstudie/simstudy_lvl2")
simistudy_lvl1_small <- readRDS(file = "../Code/Simulationsstudie/simstudy_lvl1_small")
simistudy_lvl2_small <- readRDS(file = "../Code/Simulationsstudie/simstudy_lvl2_small")




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

se_efficacy_lvl1 <- se_efficacy(simistudy_lvl1)
se_efficacy_lvl2 <- se_efficacy(simistudy_lvl2)



uzh_colors <- c("#3353B7", "#E38052")
paper_colors <- c("grey", "#B01111")


intercept_lvl1 <- ggplot(data = se_efficacy_lvl1, aes(y = intercept_efficacy, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.1, linetype = "dashed")+
  geom_hline(yintercept = -0.1, linetype = "dashed")+
  scale_fill_manual(values = uzh_colors) + 
  scale_y_continuous(breaks=seq(-1,1, 0.1)) + 
  labs(title = "SE Genauigkeit des Achsenabschnittes bei Intervention auf Level-1")+
  theme(legend.position = "none", legend.box.spacing = )


intercept_lvl2 <- ggplot(data = se_efficacy_lvl2, aes(y = intercept_efficacy, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.1, linetype = "dashed")+
  geom_hline(yintercept = -0.1, linetype = "dashed")+
  scale_fill_manual(values = uzh_colors) + 
  scale_y_continuous(breaks=seq(-1,1, 0.1)) +
  labs(title = "SE Genauigkeit des Achsenabschnittes bei Intervention auf Level-2")+
  theme(axis.title.y = element_blank())

grid.arrange(intercept_lvl1,intercept_lvl2, nrow = 1)

# scale to 0
ggplot(data = se_efficacy_lvl1, aes(y = treatment_efficacy, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.1, linetype = "dashed")+
  geom_hline(yintercept = -0.1, linetype = "dashed")+
  scale_fill_manual(values = uzh_colors) + 
  scale_y_continuous(breaks=seq(-1,1, 0.1)) +
  labs(title = "SE Efficacy Treatment fo Treatment at Level 1")

ggplot(data = se_efficacy_lvl2, aes(y = treatment_efficacy, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = 0.1, linetype = "dashed")+
  geom_hline(yintercept = -0.1, linetype = "dashed")+
  scale_fill_manual(values = uzh_colors) + 
  scale_y_continuous(breaks=seq(-1,1, 0.1)) +
  labs(title = "SE Efficacy Treatment fo Treatment at Level 2")

# scale to 1
se_efficacy <- function(df){
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
se_efficacy_lvl1 <- se_efficacy(simistudy_lvl1)
se_efficacy_lvl2 <- se_efficacy(simistudy_lvl2)

ggplot(data = se_efficacy_lvl1, aes(y = treatment_efficacy, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 1)+
  geom_hline(yintercept = 1.1, linetype = "dashed")+
  geom_hline(yintercept = 0.9, linetype = "dashed")+
  scale_fill_manual(values = uzh_colors) + 
  scale_y_continuous(breaks=seq(0,2, 0.1)) +
  labs(title = "SE Efficacy Treatment fo Treatment at Level 1")

ggplot(data = se_efficacy_lvl2, aes(y = treatment_efficacy, x = icc, fill = method))+
  geom_col(position = "dodge2") +
  geom_hline(yintercept = 1)+
  geom_hline(yintercept = 1.1, linetype = "dashed")+
  geom_hline(yintercept = 0.9, linetype = "dashed")+
  scale_fill_manual(values = uzh_colors) + 
  scale_y_continuous(breaks=seq(0,2, 0.1)) +
  labs(title = "SE Efficacy Treatment fo Treatment at Level 2")
