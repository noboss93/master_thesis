pos_cor <- gen_ml_data(n = 150, nklassen = 5, corr = 1, sd_slope = 1, sd_intercept = 10)
neutr_cor <- gen_ml_data(n = 150, nklassen = 5, corr = 0, sd_slope = 1, sd_intercept = 10)
neg_cor <- gen_ml_data(n = 150, nklassen = 5, corr = -1, sd_slope = 1, sd_intercept = 10)


lm_neg<- lmer(punktzahl ~ uebung + (uebung|klasse), data = neg_cor)
intercept <- as.numeric(ranef(lm_neg)$klasse[,1]) + as.numeric(fixef(lm_neg)[1])
slope <- as.numeric(ranef(lm_neg)$klasse[,2]) + as.numeric(fixef(lm_neg)[2])
klasse <- c(1:5)
coef_data <- data.frame(intercept, slope, klasse)
colors <- c("Gesamtgerade" = "red", "Klassengerade" = "black")

neg <- ggplot(data = neg_cor, mapping = aes(x = uebung, y = punktzahl)) + 
  geom_point(size = 2) +
  geom_abline(slope = slope, intercept = intercept, size = 1) +
  theme_gray(base_size = 15) +
  labs(x = " ", y = "Punktzahl", title = "Negative Korrelation")+
  theme(axis.title.y = element_blank())


lm_neutr<- lmer(punktzahl ~ uebung + (uebung||klasse), data = neutr_cor)
intercept <- as.numeric(ranef(lm_neutr)$klasse[,1]) + as.numeric(fixef(lm_neutr)[1])
slope <- as.numeric(ranef(lm_neutr)$klasse[,2]) + as.numeric(fixef(lm_neutr)[2])
klasse <- c(1:5)
coef_data <- data.frame(intercept, slope, klasse)
colors <- c("Gesamtgerade" = "red", "Klassengerade" = "black")

neutr <- ggplot(data = neutr_cor, mapping = aes(x = uebung, y = punktzahl)) + 
  geom_point(size = 2) +
  geom_abline(slope = slope, intercept = intercept, size = 1) +
  theme_gray(base_size = 15) +
  labs(x = "Unabhängie Variable", y = "Punktzahl", title = "Unkorreliert")+
  theme(axis.title.y = element_blank())


lm3<- lmer(punktzahl ~ uebung + (uebung||klasse), data = pos_cor)
intercept <- as.numeric(ranef(lm3)$klasse[,1]) + as.numeric(fixef(lm3)[1])
slope <- as.numeric(ranef(lm3)$klasse[,2]) + as.numeric(fixef(lm3)[2])
klasse <- c(1:5)
coef_data <- data.frame(intercept, slope, klasse)
colors <- c("LM (Disaggregation)" = "red", "HLM" = "black")
pos <- ggplot(data = pos_cor, mapping = aes(x = uebung, y = punktzahl)) + 
  geom_point(size = 2) +
  geom_abline(slope = slope, intercept = intercept, size = 1) +
  theme_gray(base_size = 15) +
  labs(x = " ", y = "Abhängige Variable", title = "Positive Korrelation")

grid.arrange(pos, neutr, neg, nrow = 1)

