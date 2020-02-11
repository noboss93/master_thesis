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

font_import()
loadfonts(device="win") 

# Funktion ----------------------------------------------------------------
rtnorm <- function(n, mean, sd, a = -Inf, b = Inf){
  qnorm(runif(n, pnorm(a, mean, sd), pnorm(b, mean, sd)), mean, sd)
}

gen_ml_data <- function(n = 15000, nklassen = 300, sd_intercept = 2, sd_slope = 0, 
                        corr = 0, sd_error = 5, b00 = 15, b10 = 0.35){
  
  # Creating Level-1 Variable
  uebung <- sample(rep(c(0:29), each = n/length(c(0:29))), n)
  geschl <- sample(rep(c("m", "w"), each = n/2), n)
  iq <- round(rnorm(n, mean = 100, sd = 10))
  ses <- round(rnorm(n, mean = 20, sd = 4))
  klasse <- rep(1:nklassen, each = n/nklassen)
  
  # Creating Level-2 Variable
  didaktik <- round(rtnorm(n, mean = 5, sd = 2, a = 0, b = 10))
  
  
  # Creating random effects of klassen
  covar01 <- corr * sqrt(sd_intercept^2 * sd_slope^2)
  
  effekte_cov_matrix <- matrix(c(sd_intercept^2,
                                 covar01,
                                 covar01,
                                 sd_slope^2
  ), 2, 2)
  
  effekte <- mvrnorm(n = nklassen, mu = c(0,0), 
                     Sigma = effekte_cov_matrix, empirical = TRUE)
  
  effekt_int <- effekte[,1]
  effekt_slope <- effekte[,2]
  
  # Creating random effects for Intercept and Slope based on klassen
  random_intercept <- numeric(n)
  for (i in 1:n){
    random_intercept[i] <- effekt_int[klasse[i]]
  }
  
  random_slope <- numeric(n)
  for (i in 1:n){
    random_slope[i] <- effekt_slope[klasse[i]]
  }
  
  # Creating random error for individuals
  error <- rnorm(n, mean = 0, sd = sd_error)
  
  # Calculating individual punktzahl score
  punktzahl <- b00 +  
    b10 * uebung + 
    random_intercept + 
    random_slope * uebung + 
    error
  
  # Creating dataframe
  klasse <- as.factor(klasse)
  levels(klasse) <- c(1:nklassen)
  
  ml_data <- data.frame(klasse, uebung, punktzahl, geschl, didaktik, ses, iq)
  
  return(ml_data)
}


# Beispieldaten -----------------------------------------------------------

# Beispiel Daten für Theorie
test <- gen_ml_data(n = 150, nklassen = 5, sd_intercept = 2, 
                    b10 = 0.5, b00 = 20,
                    sd_slope = 0.2, corr = 0.5, sd_error = 5)

test_neg <- gen_ml_data(n = 150, nklassen = 5, sd_intercept = 5, 
                        b10 = 0.5, b00 = 20,
                        sd_slope = 1, corr = -0.8, sd_error = 5)
test_neutr <- gen_ml_data(n = 150, nklassen = 5, sd_intercept = 5, 
                          b10 = 0.5, b00 = 20,
                          sd_slope = 1, corr = 0, sd_error = 5)


# saveRDS(test, file = "dataset_theory")
# saveRDS(test_neg, file = "neg_corr")
# saveRDS(test_neutr, file = "neutr_corr")

test <- readRDS("dataset_theory")
colnames(test) <- c("klasse", "uebung", "punktzahl", "geschl", "fenster", "ses", "iq")


# Aggregated Dataframe
punktzahl_aggr <- c()
uebung_aggr <- c()
klasse <- c(1:5)

for (i in 1:5){
  punktzahl_aggr[i] <- mean(test[test[,1]==i,3])
  uebung_aggr[i] <- mean(test[test[,1]==i,2])
}

data_aggr <- data.frame(matrix(c(klasse, uebung_aggr, punktzahl_aggr), ncol = 3, nrow = 5))
colnames(data_aggr) <- c("klasse", "uebung", "punktzahl")
data_aggr[,1] <- as.factor(data_aggr[,1])
# Latex Tabelle -----------------------------------------------------------

# Intro Beispiel
test <- test[sample(1:length(test[,1])),]
xtable(head(test, n = 10), digits = 0)

# Aggregated Table
xtable(data_aggr, digits = 1)



# Regressionsmodelle ------------------------------------------------------

lm0 <- lmer(punktzahl ~ (1|klasse), data = test)
lm2<- lmer(punktzahl ~ uebung + (1|klasse), data = test)
lm3 <- lmer(punktzahl ~ uebung + (uebung || klasse), data = test)

as0 <- lm(data = test, punktzahl ~ 1)
as1 <- lm(data = test, punktzahl ~ math_lektionen)
as2 <- lm(data = test, punktzahl ~ math_lektionen + klasse)
as3 <- lm(data = test, punktzahl ~ math_lektionen * klasse)

lm_aggr <- lm(data = data_aggr, punktzahl ~ uebung)

summary(lm0)
summary(lm2)
anova(lm2, dis_lm)
summary(as0)


# Theory Plots LM ---------------------------------------------------------

# Regression Aggregation
ggplot(data = data_aggr, mapping = aes(x = uebung, y = punktzahl))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", se = FALSE, col = "red", size = 1) +
  labs(x = "Anzahl gelöster Übungsaufgaben", y = "Punktzahl") +
  theme_gray(base_size = 15)

# Regression Disagregation
dis_lm <- lm(data = test, punktzahl ~ uebung)
a <- ggplot(data = test, mapping = aes(x = uebung, y = punktzahl))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "red", fullrange = TRUE)+
  labs(x = "Anzahl gelöster Übungsaufgaben", y = "Punktzahl", title = "Modell für gesamten Datensatz") +
  theme_gray(base_size = 15)

b <- ggplot(data = test, mapping = aes(x = uebung, y = punktzahl))+
  geom_point()+
  geom_abline(intercept = coef(dis_lm)[1], slope = coef(dis_lm)[2], size = 1, col = "red")+
  facet_wrap(~klasse)+
  labs(x = "Anzahl gelöster Übungsaufgaben", y = "Punktzahl", title = "Modell für jede Klasse") +
  theme_gray(base_size = 15) +
  theme(axis.title.y = element_blank())

grid.arrange(a,b, nrow = 1)


# Theory Plots HLM --------------------------------------------------------

lm2<- lmer(punktzahl ~ uebung + (1|klasse), data = test)
intercept <- as.numeric(ranef(lm2)$klasse[,1]) + as.numeric(fixef(lm2)[1])
fix_slope <- rep(as.numeric(fixef(lm2)[2]), times = 5)
klasse <- c(1:5)
coef_data <- data.frame(intercept, fix_slope, klasse)
colors <- c("Gesamtgerade" = "red", "Klassengerade" = "black")

a <- ggplot(data = test, mapping = aes(x = uebung, y = punktzahl)) + 
  geom_point(size = 2) +
  geom_abline(slope = fix_slope, intercept = intercept, size = 1) +
  geom_abline(slope = fix_slope, 
              intercept = mean(intercept), col = "red", size = 1) +
  theme_gray(base_size = 15) +
  labs(x = "Anzahl gelöster Übungsaufgaben", y = "Punktzahl", title = "Modell für gesamten Datensatz")

b <- ggplot(data = test, mapping = aes(x = uebung, y = punktzahl))+
  geom_point()+
  geom_abline(data = coef_data, aes(intercept = intercept, slope = fix_slope, group = klasse, color = "Klassengerade"), size = 1)+
  geom_abline(data = coef_data, aes(intercept = mean(intercept), slope = mean(fix_slope), color = "Gesamtgerade"), size = 1)+
  facet_wrap(~klasse)+
  labs(x = "Anzahl gelöster Übungsaufgaben", y = "Punktzahl", title = "Modell für jede Klasse") +
  theme_gray(base_size = 15) +
  scale_color_manual(values = colors, name = "Legende")+
  theme(axis.title.y = element_blank(), legend.position = c(0.85,0.25))

grid.arrange(a,b, nrow = 1)

c <- ggplot(dis_lm, aes(.fitted, .resid))+
  geom_point()+
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1)+
  geom_hline(yintercept = 0, color = "black", size = 1) +
  labs(y = "Residuen", x = "Erwartete Werte", title = "Lineares Modell") + 
  theme_gray(base_size = 15) +
  ylim(-25,25)

d <- ggplot(lm2, aes(.fitted, .resid))+
  geom_point()+
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1)+
  geom_hline(yintercept = 0, color = "black", size = 1) +
  labs(y = "Residuen", x = "Erwartete Werte", title = "Random Intercept Modell") + 
  theme_gray(base_size = 15)+
  ylim(-25,25)+
  theme(axis.title.y = element_blank())

grid.arrange(c,d, nrow = 1)


lm3<- lmer(punktzahl ~ uebung + (uebung||klasse), data = test)
intercept <- as.numeric(ranef(lm3)$klasse[,1]) + as.numeric(fixef(lm3)[1])
slope <- as.numeric(ranef(lm3)$klasse[,2]) + as.numeric(fixef(lm3)[2])
klasse <- c(1:5)
coef_data <- data.frame(intercept, slope, klasse)
colors <- c("Gesamtgerade" = "red", "Klassengerade" = "black")

e <- ggplot(data = test, mapping = aes(x = uebung, y = punktzahl)) + 
  geom_point(size = 2) +
  geom_abline(slope = slope, intercept = intercept, size = 1) +
  geom_abline(slope = mean(slope), 
              intercept = mean(intercept), col = "red", size = 1) +
  theme_gray(base_size = 15) +
  labs(x = "Anzahl gelöster Übungsaufgaben", y = "Punktzahl", title = "Modell für gesamten Datensatz")

f <- ggplot(data = test, mapping = aes(x = uebung, y = punktzahl))+
  geom_point()+
  geom_abline(data = coef_data, aes(intercept = intercept, slope = slope, group = klasse, color = "Klassengerade"), size = 1)+
  geom_abline(data = coef_data, aes(intercept = mean(intercept), slope = mean(slope), color = "Gesamtgerade"), size = 1)+
  facet_wrap(~klasse)+
  labs(x = "Anzahl gelöster Übungsaufgaben", y = "Punktzahl", title = "Modell für jede Klasse") +
  theme_gray(base_size = 15) +
  scale_color_manual(values = colors, name = "Legende")+
  theme(axis.title.y = element_blank(), legend.position = c(0.85,0.25))

grid.arrange(e,f, nrow = 1)

ggplot(lm3, aes(.fitted, .resid))+
  geom_point()+
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1)+
  geom_hline(yintercept = 0, color = "black", size = 1) +
  labs(y = "Residuen", x = "Erwartete Werte", title = "Random Intercept and Slope Modell") +
  ylim(-25,25)+
  theme_gray(base_size = 15)



lm_neg<- lmer(punktzahl ~ uebung + (uebung|klasse), data = test_neg)
intercept <- as.numeric(ranef(lm_neg)$klasse[,1]) + as.numeric(fixef(lm_neg)[1])
slope <- as.numeric(ranef(lm_neg)$klasse[,2]) + as.numeric(fixef(lm_neg)[2])
klasse <- c(1:5)
coef_data <- data.frame(intercept, slope, klasse)
colors <- c("Gesamtgerade" = "red", "Klassengerade" = "black")

neg <- ggplot(data = test_neg, mapping = aes(x = uebung, y = punktzahl)) + 
  geom_point(size = 2) +
  geom_abline(slope = slope, intercept = intercept, size = 1) +
  geom_abline(slope = mean(slope), 
              intercept = mean(intercept), col = "red", size = 1) +
  theme_gray(base_size = 15) +
  labs(x = "Anzahl gelöster Übungsaufgaben", y = "Punktzahl", title = "Negative Korrelation")


lm_neutr<- lmer(punktzahl ~ uebung + (uebung||klasse), data = test_neutr)
intercept <- as.numeric(ranef(lm_neutr)$klasse[,1]) + as.numeric(fixef(lm_neutr)[1])
slope <- as.numeric(ranef(lm_neutr)$klasse[,2]) + as.numeric(fixef(lm_neutr)[2])
klasse <- c(1:5)
coef_data <- data.frame(intercept, slope, klasse)
colors <- c("Gesamtgerade" = "red", "Klassengerade" = "black")

neutr <- ggplot(data = test_neutr, mapping = aes(x = uebung, y = punktzahl)) + 
  geom_point(size = 2) +
  geom_abline(slope = slope, intercept = intercept, size = 1) +
  geom_abline(slope = mean(slope), 
              intercept = mean(intercept), col = "red", size = 1) +
  theme_gray(base_size = 15) +
  labs(x = "Anzahl gelöster Übungsaufgaben", y = "Punktzahl", title = "Unkorreliert")+
  theme(axis.title.y = element_blank())

grid.arrange(neg,neutr, nrow = 1)





