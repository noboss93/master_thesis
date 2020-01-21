library(xtable)
library(MASS)
library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)

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
  
  # Calculating individual leistung score
  leistung <- b00 +  
    b10 * uebung + 
    random_intercept + 
    random_slope * uebung + 
    error
  
  # Creating dataframe
  klasse <- as.factor(klasse)
  levels(klasse) <- c(1:nklassen)
  
  ml_data <- data.frame(klasse, uebung, leistung, geschl, didaktik, ses, iq)
  
  return(ml_data)
}


# Beispieldaten -----------------------------------------------------------

# Beispiel Daten für Theorie
test <- gen_ml_data(n = 150, nklassen = 5, sd_intercept = 2, 
                    b10 = 0.5, b00 = 20,
                    sd_slope = 0.2, corr = 0.5, sd_error = 5)

# saveRDS(test, file = "dataset_theory")
test <- readRDS("dataset_theory")

# Aggregated Dataframe
leistung_aggr <- c()
uebung_aggr <- c()
klasse <- c(1:5)

for (i in 1:5){
  leistung_aggr[i] <- mean(test[test[,1]==i,3])
  uebung_aggr[i] <- mean(test[test[,1]==i,2])
}

data_aggr <- data.frame(matrix(c(klasse, uebung_aggr, leistung_aggr), ncol = 3, nrow = 5))
colnames(data_aggr) <- c("klasse", "uebung", "leistung")
data_aggr[,1] <- as.factor(data_aggr[,1])
# Latex Tabelle -----------------------------------------------------------

# Intro Beispiel
test <- test[sample(1:length(test[,1])),]
xtable(head(test, n = 10), digits = 0)

# Aggregated Table
xtable(data_aggr, digits = 1)


# Theory Plots LM ---------------------------------------------------------

# Regression Aggregation
ggplot(data = data_aggr, mapping = aes(x = uebung, y = leistung))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", se = FALSE, col = "red", size = 1) +
  labs(x = "Anzahl gelöster Übungsaufgaben", y = "Punktzahl") +
  theme_gray(base_size = 20)







as4 <- lm(data = test, leistung ~ uebung)
ggplot(data = test, mapping = aes(x = uebung, y = leistung))+
  geom_point()+
  geom_abline(intercept = coef(as4)[1], slope = coef(as4)[2], size = 1)+
  facet_wrap(~klasse)

ggplot(data = test, mapping = aes(x = uebung, y = leistung))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, col = "black", fullrange = TRUE)+
  facet_wrap(~klasse)

ggplot(data = test, mapping = aes(x = uebung, y = leistung))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "black", fullrange = TRUE)

ggplot(data = test, mapping = aes(x = uebung, y = leistung))+
  geom_point(aes(shape = klasse)) +
  geom_smooth(method = "lm", se = FALSE, col = "black", fullrange = TRUE)

ggplot(data = test, mapping = aes(sample = leistung, shape = klasse))+
  geom_qq() + 
  geom_qq_line()

ggplot(data = test, mapping = aes(sample = leistung))+
  geom_qq() +
  geom_qq_line()



# Regressionsmodelle ------------------------------------------------------

lm0 <- lmer(leistung ~ (1|klasse), data = test)
lm2 <- lmer(leistung ~ math_lektionen + (1|klasse), data = test)
lm3 <- lmer(leistung ~ math_lektionen + (math_lektionen | klasse), data = test)

as0 <- lm(data = test, leistung ~ klasse)
as1 <- lm(data = test, leistung ~ math_lektionen)
as2 <- lm(data = test, leistung ~ math_lektionen + klasse)
as3 <- lm(data = test, leistung ~ math_lektionen * klasse)

lm_aggr <- lm(data = data_aggr, leistung ~ uebung)




