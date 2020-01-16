library(xtable)
library(MASS)
library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
saveRDS(beispiel, file = "beispiel_theorie")

# Funktion ----------------------------------------------------------------

gen_ml_data <- function(n = 15000, nklassen = 300, sd_intercept = 2, sd_slope = 0, 
                        corr = 0, sd_error = 5, b00 = 15, b10 = 0.35){
  
  # Creating Treatment as Level-1 Variable
  uebung <- sample(rep(c(0:29), each = n/length(c(0:29))), n)
  geschl <- sample(rep(c("m", "w"), each = n/2), n)
  iq <- round(rnorm(n, mean = 100, sd = 10))
  ses <- round(rnorm(n, mean = 20, sd = 4))
  klasse <- rep(1:nklassen, each = n/nklassen)
  
  # Creating Treatment as Level-2 Variable
  anz_fenster <- sample(c(3:10), n, replace = TRUE)
  fenster <- c()
  for (i in 1:n){
    fenster[i] <- anz_fenster[klasse[i]]
  }
  
  
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
  
  ml_data <- data.frame(klasse, uebung, leistung, geschl, fenster, ses, iq)
  
  return(ml_data)
}


# Beispieldaten -----------------------------------------------------------

# Beispiel Daten für Theorie
test <- gen_ml_data(n = 150, nklassen = 5, sd_intercept = 2, 
                    b10 = 0.5, b00 = 20,
                    sd_slope = 0.2, corr = 0.5, sd_error = 5)
test[,3] <- round(test[,3])

# saveRDS(test, file = "dataset_theory")
test <- readRDS("dataset_theory")

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

test <- test[sample(1:length(test[,1])),]

xtable(head(test, n = 10), digits = 0)

lm <- lmer(data = test, leistung ~ (1|klasse))
lm2 <- lmer(data = test, leistung ~ math_lektionen + (1|klasse))
lm3 <- lmer(data = test, leistung ~ math_lektionen + (math_lektionen | klasse))
as <- lm(data = test, leistung ~ klasse)
as2 <- lm(data = test, leistung ~ math_lektionen + klasse)
as3 <- lm(data = test, leistung ~ math_lektionen * klasse)
as4 <- lm(data = test, leistung ~ math_lektionen)
summary(lm)
summary(lm2)

summary(as)
summary(as2)
summary(as3)
summary(as4)


anova(lm, lm2, lm3, test = "LRT")
anova(as, as2, as3, test = "LRT")

anova(lm3, as4, test = "LRT")

anova(as)






