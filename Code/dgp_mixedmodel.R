library(lme4)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)


# generating one random intercept model -----------------------------------

ran_inter <- function(n = 1000, nklassen = 20, sd_intercept = 40){
  stunden <- round(runif(n, 1, 30), digits = 0)
  klasse <- sample(1:nklassen, n, replace = TRUE)
  effekt <- round(rnorm(nklassen, 0, sd_intercept), digits = 1)
  
  random_intercept <- numeric(n)
  for (i in 1:n){
    random_intercept[i] <- effekt[klasse[i]]
  }
  
  error <- round(rnorm(n, 0, 5), digits = 1)
  leistung <- round(30 + 2.5 * stunden + random_intercept + error, digits = 0)
  
  klasse <- as.factor(klasse)
  levels(klasse) <- paste(1:nklassen, "md", sep = "")
  
  ranint_data <- data.frame(stunden, klasse, leistung)
  
  # writing multilevel model
  ri_model <- lmer(leistung ~ stunden + (1|klasse), data = ranint_data)
  
  # saving random variances
  var_residual <- attr(VarCorr(ri_model), "sc")^2
  var_intercept <- as.numeric(VarCorr(ri_model))
  
  # calculating intraclass correlation
  icc <- var_intercept / (var_residual + var_intercept)
  
  # writing lm model
  lm_model <- lm(leistung ~ stunden, data = ranint_data)
  intercept_lm <- coef(lm_model)[1]
  slope_lm <- coef(lm_model)[2]
  
  # plotting data
  intercept = coef(ri_model)$klasse[,1]
  slope = coef(ri_model)$klasse[,2]
  
  ggplot(data = ranint_data, mapping = aes(x = stunden, y = leistung, color = klasse)) + 
    geom_point() +
    scale_color_viridis_d() +
    geom_abline(slope = slope, intercept = intercept, col = viridis(n = nklassen)) +
    geom_abline(slope = mean(slope), intercept = mean(intercept), col = "red", size = 1) +
    labs(x = "Anzahl Lernstunden", y = "Anzahl Punkte", title = "Erreichte Punktzahl nach Klassen") +
    ylim(0,NA)
}

ran_inter(n = 240, nklassen = 8, sd_intercept = 50)

