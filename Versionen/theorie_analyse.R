library(xtable)
library(MASS)
library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)

# Beispiel Daten für Theorie
source("dgp_multi_ml.R")
test <- gen_ml_data(n = 50, nklassen = 5, sd_intercept = 5, b10 = 1)
test_ohne_math <- select(.data = test, "klasse", "leistung")
test_ohne_math[,1] <- sort(test_ohne_math[,1])
test_ohne_math[,2] <- round(test_ohne_math[,2], digits = 0)
test_ohne_math[,2] <- ifelse(test_ohne_math[,2] < 0, 0,test_ohne_math[,2])

m <- matrix(test_ohne_math[,2],ncol = 5, nrow = 10)
beispiel <- data.frame(m)
colnames(beispiel) <- c(paste("Klasse", sep = " ", 1:5))
beispiel <- round(beispiel)
saveRDS(beispiel, file = "beispiel_theorie")


xtable(beispiel[,], caption = "Erzielte Leistung in Mathematikprüfung", 
       label = "tab:beispiel_theorie", digits = 0)

lm <- lmer(data = test, leistung ~ (1|klasse))
lm2 <- lmer(data = test, leistung ~ math_lektionen + (1|klasse))
lm3 <- lmer(data = test, leistung ~ math_lektionen + (math_lektionen | klasse))
as <- lm(data = test, leistung ~ klasse)
as2 <- lm(data = test, leistung ~ math_lektionen + klasse)
as3 <- lm(data = test, leistung ~ math_lektionen * klasse)
summary(lm)
summary(lm2)

summary(as)
summary(as2)
summary(as3)


anova(lm, lm2, lm3, test = "LRT")
anova(as, as2, as3, test = "LRT")


anova(as2)

ggplot(data = test, mapping = aes(x = math_lektionen, y = leistung, group = klasse))+
  geom_point(aes(shape = klasse))+ 
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x, fullrange = TRUE)
