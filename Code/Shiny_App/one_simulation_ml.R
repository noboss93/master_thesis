one_simulation <- function(sd_intercept = 10, sd_slope = 0, corr = 0){
  source("dgp_multi_ml.R")
  beta1_mlm <- NA
  beta1_lm <- NA
  ml_data <- gen_ml_data()
  mlm_model <- lmer(leistung ~ iq + math_lektionen + (1 | klasse), data = ml_data)
  lm_model <- lm(leistung ~ iq + math_lektionen, data = ml_data)
  beta1_lm <- coef(lm_model)[2]
  beta1_mlm <- fixef(mlm_model)[2]
  return(c(beta1_lm, beta1_mlm))
}

niter = 1000
beta1coef <- data.frame(matrix(ncol = 2))
colnames(beta1coef) <- c("beta1_lm", "beta1_mlm")
for(i in 1:niter){
  beta1coef[i,] <- one_simulation(sd_intercept = 10)
}

bias <- beta1coef - 1.5
t.test(bias[,1], bias[,2])


boxplot(beta1coef)
sd(beta1coef[,1])
sd(beta1coef[,2])
