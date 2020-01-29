gen_ml_data <- function(nschueler = 50, 
                        nklassen = 300, 
                        sd_intercept = 2, 
                        sd_slope = 0, 
                        corr = 0, 
                        sd_error = 5,
                        sd_didaktik = 5,
                        sd_uebung = 5,
                        y00 = 15, 
                        y10 = 0.5,
                        y01 = 0.5){
  
  # Function for truncated normal distribution
  rtnorm <- function(n, mean, sd, a = -Inf, b = Inf){
    qnorm(runif(n, pnorm(a, mean, sd), pnorm(b, mean, sd)), mean, sd)
  }
  
  # Creating Variables
  n <- nschueler * nklassen
  error <- rnorm(n, mean = 0, sd = sd_error)
  uebung <- round(rtnorm(n, mean = 15, sd = sd_uebung, a = 0, b = 30))
  didaktik <- rep(rnorm(nklassen, mean = 0, sd = sd_didaktik), each = nschueler)
  klasse <- rep(1:nklassen, each = nschueler)
  
  # Creating random effects of klassen
  covar01 <- corr * sqrt(sd_intercept^2 * sd_slope^2)

  effekte_cov_matrix <- matrix(c(sd_intercept^2,
                                 covar01,
                                 covar01,
                                 sd_slope^2
                                 ), 2, 2)
  
  effekte <- mvrnorm(n = nklassen, mu = c(0,0), 
                     Sigma = effekte_cov_matrix, empirical = TRUE)
  
  random_intercept <- rep(effekte[,1], each = nschueler)
  random_slope <- rep(effekte[,2], each = nschueler)
  
  # Calculating individual leistung score
  leistung <- numeric(n)
  for (i in 1:n){
  leistung[i] <- y00 +  
    y10 * uebung[i] + 
    y01 * didaktik[i] +
    random_intercept[i] + 
    random_slope[i] * uebung[i] + 
    error[i]
  }
  # Creating dataframe
  klasse <- as.factor(klasse)
  
  ml_data <- data.frame(klasse, uebung, didaktik, leistung, random_intercept, random_slope)
  
  return(ml_data)
}
