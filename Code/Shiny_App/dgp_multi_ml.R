gen_ml_data <- function(n = 240, nklassen = 8, sd_intercept = 10, sd_slope = 0, corr_01 = 0){
  
  stunden <- round(runif(n, 1, 30), digits = 0)
  iq <- round(rnorm(n, mean = 100, sd = 10))
  ses <- rnorm(nklassen, mean = 50, sd = 5)
  klasse <- sample(1:nklassen, n, replace = TRUE)
  
  class_ses <- c()
  for (i in 1:n){
    class_ses[i] <- ses[klasse[i]]
  }
  
  covar01 <- corr_01 * sqrt(sd_intercept^2 * sd_slope^2)

  effekte_cov_matrix <- matrix(c(sd_intercept^2,
                                 covar01,
                                 covar01,
                                 sd_slope^2
                                 ), 2, 2)
  
  effekte <- mvrnorm(n = nklassen, mu = c(0,0), 
                     Sigma = effekte_cov_matrix, empirical = TRUE)
  
  effekt_int <- effekte[,1]
  effekt_slope <- effekte[,2]
  
  random_intercept <- numeric(n)
  for (i in 1:n){
    random_intercept[i] <- effekt_int[klasse[i]]
  }
  
  random_slope <- numeric(n)
  for (i in 1:n){
    random_slope[i] <- effekt_slope[klasse[i]]
  }
  
  error <- round(rnorm(n, 0, 5), digits = 1)
  leistung <- round(20 + 2 * stunden + 0.7 * iq + 0.5 * class_ses + 
                      random_intercept + random_slope * stunden + error, digits = 0)
  
  klasse <- as.factor(klasse)
  levels(klasse) <- paste(1:nklassen, "md", sep = "")
  
  ranint_data <- data.frame(klasse, stunden, iq, class_ses, leistung)
  return(ranint_data)
}
