gen_ml_data <- function(n = 240, nklassen = 8, sd_intercept = 10, sd_slope = 0, corr = 0){
  
  # Creating Level-1 Variables with fixed effects
  stunden <- round(runif(n, 1, 30), digits = 0)
  iq <- round(rnorm(n, mean = 100, sd = 10))
 
  
  
  # Creating Level-2 Variables
  ses <- round(rnorm(nklassen, mean = 50, sd = 5), digits = 0)
  klasse <- sample(1:nklassen, n, replace = TRUE)
  class_ses <- c()
  for (i in 1:n){
    class_ses[i] <- ses[klasse[i]]
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
  error <- round(rnorm(n, 0, 5), digits = 1)
  
  # Calculating individual leistung score
  leistung <- round(20 + 2 * stunden + 0.7 * iq + 0.5 * class_ses + 
                      random_intercept + random_slope * stunden + error, digits = 0)
  
  # Creating dataframe
  klasse <- as.factor(klasse)
  levels(klasse) <- paste(1:nklassen, "md", sep = "")
  
  ml_data <- data.frame(klasse, stunden, iq, class_ses, leistung)
  return(ml_data)
}
