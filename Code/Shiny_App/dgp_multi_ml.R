gen_ml_data <- function(n = 15000, nklassen = 300, sd_intercept = 10, sd_slope = 0, 
                        corr = 0, sd_error = 5, y00 = 15, y10 = 1.5){
  
  # Creating Treatment as Level-1 Variable
  # math_lektionen <- sample(c(0:6), n, replace = TRUE)
  klasse <- sample(1:nklassen, n, replace = TRUE)
  
  # Creating Treatment as Level-2 Variable
  anz_math_lektionen <- sample(c(0:6), n, replace = TRUE)
  math_lektionen <- c()
  for (i in 1:n){
    math_lektionen[i] <- anz_math_lektionen[klasse[i]]
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
  leistung <- y00 +  
    y10 * math_lektionen + 
    random_intercept + 
    # random_slope * math_lektionen + 
    error
  
  # Creating dataframe
  klasse <- as.factor(klasse)
  levels(klasse) <- paste(1:nklassen, "md", sep = "")
  
  ml_data <- data.frame(klasse, math_lektionen, leistung)
  
  return(ml_data)
}
