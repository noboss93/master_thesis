gen_ml_data <- function(nschueler = 50, 
                        nklassen = 300, 
                        sd_intercept = 3, 
                        sd_slope = 0, 
                        corr = 0, 
                        sd_error = 5,
                        y00 = 15, 
                        y10 = 0.3,
                        treatment_level1 = TRUE){
  
  # Creating Variables
  n <- nschueler * nklassen
  error <- rnorm(n, mean = 0, sd = sd_error)
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
  
  if (treatment_level1 == TRUE){
    uebung <- sample(c(1:30), replace = TRUE, size = nschueler * nklassen)
    
    # Calculating individual punktzahl score
    punktzahl <- numeric(n)
    for (i in 1:n){
      punktzahl[i] <- y00 +  
        y10 * uebung[i] + 
        random_intercept[i] + 
        random_slope[i] * uebung[i] + 
        error[i]
    }
    
  } else{
    uebung <- rep(sample(rep(c(-1,1), each = nklassen/2), size = nklassen), 
                  each = nschueler)
    
    punktzahl <- numeric(n)
    for (i in 1:n){
      punktzahl[i] <- y00 +  
        y10 * uebung[i] + 
        random_intercept[i] + 
        error[i]
    }
  }
  
  # Creating dataframe
  klasse <- as.factor(klasse)
  
  ml_data <- data.frame(klasse, uebung, punktzahl, random_intercept, random_slope)
  
  return(ml_data)
}

