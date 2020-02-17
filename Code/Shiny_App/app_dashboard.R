library(shiny)
library(shinyjs) 
library(shinydashboard)
library(lme4)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(MASS)

ui <- dashboardPage(
  dashboardHeader(title = "Multilevel Analyse", titleWidth = 350),
  dashboardSidebar(width = 350,
    sidebarMenu(
      menuItem("Was ist eine Multilevel Analyse?", tabName = "intro"),
      menuItem("Beispiel einer Multilevel Analyse", tabName = "analysing"),
      menuItem("Simulationsstudie", tabName = "simstudy")
      )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
    tabItem(tabName = "intro",
            source("tab_intro.R", encoding = "utf8")[1]),
    tabItem(tabName = "analysing", 
            source("tab_analysing_intro.R", encoding = "utf8")[1]),
    tabItem(tabName = "simstudy", 
            source("tab_simstudy_app.R", encoding = "utf8")[1])
    )
  )
)


server <- function(input, output, session) {
  
  # Laden der richtigen Funktion
  source("dgp_app.R")
  
  # Generieren des Datensatzes
  data_model  <- eventReactive(input$gen_data, {
    gen_ml_data(nschueler = 30, nklassen = 8, sd_intercept = input$int_sd, 
              sd_slope = input$slope_sd, corr = input$corr)
    })
  
  # Model und Koeffizienten berechnen
  model <- eventReactive(c(input$method, input$gen_data), {
    switch(input$method,
           "lm" = lm(punktzahl ~ uebung, data = data_model()),
           "ri" = lmer(punktzahl ~ uebung + (1|klasse), data = data_model()),
           "rs" = lmer(punktzahl ~ uebung + (uebung|klasse), data = data_model())
    )
  })
  
  intercept <- eventReactive(c(input$method, input$gen_data), {
    if (input$method == "lm"){
      coef(model())[1]
    } else {
      coef(model())$klasse[,1]
    }
  })
  
  slope <- eventReactive(c(input$method, input$gen_data), {
  if (input$method == "lm"){
    coef(model())[2]
  } else {
    coef(model())$klasse[,2]
  }
  })
  

# Intro Summary Data ------------------------------------------------------
  
  
  # Ausgabe der Tabelle
  output$table <- renderDataTable({
    data_model()
  })
  
  # Ausgabe der Struktur
  output$str <- renderPrint({
    str(data_model())
  })
  
  # Ausgabe der Summary
  output$data_summary <- renderPrint({
    summary(data_model())
  })
  

# Intro Plots -------------------------------------------------------------
  
  observeEvent(c(input$method, input$gen_data), {
  if (input$method == "lm"){
    shinyjs::show("groupcolor")
  } else {
    hide("groupcolor")
  }
  })
  
  # Plotten der Regressions Geraden
  output$multiplot <- renderPlot({
    if (input$method == "lm"){
      if (input$groupcolor == TRUE){
      ggplot(data = data_model(), mapping = aes(x = uebung, y = punktzahl, color = klasse)) + 
        geom_point(size = 2) +
        scale_color_viridis_d() +
        geom_abline(slope = slope(), intercept = intercept(), col = "red", size = 1) +
        labs(x = "Anzahl gelöste Übungen", y = "Anzahl Punkte")
          
      } else {
        ggplot(data = data_model(), mapping = aes(x = uebung, y = punktzahl)) + 
          geom_point(size = 2) +
          geom_abline(slope = slope(), intercept = intercept(), col = "red", size = 1) +
          labs(x = "Anzahl gelöste Übungen", y = "Anzahl Punkte")
          
      }
    } else {
      ggplot(data = data_model(), mapping = aes(x = uebung, y = punktzahl, color = klasse)) + 
        geom_point(size = 2) +
        scale_color_viridis_d() +
        geom_abline(slope = slope(), intercept = intercept(), col = viridis(n = 8), size = 1) +
        geom_abline(slope = mean(slope()), 
                    intercept = mean(intercept()), col = "red", size = 1) +
        labs(x = "Anzahl gelöste Übungen", y = "Anzahl Punkte")
        
    }
  })
  
  # Plotten der Residuenplots
  output$residual <- renderPlot({
    residual_df <- data.frame(residuals(model()), fitted.values(model()), data_model()$klasse)
    colnames(residual_df) <- c("residuals", "fitted", "klasse")
    
    ggplot(residual_df, aes(x = fitted, y = residuals)) +
      geom_point(shape = 16, size = 3) +
      scale_color_viridis_d() +
      geom_hline(yintercept = 0, col = "black") + 
      geom_smooth(method = "loess", se = FALSE, col = "red") +
      # geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Angepasster Wert", y = "Residuen")
  })
  
  # Plot des QQ-Plots
  output$qq <- renderPlot({
    residual_df <- data.frame(residuals(model()), fitted.values(model()), data_model()$klasse)
    colnames(residual_df) <- c("residuals", "fitted", "klasse")
    
    ggplot(residual_df, aes(sample = residuals)) +
      geom_qq(shape = 1, size = 3) +
      geom_qq_line() + 
      labs(x = "Erwartete Werte ", y = "Beobachtete Werte")
  })
  
  # PLot des Summarys
  output$summary <- renderPrint({
    summary(model())
  })
   

# Sim Study ----------------------------------------------------------------
  mean_parameters <- function(df){
    icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
    intercept_mean_lm <- c()
    intercept_mean_mlm <- c()
    treatment_mean_lm <- c()
    treatment_mean_mlm <- c()
    
    for(i in 1:length(icc)){
      intercept_mean_lm[i] <- mean(df$beta_0[test_lvl1$method == "lm" & test_lvl1$theoretical_icc == icc[i]])
      intercept_mean_mlm[i] <- mean(df$beta_0[test_lvl1$method == "mlm" & test_lvl1$theoretical_icc == icc[i]])
      treatment_mean_lm[i] <- mean(df$beta_treatment[test_lvl1$method == "lm" & test_lvl1$theoretical_icc == icc[i]])
      treatment_mean_mlm[i] <- mean(df$beta_treatment[test_lvl1$method == "mlm" & test_lvl1$theoretical_icc == icc[i]])
    }
    
    methods <- rep(c("lm", "mlm"), each = 9)
    icc_df <- rep(icc, times = 2)
    
    temp_m <- matrix(c(intercept_mean_lm, intercept_mean_mlm, treatment_mean_lm, treatment_mean_mlm, methods, icc_df), ncol = 4)
    mean_dataframe <- data.frame(temp_m)
    colnames(mean_dataframe) <- c("intercept_mean", "treatment_mean", "method", "icc")
    mean_dataframe[,1:2] <- apply(mean_dataframe[,1:2], 2, as.character)
    mean_dataframe[,1:2] <- apply(mean_dataframe[,1:2], 2, as.numeric)
    
    return(mean_dataframe)
  }
  
  parameter_efficacy <- function(df){
    icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
    mean_par <- mean_parameters(df)
    
    intercept_efficacy <- mean_par$intercept_mean / 15
    treatment_efficacy <- mean_par$treatment_mean / 0.35
    
    methods <- rep(c("lm", "mlm"), each = 9)
    icc_df <- rep(icc, times = 2)
    
    temp_m <- matrix(c(intercept_efficacy, treatment_efficacy, methods, icc_df), ncol = 4)
    
    par_efficacy_df <- data.frame(temp_m)
    colnames(par_efficacy_df) <- c("intercept_efficacy", "treatment_efficacy", "method", "icc")
    par_efficacy_df[,1:2] <- apply(par_efficacy_df[,1:2], 2, as.character)
    par_efficacy_df[,1:2] <- apply(par_efficacy_df[,1:2], 2, as.numeric)
    
    return(par_efficacy_df)
  }
  
  mean_se <- function(df){
    icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
    se_0_mean_lm <- c()
    se_0_mean_mlm <- c()
    se_treatment_mean_lm <- c()
    se_treatment_mean_mlm <- c()
    
    for(i in 1:length(icc)){
      se_0_mean_lm[i] <-  mean(df$SE_beta_0[df$method == "lm"  & df$theoretical_icc == icc[i]])
      se_0_mean_mlm[i] <- mean(df$SE_beta_0[df$method == "mlm" & df$theoretical_icc == icc[i]])
      se_treatment_mean_lm[i] <-  mean(df$SE_beta_treatment[df$method == "lm"  & df$theoretical_icc == icc[i]])
      se_treatment_mean_mlm[i] <- mean(df$SE_beta_treatment[df$method == "mlm" & df$theoretical_icc == icc[i]])
    }
    
    methods <- rep(c("lm", "mlm"), each = 9)
    icc_df <- rep(icc, times = 2)
    
    temp_m <- matrix(c(se_0_mean_lm, se_0_mean_mlm, se_treatment_mean_lm, se_treatment_mean_mlm, methods, icc_df), ncol = 4)
    mean_se_dataframe <- data.frame(temp_m)
    colnames(mean_se_dataframe) <- c("mean_se_intercept", "mean_se_treatment", "method", "icc")
    mean_se_dataframe[,1:2] <- apply(mean_se_dataframe[,1:2], 2, as.character)
    mean_se_dataframe[,1:2] <- apply(mean_se_dataframe[,1:2], 2, as.numeric)
    
    return(mean_se_dataframe)
  }
  
  sd_coefs <- function(df){
    icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
    sd_0_lm <- c()
    sd_0_mlm <- c()
    sd_treatment_lm <- c()
    sd_treatment_mlm <- c()
    
    for(i in 1:length(icc)){
      sd_0_lm[i] <- sd(df$beta_0[df$method == "lm" & df$theoretical_icc == icc[i]])
      sd_0_mlm[i] <- sd(df$beta_0[df$method == "mlm" & df$theoretical_icc == icc[i]])
      sd_treatment_lm[i] <- sd(df$beta_treatment[df$method == "lm" & df$theoretical_icc == icc[i]])
      sd_treatment_mlm[i] <- sd(df$beta_treatment[df$method == "mlm" & df$theoretical_icc == icc[i]])
    }
    
    methods <- rep(c("lm", "mlm"), each = 9)
    icc_df <- rep(icc, times = 2)
    
    temp_m <- matrix(c(sd_0_lm, sd_0_mlm, sd_treatment_lm, sd_treatment_mlm, methods, icc_df), ncol = 4)
    mean_sd_dataframe <- data.frame(temp_m)
    colnames(mean_sd_dataframe) <- c("sd_intercept", "sd_treatment", "method", "icc")
    mean_sd_dataframe[,1:2] <- apply(mean_sd_dataframe[,1:2], 2, as.character)
    mean_sd_dataframe[,1:2] <- apply(mean_sd_dataframe[,1:2], 2, as.numeric)
    
    return(mean_sd_dataframe)
  }
  
  se_efficacy <- function(df){
    icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
    
    se_means <- mean_se(df)
    coefs_sd <- sd_coefs(df)
    
    se_efficacy_intercept <- se_means$mean_se_intercept / coefs_sd$sd_intercept
    se_efficacy_treatment <- se_means$mean_se_treatment / coefs_sd$sd_treatment
    
    methods <- rep(c("lm", "mlm"), each = 9)
    icc_df <- rep(icc, times = 2)
    
    temp_m <- matrix(c(se_efficacy_intercept, se_efficacy_treatment, methods, icc_df), ncol = 4)
    
    se_efficacy_df <- data.frame(temp_m)
    colnames(se_efficacy_df) <- c("intercept_efficacy", "treatment_efficacy", "method", "icc")
    se_efficacy_df[,1:2] <- apply(se_efficacy_df[,1:2], 2, as.character)
    se_efficacy_df[,1:2] <- apply(se_efficacy_df[,1:2], 2, as.numeric)
    
    return(se_efficacy_df)
  }
  
  
sim_data <- eventReactive(c(input$sim_study, input$results),{
  switch(input$sim_study,
         "lvl1" = readRDS("test_lvl1"),
         "lvl2" = readRDS("test_lvl2"))
})

parameter_efficacy_df <- eventReactive(input$results, {
  parameter_efficacy(sim_data())
})

output$parameter_eff <- renderPlot({
  ggplot(data = parameter_efficacy_df(), aes(y = treatment_efficacy, x = method, fill = method))+
    geom_col() +
    geom_hline(yintercept = 1, size = 1) +
    facet_grid(~ icc) + 
    labs(title = "Parameter Effizienz")
})

se_efficacy_df <- eventReactive(input$results, {
  se_efficacy(sim_data())
})

output$se_eff <- renderPlot({
  ggplot(data = se_efficacy_df(), aes(y = treatment_efficacy, x = method, fill = method))+
    geom_col() +
    geom_hline(yintercept = 1, size = 1) +
    facet_grid(~ icc) + 
    labs(title = "Parameter Effizienz")
})
  

}
shinyApp(ui = ui, server = server)
