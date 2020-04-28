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
      menuItem("Einführung in Hierarchische lineare Modelle", startExpanded = TRUE,
               menuSubItem("Theoretischer Hintergrund", tabName = "intro", selected = TRUE),
               menuSubItem("Beispiel einer Multilevel Analyse mit HLM", tabName = "analysing")
               ),
      menuItem("Simulationsstudie", startExpanded = FALSE,
               menuSubItem("Forschungsfrage und Studiendesign", tabName = "studyquestion"),
               menuSubItem("Studie 1: Genauigkeit von Schätzparametern", tabName = "study1"),
               menuSubItem("Studie 2: Statistische Power von HLM", tabName = "study2"))
      
      )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
    tabItem(tabName = "intro",
            source("tab_intro.R", encoding = "utf8")[1]),
   tabItem(tabName = "analysing", 
            source("tab_analysing_intro.R", encoding = "utf8")[1]),
    tabItem(tabName = "studyquestion", 
            source("tab_simstudy_app.R", encoding = "utf8")[1]),
    tabItem(tabName = "study1", 
            source("tab_study_1.R", encoding = "utf8")[1]),
    tabItem(tabName = "study2", 
            source("tab_study_2.R", encoding = "utf8")[1])
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
  

# Intro Plots -------------------------------------------------------------
  
  observeEvent(c(input$method, input$gen_data), {
  if (input$method == "lm"){
  } else {
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
        labs(x = "Anzahl gelöste Übungen", y = "Anzahl Punkte", color = "Klasse")
          
      } else {
        ggplot(data = data_model(), mapping = aes(x = uebung, y = punktzahl)) + 
          geom_point(size = 2) +
          geom_abline(slope = slope(), intercept = intercept(), col = "red", size = 1) +
          labs(x = "Anzahl gelöste Übungen", y = "Anzahl Punkte")
          
      }
    } else {
      if (input$groupcolor == TRUE){
      ggplot(data = data_model(), mapping = aes(x = uebung, y = punktzahl, color = klasse)) + 
        geom_point(size = 2) +
        scale_color_viridis_d() +
        scale_fill_discrete(name = "Gruppe") +
        geom_abline(slope = slope(), intercept = intercept(), col = viridis(n = 8), size = 1) +
        geom_abline(slope = mean(slope()), 
                    intercept = mean(intercept()), col = "red", size = 1) +
        labs(x = "Anzahl gelöste Übungen", y = "Anzahl Punkte", color = "Klasse")
      } else {
        ggplot(data = data_model(), mapping = aes(x = uebung, y = punktzahl)) + 
          geom_point(size = 2) +
          geom_abline(slope = slope(), intercept = intercept(), size = 1) +
          geom_abline(slope = mean(slope()), 
                      intercept = mean(intercept()), col = "red", size = 1) +
          labs(x = "Anzahl gelöste Übungen", y = "Anzahl Punkte")
        
      }
    }
  })
  
  # Plotten der Residuenplots
  output$residual <- renderPlot({
    if (input$method == "lm"){
      if (input$groupcolor == TRUE){
    residual_df <- data.frame(residuals(model()), fitted.values(model()), data_model()$klasse)
    colnames(residual_df) <- c("residuals", "fitted", "klasse")
    
    ggplot(residual_df, aes(x = fitted, y = residuals, color = klasse)) +
      geom_point(shape = 16, size = 2) +
      scale_color_viridis_d() +
      geom_hline(yintercept = 0, col = "black") + 
      geom_smooth(method = "loess", se = FALSE, col = "red") +
      labs(x = "Angepasster Wert", y = "Residuen")
      } else {
        residual_df <- data.frame(residuals(model()), fitted.values(model()), data_model()$klasse)
        colnames(residual_df) <- c("residuals", "fitted", "klasse")
        
        ggplot(residual_df, aes(x = fitted, y = residuals)) +
          geom_point(shape = 16, size = 2) +
          scale_color_viridis_d() +
          geom_hline(yintercept = 0, col = "black") + 
          geom_smooth(method = "loess", se = FALSE, col = "red") +
          labs(x = "Angepasster Wert", y = "Residuen")
      }
    } else {
      if (input$groupcolor == TRUE){
      residual_df <- data.frame(residuals(model()), fitted.values(model()), data_model()$klasse)
      colnames(residual_df) <- c("residuals", "fitted", "klasse")
      
      ggplot(residual_df, aes(x = fitted, y = residuals, color = klasse)) +
        geom_point(shape = 16, size = 2) +
        scale_color_viridis_d() +
        geom_hline(yintercept = 0, col = "black") + 
        geom_smooth(method = "loess", se = FALSE, col = "red") +
        labs(x = "Angepasster Wert", y = "Residuen")
      } else {
        residual_df <- data.frame(residuals(model()), fitted.values(model()), data_model()$klasse)
        colnames(residual_df) <- c("residuals", "fitted", "klasse")
        
        ggplot(residual_df, aes(x = fitted, y = residuals)) +
          geom_point(shape = 16, size = 2) +
          geom_hline(yintercept = 0, col = "black") + 
          geom_smooth(method = "loess", se = FALSE, col = "red") +
          labs(x = "Angepasster Wert", y = "Residuen")
        
      }
    }
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
   

# Sim Study Functions and Data ----------------------------------------------------------------
  simstudy_lvl1 <- readRDS("simstudy_lvl1")
  simstudy_lvl2 <- readRDS("simstudy_lvl2")
  simstudy_lvl1_small <- readRDS("simstudy_lvl1_small")
  simstudy_lvl2_small <- readRDS("simstudy_lvl2_small")
  paper_colors <- c("darkgrey", "#B01111")
  
  
  parameter_spread <- function(df){
    intercept_spread_lm <- c()
    intercept_spread_mlm <- c()
    treatment_spread_lm <- c()
    treatment_spread_mlm <- c()
    icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
    
    for(i in 1:length(icc)){
      intercept_spread_lm[i] <- var((df$beta_0[df$method == "lm" & df$icc == icc[i]] - 2.23)/2.23)
      intercept_spread_mlm[i] <- var((df$beta_0[df$method == "mlm" & df$icc == icc[i]] - 2.23)/2.23)
      treatment_spread_lm[i] <- var((df$beta_treatment[df$method == "lm" & df$icc == icc[i]] - 0.12)/0.12)
      treatment_spread_mlm[i] <- var((df$beta_treatment[df$method == "mlm" & df$icc == icc[i]] - 0.12)/0.12)
    }
    methods <- rep(c("lm", "mlm"), each = length(icc))
    icc_df <- rep(icc, times = 2)
    
    temp_m <- matrix(c(intercept_spread_lm, intercept_spread_mlm, treatment_spread_lm, treatment_spread_mlm, methods, icc_df), ncol = 4)
    spread_dataframe <- data.frame(temp_m)
    colnames(spread_dataframe) <- c("intercept_var", "treatment_var", "method", "icc")
    spread_dataframe[,1:2] <- apply(spread_dataframe[,1:2], 2, as.character)
    spread_dataframe[,1:2] <- apply(spread_dataframe[,1:2], 2, as.numeric)
    
    return(spread_dataframe)
  }
  
  mean_parameters <- function(df){
    intercept_mean_lm <- c()
    intercept_mean_mlm <- c()
    treatment_mean_lm <- c()
    treatment_mean_mlm <- c()
    icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
    
    for(i in 1:length(icc)){
      intercept_mean_lm[i] <- mean(df$beta_0[df$method == "lm" & df$icc == icc[i]])
      intercept_mean_mlm[i] <- mean(df$beta_0[df$method == "mlm" & df$icc == icc[i]])
      treatment_mean_lm[i] <- mean(df$beta_treatment[df$method == "lm" & df$icc == icc[i]])
      treatment_mean_mlm[i] <- mean(df$beta_treatment[df$method == "mlm" & df$icc == icc[i]])
    }
    
    methods <- rep(c("lm", "mlm"), each = length(icc))
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
    par_var <- parameter_spread(df)
    
    intercept_efficacy <- (mean_par$intercept_mean - 2.34) / 2.34
    treatment_efficacy <- (mean_par$treatment_mean - 0.12) / 0.12
    
    methods <- rep(c("lm", "mlm"), each = length(icc))
    icc_df <- rep(icc, times = 2)
    
    temp_m <- matrix(c(icc_df, intercept_efficacy, par_var$intercept_var, treatment_efficacy, 
                       par_var$treatment_var, methods), ncol = 6)
    
    par_efficacy_df <- data.frame(temp_m)
    colnames(par_efficacy_df) <- c("icc", "intercept_efficacy", "intercept_var", "treatment_efficacy", "treatment_var", "method")
    par_efficacy_df[,2:5] <- apply(par_efficacy_df[,2:5], 2, as.character)
    par_efficacy_df[,2:5] <- apply(par_efficacy_df[,2:5], 2, as.numeric)
    
    return(par_efficacy_df)
  }
  
  
  mean_se <- function(df){
    icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
    se_0_mean_lm <- c()
    se_0_mean_mlm <- c()
    se_treatment_mean_lm <- c()
    se_treatment_mean_mlm <- c()
    
    for(i in 1:length(icc)){
      se_0_mean_lm[i] <-  mean(df$SE_beta_0[df$method == "lm"  & df$icc == icc[i]])
      se_0_mean_mlm[i] <- mean(df$SE_beta_0[df$method == "mlm" & df$icc == icc[i]])
      se_treatment_mean_lm[i] <-  mean(df$SE_beta_treatment[df$method == "lm"  & df$icc == icc[i]])
      se_treatment_mean_mlm[i] <- mean(df$SE_beta_treatment[df$method == "mlm" & df$icc == icc[i]])
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
      sd_0_lm[i] <- sd(df$beta_0[df$method == "lm" & df$icc == icc[i]])
      sd_0_mlm[i] <- sd(df$beta_0[df$method == "mlm" & df$icc == icc[i]])
      sd_treatment_lm[i] <- sd(df$beta_treatment[df$method == "lm" & df$icc == icc[i]])
      sd_treatment_mlm[i] <- sd(df$beta_treatment[df$method == "mlm" & df$icc == icc[i]])
    }
    
    methods <- rep(c("lm", "mlm"), each = length(icc))
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
    
    se_efficacy_intercept <- (se_means$mean_se_intercept - coefs_sd$sd_intercept) / coefs_sd$sd_intercept
    se_efficacy_treatment <- (se_means$mean_se_treatment - coefs_sd$sd_treatment) / coefs_sd$sd_treatment
    
    methods <- rep(c("lm", "mlm"), each = length(icc))
    icc_df <- rep(icc, times = 2)
    
    temp_m <- matrix(c(se_efficacy_intercept, se_efficacy_treatment, methods, icc_df), ncol = 4)
    
    se_efficacy_df <- data.frame(temp_m)
    colnames(se_efficacy_df) <- c("intercept_efficacy", "treatment_efficacy", "method", "icc")
    se_efficacy_df[,1:2] <- apply(se_efficacy_df[,1:2], 2, as.character)
    se_efficacy_df[,1:2] <- apply(se_efficacy_df[,1:2], 2, as.numeric)
    
    return(se_efficacy_df)
  }
  
  power_model <- function(df){
    icc <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5)
    power_intercept_lm <- c()
    power_intercept_mlm <- c()
    power_treatment_lm <- c()
    power_treatment_mlm <- c()
    
    for(i in 1:length(icc)){
      power_intercept_lm[i] <- length(df$p_value_0[df$method == "lm" & df$icc == icc[i] & df$p_value_0 < .05])/1000
      power_intercept_mlm[i] <- length(df$p_value_0[df$method == "mlm" & df$icc == icc[i] & df$p_value_0 < .05])/1000
      power_treatment_lm[i] <- length(df$p_value_treatment[df$method == "lm" & df$icc == icc[i] & df$p_value_treatment < .05])/1000
      power_treatment_mlm[i] <- length(df$p_value_treatment[df$method == "mlm" & df$icc == icc[i] & df$p_value_treatment < .05])/1000
      
    }
    
    methods <- rep(c("lm", "mlm"), each = 9)
    icc_df <- rep(icc, times = 2)
    
    temp_m <- matrix(c(power_intercept_lm, power_intercept_mlm, power_treatment_lm, power_treatment_mlm, methods, icc_df), ncol = 4)
    power_dataframe <- data.frame(temp_m)
    colnames(power_dataframe) <- c("power_intercept", "power_treatment", "method", "icc")
    power_dataframe[,1:2] <- apply(power_dataframe[,1:2], 2, as.character)
    power_dataframe[,1:2] <- apply(power_dataframe[,1:2], 2, as.numeric)
    
    return(power_dataframe)
  }
  
 

# Study 1 -----------------------------------------------------------------
  outcomeNames <- c("Relative Abweichung" = "rel_bias", "SE Genauigkeit" = "se_eff")
  designNames <- c("bei einer Intervention auf Level-1" = "lvl1", "bei einer Intervention auf Level-2" = "lvl2")
  coefNames <- c("des Achsenabschnitts" = "intercept", "der Steigung (Intervention)" = "treatment")
  
  sim_data_1 <- reactive({
    switch(input$design_cond_1,
           "lvl1" = simstudy_lvl1,
           "lvl2" = simstudy_lvl2)
  })
  
  coef_data <- reactive({
    if (input$outcomes_1 == "se_eff"){
      se_efficacy(sim_data_1())
    }else{
      parameter_efficacy(sim_data_1())
    }
  })
  
  
  results_study1 <- reactive({
      switch(input$outcomes_1,
             "rel_bias" = "results_coefs_1.Rmd",
             "se_eff" = "results_se_1.Rmd")
  })
  
  output$mrkdwn <- renderUI({
    withMathJax(includeMarkdown(results_study1()))
  })

  output$se_eff <- renderPlot({
   ggplot(data = coef_data(), aes_string(y = paste(input$coef_cond_1, "_efficacy", sep = ""), 
                                              x = "icc", 
                                              fill = "method"))+
    geom_col(position = "dodge2") +
    geom_hline(yintercept = 0)+
    geom_hline(yintercept = 0.1, linetype = "dashed")+
    geom_hline(yintercept = -0.1, linetype = "dashed")+
    geom_hline(yintercept = 0.05, linetype = "dotted")+
    geom_hline(yintercept = -0.05, linetype = "dotted")+
    scale_fill_manual(values = paper_colors, name = "Methode", labels = c("LM", "HLM")) + 
    scale_y_continuous(breaks=seq(-1,1, 0.05)) +
    theme_gray(base_size = 15) +
    labs(title = paste(names(outcomeNames)[outcomeNames == input$outcomes_1], 
                       names(coefNames)[coefNames == input$coef_cond_1], 
                       names(designNames)[designNames == input$design_cond_1]))+
    xlab("IKK")+
    ylab(paste(names(outcomeNames)[outcomeNames == input$outcomes_1]))
  })


# Study 2  --------------------------------------------------------

  sim_data_small <- reactive({
    switch(input$design_cond_2,
           "lvl1" = simstudy_lvl1_small,
           "lvl2" = simstudy_lvl2_small)
  })
  
  power_data <- reactive({
   power_model(sim_data_small())
  })
  
  output$power <- renderPlot({
    ggplot(data = power_data(), mapping = aes(y = power_treatment, x = icc, fill = method))+
    geom_col(position = "dodge2") +
    scale_fill_manual(values = paper_colors, name = "Methode", labels = c("LM", "HLM")) +
    scale_y_continuous(breaks=seq(0,1, 0.05), limits = c(0,1)) +
    geom_hline(yintercept = 0.8, linetype = "dashed")+
    theme_gray(base_size = 15) +
    labs(title = "Power beider Analysemethoden")+
    xlab("IKK")+
    ylab("Power")
  })


}
shinyApp(ui = ui, server = server)
