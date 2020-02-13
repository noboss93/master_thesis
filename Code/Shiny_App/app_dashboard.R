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
            source("simstudy_app.R", encoding = "utf8")[1])
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
   

# MLA Info ----------------------------------------------------------------

  
  # Ausgabe der Tabelle
  output$table_full <- renderDataTable({
    data_model()
  })
  
  # Ausgabe der Struktur
  output$str_full <- renderPrint({
    str(data_model())
  })
  
  # Ausgabe der Summary
  output$data_summary_full <- renderPrint({
    summary(data_model())
  })  
  
}

shinyApp(ui = ui, server = server)
