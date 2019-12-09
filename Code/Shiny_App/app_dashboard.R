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
  dashboardHeader(title = "Multilevel Analyse", titleWidth = 300),
  dashboardSidebar(width = 300,
    sidebarMenu(
      menuItem("Einführung", tabName = "einführung", startExpanded = TRUE,
               menuSubItem("Was ist eine Multilevel Analyse", tabName = "intro"),
               menuSubItem("Genestete Daten generieren", tabName = "generating"),
               menuSubItem("Daten analysieren", tabName = "analysing")
               ),
      menuItem("Eigene Multilevel Analyse", tabName = "own_mla", startExpanded = TRUE,
               menuSubItem("Informationen zum Datensatz", tabName = "infos"),
               menuSubItem("1. Schritt: Forschungsfrage", tabName = "researchquestion"),
               menuSubItem("2. Schritt: Wahl des Schätzers", tabName = "estimator"),
               menuSubItem("3. Schritt: Notwendigkeit von Multilevel Analyse", tabName = "needformla"),
               menuSubItem("4. Schritt: Erstellen eines Level-1 Modells", tabName = "lvl1"),
               menuSubItem("5. Schritt: Erstellen eines Level-2 Modells", tabName = "lvl2"),
               menuSubItem("6. Schritt: Effektgrössen", tabName = "effectsize"),
               menuSubItem("7. Schritt: Modelltestung", tabName = "modeltesting")
              )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              "Hier kommt eine Einführung"
              ),
    tabItem(tabName = "generating",
            fluidRow(
              column(width = 3,
            sliderInput(inputId = "int_sd",
                        label = "Standardabweichung des Intercepts",
                        min = 0,
                        max = 30,
                        value = 10),
            sliderInput(inputId = "slope_sd",
                        label = "Standardabweichung des Slopes",
                        min = 0, 
                        max = 5,
                        value = 0,
                        step = 0.1),
            sliderInput(inputId = "corr",
                        label = "Korrelation zwischen Slope und Intercept",
                        min = -1,
                        max = 1,
                        value = 0,
                        step = 0.1),
            actionButton(inputId = "gen_data",
                         label = "Datensatz generieren")
            ),
            column(width = 9,
                   tabsetPanel(type = "tabs",
                               tabPanel(
                                 "Übersicht der Daten",
                                 dataTableOutput(outputId = "table")
                                 ),
                               tabPanel(
                                 "Struktur der Daten",
                                 verbatimTextOutput(outputId = "str", 
                                                    placeholder = FALSE)
                                 ),
                               tabPanel(
                                 "Summary der Daten",
                                 verbatimTextOutput(outputId = "data_summary", 
                                                    placeholder = FALSE)
                                 )
                               )
                   )
            )
    ),
    tabItem(tabName = "analysing",
            selectInput(inputId = "method",
                        label = "Wähle eine Methode aus:",
                        choices = c("Lineares Regressionsmodell" = "lm", 
                                    "Random Intercept Modell" = "ri", 
                                    "Random Intercept und Slope Modell" = "rs"),
                        multiple = FALSE),
            tabsetPanel(type = "tabs",
                        tabPanel(
                          "Regressions Geraden",
                          plotOutput(outputId = "multiplot")
                          ),
                        tabPanel(
                          "Residuen Plot",
                          plotOutput(outputId = "residual")
                          ),
                        tabPanel(
                          "Q-Q Plot",
                          plotOutput(outputId = "qq")
                          ),
                        tabPanel(
                          "Summary Output",
                          verbatimTextOutput(outputId = "summary", 
                                             placeholder = FALSE)
                          )
                        )
              
            
            ),
    tabItem(tabName = "own_mla",
            "Hier Einführung zum Datensatz")
    )
  )
)


server <- function(input, output) {
  
  # Laden der richtigen Funktion
  source("dgp_multi_ml.R")
  
  # Generieren des Datensatzes
  data_model  <- eventReactive(input$gen_data, {
    gen_ml_data(n = 240, nklassen = 8, sd_intercept = input$int_sd, 
              sd_slope = input$slope_sd, corr = input$corr)
    })
  
  # Model und Koeffizienten berechnen
  model <- eventReactive(c(input$method, input$gen_data), {
    switch(input$method,
           "lm" = lm(leistung ~ stunden, data = data_model()),
           "ri" = lmer(leistung ~ stunden + (1|klasse), data = data_model()),
           "rs" = lmer(leistung ~ stunden + (stunden|klasse), data = data_model())
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
  
  # Plotten der Regressions Geraden
  output$multiplot <- renderPlot({
    if (input$method == "lm"){
      ggplot(data = data_model(), mapping = aes(x = stunden, y = leistung, color = klasse)) + 
        geom_point(size = 2) +
        scale_color_viridis_d() +
        geom_abline(slope = slope(), intercept = intercept(), col = "red", size = 1) +
        labs(x = "Anzahl Lernstunden", y = "Anzahl Punkte") +
        ylim(0,NA)  
    } else {
      ggplot(data = data_model(), mapping = aes(x = stunden, y = leistung, color = klasse)) + 
        geom_point(size = 2) +
        scale_color_viridis_d() +
        geom_abline(slope = slope(), intercept = intercept(), col = viridis(n = 8), size = 1) +
        geom_abline(slope = mean(slope()), 
                    intercept = mean(intercept()), col = "red", size = 1) +
        labs(x = "Anzahl Lernstunden", y = "Anzahl Punkte") +
        ylim(0,NA)
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
   
}

shinyApp(ui = ui, server = server)
