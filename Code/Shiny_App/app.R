library(shiny)
library(shinyjs) 
library(shinydashboard)
library(lme4)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(MASS)

ui <- navbarPage(title = "Multilevel Analyse", collapsible = TRUE,
tabPanel(title = "Einführung",
         tags$head(
           tags$style(HTML("hr{border-top: 1px solid #95a5a6})")
                      )
           ),
         p("Kurze Einführung in die Thematik und vorstellen eines 
           Beispiels z.B. Schüler in Klassen, die eine Prüfung schreiben müssen..")
         ),
tabPanel(title = "Grafiken",
         sidebarLayout(
           sidebarPanel(
             h4("Datensatz Eigenschaften"),
             # sliderInput(inputId = "n_total",
             #              label = "Gesamtanzahl an Schülern",
             #              value = 240,
             #              min = 100,
             #              max = 1000,
             #              step = 10),
             # 
             # hr(),
             # 
             # sliderInput(inputId = "n_klassen",
             #              label = "Anzahl Klassen",
             #              value = 8,
             #              min = 2,
             #              max = 20),
             # 
             # hr(),
             
             sliderInput(inputId = "int_sd",
                         label = "Standardabweichung des Intercepts",
                         min = 0,
                         max = 30,
                         value = 10),
             
             hr(), 
             
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
                          label = "Datensatz generieren"),
             hr(),
             h4("Analysemethoden"),
             selectInput(inputId = "method",
                         label = "Wähle eine Methode aus:",
                         choices = c("Lineares Regressionsmodell" = "lm", 
                                     "Random Intercept Modell" = "ri", 
                                     "Random Intercept und Slope Modell" = "rs"),
                         multiple = FALSE)
           ),
           mainPanel(
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
           )
           )
         )
)

server <- function(input, output) {
  
  # Laden der richtigen Funktion
  source("corr_ml.R")
  
  # Generieren des Datensatzes
  data_model  <- eventReactive(input$gen_data, {
    ran_inter(n = 240, nklassen = 8, sd_intercept = input$int_sd, 
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
      labs(x = "Fitted Values", y = "Residuals")
  })
  
  # Plot des QQ-Plots
  output$qq <- renderPlot({
    residual_df <- data.frame(residuals(model()), fitted.values(model()), data_model()$klasse)
    colnames(residual_df) <- c("residuals", "fitted", "klasse")
    
    ggplot(residual_df, aes(sample = residuals)) +
      geom_qq(shape = 1, size = 3) +
      geom_qq_line() + 
      labs(x = "Standardisierte Residuen", y = "Theoretische Quantile")
  })
  
  # PLot des Summarys
  output$summary <- renderPrint({
    summary(model())
  })
   
}

shinyApp(ui = ui, server = server)
