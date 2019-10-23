library(shiny)
library(shinyjs) 
library(lme4)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

ui <- navbarPage(title = "Multilevel Analyse von genesteten Daten", 
tabPanel(title = "Einführung",
         tags$head(
           tags$style(HTML("hr{border-top: 1px solid #95a5a6})")
                      )
           ),
         p("Kurze Einführung in die Thematik und vorstellen eines Beispiels z.B. Schüler in Klassen, die eine Prüfung schreiben müssen..")
         ),
tabPanel(title = "Grafiken",
         sidebarLayout(
           sidebarPanel(
             h4("Parameter"),
             sliderInput(inputId = "n_total",
                          label = "Gesamtanzahl an Schülern",
                          value = 240,
                          min = 100,
                          max = 1000,
                          step = 10),
             
             hr(),
             
             sliderInput(inputId = "n_klassen",
                          label = "Anzahl Klassen",
                          value = 8,
                          min = 2,
                          max = 20),
             
             hr(),
             
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
             checkboxInput(inputId = "corr_slope",
                           label = "Korrelierter Slope",
                           value = FALSE)
           ),
           mainPanel(
             plotOutput(outputId = "multiplot"),
             h4("Summary Output"),
             verbatimTextOutput(outputId = "summary", placeholder = TRUE),
             verbatimTextOutput(outputId = "test", placeholder = TRUE)
           )
           )
         )
)

server <- function(input, output) {
  
  # Laden der richtigen Funktion
  observeEvent(input$corr_slope, {
    if (input$corr_slope == FALSE){
      source("uncorr_ml.R")
    } else {
      source("corr_ml.R")
    }
  })

  data_model <- reactive(ran_inter(n = input$n_total, nklassen = input$n_klassen, sd_intercept = input$int_sd, sd_slope = input$slope_sd))
  
  # Model und Koeffizienten berechnen
  
  ri_model <- reactive({
    if (input$slope_sd == 0) {
    lmer(leistung ~ stunden + (1|klasse), data = data_model())
   } else {
    lmer(leistung ~ stunden + (stunden|klasse), data = data_model())
   }
  })
  
  intercept <- reactive(coef(ri_model())$klasse[,1])
  slope <- reactive(coef(ri_model())$klasse[,2])
  
  
  # Ploten der Grafik
  output$multiplot <- renderPlot({
    ggplot(data = data_model(), mapping = aes(x = stunden, y = leistung, color = klasse)) + 
      geom_point() +
      scale_color_viridis_d() +
      geom_abline(slope = slope(), intercept = intercept(), col = viridis(n = input$n_klassen)) +
      geom_abline(slope = mean(slope()), intercept = mean(intercept()), col = "red", size = 1) +
      labs(x = "Anzahl Lernstunden", y = "Anzahl Punkte", title = "Erreichte Punktzahl nach Klassen") +
      ylim(0,NA)
    
  })
  
  # PLot des Summarys
  output$summary <- renderPrint({
    summary(ri_model())
    # VarCorr(ri_model())
    
  })
   
}

shinyApp(ui = ui, server = server)
