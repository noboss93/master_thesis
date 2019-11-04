library(shiny)
library(shinyjs) 
library(lme4)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(MASS)

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
                           h4("Grafik"),
                           plotOutput(outputId = "multiplot")
                         ),
                         tabPanel(
                           h4("Summary Output"),
                           verbatimTextOutput(outputId = "summary", placeholder = TRUE)
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
    ran_inter(n = 240, nklassen = 8, sd_intercept = input$int_sd, sd_slope = input$slope_sd, corr = input$corr)
    })


  
  # Model und Koeffizienten berechnen
  ri_model <- eventReactive(c(input$method, input$gen_data), {
    switch(input$method,
           "lm" = lm(leistung ~ stunden, data = data_model()),
           "ri" = lmer(leistung ~ stunden + (1|klasse), data = data_model()),
           "rs" = lmer(leistung ~ stunden + (stunden|klasse), data = data_model())
    )
  })
  
  intercept <- eventReactive(c(input$method, input$gen_data), {
    if (input$method == "lm"){
      coef(ri_model())[1]
    } else {
      coef(ri_model())$klasse[,1]
    }
  })
  
  slope <- eventReactive(c(input$method, input$gen_data), {
  if (input$method == "lm"){
    coef(ri_model())[2]
  } else {
    coef(ri_model())$klasse[,2]
  }
  })
  
  
  # Ploten der Grafik
  output$multiplot <- renderPlot({
    if (input$method == "lm"){
      ggplot(data = data_model(), mapping = aes(x = stunden, y = leistung, color = klasse)) + 
        geom_point() +
        scale_color_viridis_d() +
        geom_abline(slope = slope(), intercept = intercept(), col = "red", size = 1) +
        labs(x = "Anzahl Lernstunden", y = "Anzahl Punkte", title = "Erreichte Punktzahl nach Klassen") +
        ylim(0,NA)  
    } else {
      ggplot(data = data_model(), mapping = aes(x = stunden, y = leistung, color = klasse)) + 
        geom_point() +
        scale_color_viridis_d() +
        geom_abline(slope = slope(), intercept = intercept(), col = viridis(n = 8)) +
        geom_abline(slope = mean(slope()), intercept = mean(intercept()), col = "red", size = 1) +
        labs(x = "Anzahl Lernstunden", y = "Anzahl Punkte", title = "Erreichte Punktzahl nach Klassen") +
        ylim(0,NA)
    }
  })
  
  # PLot des Summarys
  output$summary <- renderPrint({
    summary(ri_model())
    # VarCorr(ri_model())
    
  })
   
}

shinyApp(ui = ui, server = server)
