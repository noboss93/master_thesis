library(shiny)
library(shinyjs) 
library(lme4)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

ui <- navbarPage(title = "Multilevel Analyse von genesteten Daten", 
tabPanel(title = "Einführung",
         p("Kurze Einführung in die Thematik und vorstellen eines Beispiels z.B. Schüler in Klassen, die eine Prüfung schreiben müssen..")
         ),
tabPanel(title = "Grafiken",
         sidebarLayout(
           sidebarPanel(
             sliderInput(inputId = "n_total",
                          label = "Gesamtanzahl an Schülern",
                          value = 240,
                          min = 100,
                          max = 1000,
                          step = 10),
             
             sliderInput(inputId = "n_klassen",
                          label = "Anzahl Klassen",
                          value = 8,
                          min = 2,
                          max = 20),
             
             sliderInput(inputId = "int_sd",
                         label = "Standardabweichung des Intercepts",
                         min = 0,
                         max = 30,
                         value = 10),
             sliderInput(inputId = "slope_sd",
                         label = "Standardabweichung des Slopes",
                         min = 0, 
                         max = 5,
                         value = 0),
             checkboxInput(inputId = "corr_slope",
                           label = "Korrelierter Slope",
                           value = FALSE)

             
           ),
           mainPanel(
             plotOutput(outputId = "multiplot"),
             verbatimTextOutput(outputId = "summary", placeholder = TRUE)
           )
           )
         )
)

server <- function(input, output) {
  
  # Funktion zur generierung der Daten
  ran_inter <- function(n = 240, nklassen = 8, sd_intercept = 10, sd_slope = 0){
    stunden <- round(runif(n, 1, 30), digits = 0)
    klasse <- sample(1:nklassen, n, replace = TRUE)
    effekt_int <- round(rnorm(nklassen, 0, sd_intercept), digits = 1)
    # effekt_int <- sort(effekt_int)
    effekt_slope <- round(rnorm(nklassen, 0, sd_slope), digits = 1)
    # effekt_slope <- sort(effekt_slope)
    
    random_intercept <- numeric(n)
    for (i in 1:n){
      random_intercept[i] <- effekt_int[klasse[i]]
    }
    
    random_slope <- numeric(n)
    for (i in 1:n){
      random_slope[i] <- effekt_slope[klasse[i]]
    }
    
    error <- round(rnorm(n, 0, 5), digits = 1)
    leistung <- round(20 + 2.5 * stunden + random_intercept + random_slope * stunden + error, digits = 0)
    # leistung <- ifelse(leistung > 100, 100, leistung)
    # leistung <- ifelse(leistung < 0, 0, leistung)
    # Als Fussnote erwähnen im Beispiel
    
    klasse <- as.factor(klasse)
    levels(klasse) <- paste(1:nklassen, "md", sep = "")
    
    ranint_data <- data.frame(stunden, klasse, leistung)
    return(ranint_data)
  }
  
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
