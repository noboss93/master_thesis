fluidRow(
  box(width = 12,
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
  box(width = 3, title = "Info-Box", collapsible = TRUE,
        "Hier stehen Informationen für den User, was die einzelnen Einstellungen bedeuten und 
      wie die gegebenen Outputs zu interpretieren sind.",
        
        textOutput(outputId = "icc")
  ),
  tabBox(width = 9,
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
        
