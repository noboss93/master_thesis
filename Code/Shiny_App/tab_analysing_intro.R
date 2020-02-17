fluidRow(
  
  box(width = 12, title = "Wähle eine Methode aus:", status = "primary",
  selectInput(inputId = "method",
              label = NULL,
            choices = c("Lineares Regressionsmodell" = "lm", 
                        "Random Intercept Modell" = "ri", 
                        "Random Intercept und Slope Modell" = "rs"),
            multiple = FALSE),
  checkboxInput(inputId = "groupcolor",
                label = "Gruppenfarbe anzeigen")
  ),
  box(width = 3, height = "500px", title = "Datensatz generieren", status = "primary",
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
  tabBox(width = 9, height = "500px",
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
