fluidRow(
  box(
  selectInput(inputId = "method",
            label = "Wähle eine Methode aus:",
            choices = c("Lineares Regressionsmodell" = "lm", 
                        "Random Intercept Modell" = "ri", 
                        "Random Intercept und Slope Modell" = "rs"),
            multiple = FALSE)
),
tabBox(width = 12,
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