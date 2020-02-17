fluidRow(
  box(width = 12, title = "Simulationsstudie", status = "primary",
      selectInput(inputId = "sim_study",
                  label = "Design der Simulationsstudie",
                  choices = c("Treatment auf Level 1" = "lvl1", 
                              "Treatment auf Level 2" = "lvl2"),
                  multiple = FALSE),
      actionButton(inputId = "results",
                   label = "Ergebnisse Anzeigen")
  ),
  tabBox(width = 12, height = "500px",
         tabPanel(
           "Power-Vergleich",
           plotOutput(outputId = "power")
         ),
         tabPanel(
           "Effizienz des Schätzers für Parameter",
           plotOutput(outputId = "parameter_eff")
         ),
         tabPanel(
           "Effizienz des Schätzers für Standardfehler",
           plotOutput(outputId = "se_eff")
         )
  )
)
