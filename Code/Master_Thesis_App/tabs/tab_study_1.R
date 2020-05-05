fluidRow(
  box(width = 3, title = "Bedingung Wählen", status = "primary",
      
      selectInput(inputId = "outcomes_1", 
                  label = "Wähle ein Kennwert:",
                  choices = c("Relative Abweichung" = "rel_bias", 
                              "SE Genauigkeit" = "se_eff")),
      
      selectInput(inputId = "design_cond_1", 
                  label = "Wähle das Level der Intervention:",
                  choices = c("Intervention auf Level-1" = "lvl1", 
                              "Intervention auf Level-2" = "lvl2")),
      
      selectInput(inputId = "coef_cond_1", 
                  label = "Wähle den Koeffizienten:",
                  choices = c("Achsenabschnitt" = "intercept", 
                              "Steigung (Intervention)" = "treatment")),
  ),
  box(width = 9, title = "Studienergebniss", height = "500px", status = "primary",
      plotOutput(outputId = "se_eff")),
  
  
  
  
  
  
  box(width = 12, title = "Interpretation der Ergebnisse", status = "primary",
     uiOutput(outputId = "mrkdwn")
  )
)

