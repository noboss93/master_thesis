fluidRow(
  box(width = 3, title = "Bedingung Wählen", status = "primary",
      
      selectInput(inputId = "outcomes_2", label = "Wähle ein Kennwert:",
                  choices = c("Statistische Power" = "power", 
                              "Fehler Typ 1 Rate" = "error")),
      
      selectInput(inputId = "design_cond_2", label = "Wähle das Level der Intervention:",
                  choices = c("Intervention auf Level-1" = "lvl1", "Intervention auf Level-2" = "lvl2"))
  ),
  box(width = 9, title = "Studienergebniss", status = "primary",
      plotOutput(outputId = "power")
      ),
  box(width = 12, title = "Infobox", status = "primary", 
      uiOutput(outputId = "study2results")
      )
)
