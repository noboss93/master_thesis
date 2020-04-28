fluidRow(
  box(width = 3, title = "Bedingung Wählen", status = "primary",
      selectInput(inputId = "design_cond_2", label = "Wähle das Level der Intervention:",
                  choices = c("Intervention auf Level-1" = "lvl1", "Intervention auf Level-2" = "lvl2"))
  ),
  box(width = 9, title = "Studienergebniss", status = "primary",
      plotOutput(outputId = "power")
      ),
  box(width = 12, title = "Infobox", height = "300px", status = "primary", collapsible = TRUE, collapsed = TRUE)
)
