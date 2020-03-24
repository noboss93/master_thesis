fluidRow(
  box(width = 3, title = "Bedingung Wählen", status = "primary",
      selectInput(inputId = "outcomes", label = "Wähle ein Kennwert:",
                  choices = c("Relative Abweichung" = "rel_bias", "SE Wirksamkeit" = "se_eff")),
      selectInput(inputId = "design_cond", label = "Wähle das Level der Intervention:",
                  choices = c("Intervention auf Level-1" = "lvl1", "Intervention auf Level-2" = "lvl2")),
      selectInput(inputId = "coef_cond", label = "Wähle den Koeffizienten:",
                  choices = c("Intercept" = "intercept", "Intervention" = "treatment")),
      selectInput(inputId = "icc_cond", label = "Wähle eine IKK:",
                  choices = c(0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.40, 0.50, "Kombiniert")),
      actionButton(inputId = "show_results", label = "Resultate Anzeigen")
  ),
  box(width = 9, title = "Studienergebniss", height = "500px", status = "primary"),
  box(width = 12, title = "Infobox", height = "300px", status = "primary", collapsible = TRUE, collapsed = TRUE)
)
