fluidRow(
  box(title = "Informationen zum Datensatz", 
      "Hier werden alle Informationen zum Datensatz stehen. z.B. Wie viele Datenpunkte in wie vielen Gruppen. 
      Wie man erkennen kann, welches Level-1 Variablen und Level-2 Variablen sind. etc."
      ),
  
  tabBox(width = 12,
         tabPanel(
           "Übersicht der Daten",
           dataTableOutput(outputId = "table_full")
         ),
         tabPanel(
           "Struktur der Daten",
           verbatimTextOutput(outputId = "str_full", 
                              placeholder = FALSE)
         ),
         tabPanel(
           "Summary der Daten",
           verbatimTextOutput(outputId = "data_summary_full",
                              placeholder = FALSE)
         )
  )
)
