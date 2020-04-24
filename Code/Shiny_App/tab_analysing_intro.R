fluidRow(
  box(width = 12, title = "Simuliertes Beispiel", status = "primary",
      "Hier können selbst hierarchische Datensätze generiert werden. Über die Schieberegler auf der linken Seite kann festgelegt werden, wie stark die Klassenzugehörigkeit (Standardabweichung des Intercepts) oder die Anzahl gelösten Übungsaufgaben (Standardabweichung des Slopes) einen Einfluss auf dei Punktzahl hat. 
      Ebenfalls kann festgelegt werden, ob die Anzahl gelöster Übungsaufgabe mit der Klassenzugehörigkeit korrelliert. Nachdem der Datensatz generiert wurde, kann die Analysemethode festgelegt werden
      und über die verschiedenen Outputs mit anderen Methoden verglichen werden."
  ),
  column(width = 3,
         fluidRow(
           box(id = "gen_dat_box", width = 12, title = "Datensatz generieren", status = "primary",
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
               )
            )
           ),
  column(width = 9,
         fluidRow(
           box(title = "Wähle eine Methode aus:", width = 12, status = "primary",
                selectInput(inputId = "method",
                            label = NULL,
                            choices = c("Lineares Regressionsmodell" = "lm", 
                                        "Random Intercept Modell" = "ri", 
                                        "Random Intercept und Slope Modell" = "rs"),
                            multiple = FALSE),
                checkboxInput(inputId = "groupcolor",
                              label = "Gruppenfarbe anzeigen")
           ),
           tabBox(width = 12, height = "500px",
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
                    )
                  )
           )
         )
         
)
