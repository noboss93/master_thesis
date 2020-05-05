fluidRow(
  box(width = 12, title = "Simuliertes Beispiel", status = "primary",
      includeMarkdown("texts/text_sim_intro.Rmd")),
  column(width = 3,
         fluidRow(
           box(id = "gen_dat_box", width = 12, title = "Datensatz simulieren", status = "primary",
                sliderInput(inputId = "int_sd",
                            label = "Standardabweichung des Achsenabschnittes",
                            min = 0,
                            max = 30,
                            value = 10),
                sliderInput(inputId = "slope_sd",
                            label = "Standardabweichung der Steigung",
                            min = 0, 
                            max = 5,
                            value = 0,
                            step = 0.1),
                sliderInput(inputId = "corr",
                            label = "Korrelation zwischen Achsenabschnitt und Steigung",
                            min = -1,
                            max = 1,
                            value = 0,
                            step = 0.1),
                actionButton(inputId = "gen_data",
                             label = "Datensatz simulieren")
               )
            )
           ),
  column(width = 9,
         fluidRow(
           box(title = "Analysemethode auswühlen:", width = 12, status = "primary",
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
