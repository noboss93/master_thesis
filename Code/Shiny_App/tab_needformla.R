fluidRow(
  box(width = 6,
    withMathJax(includeMarkdown("tab_needformla.Rmd"))
  ),
  tabBox(width = 12,

         tabPanel(title = "R Code",
                  print("lmer(leistung ~ 1 + (1 | klassen), data = data_model()")
         ),
         tabPanel(title = "R Output",
                     "summary(data_model())"
         )
  )
         
)
