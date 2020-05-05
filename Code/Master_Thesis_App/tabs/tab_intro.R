fluidRow(
  box(width = 12, title = "Hierarchische Daten", status = "primary", collapsible = TRUE, collapsed = FALSE,
      withMathJax(includeMarkdown("texts/text_hierarchische_daten.Rmd"))),
  box(width = 12, title = "Hierarchische Lineare Modelle", status = "primary", collapsible = TRUE, collapsed = TRUE,
      withMathJax(includeMarkdown("texts/text_hlm_intro.Rmd"))),
  box (width = 12, title = "Literatur", status = "primary", collapsible = TRUE, collapsed = TRUE,
       withMathJax(includeMarkdown("texts/literatur.Rmd"))),
)
