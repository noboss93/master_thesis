fluidRow(
  box(width = 12, title = "Forschungsfrage und Studiendesign", status = "primary",collapsible = TRUE, collapsed = FALSE,
      withMathJax(includeMarkdown("texts/text_researchquestion.Rmd"))
  ),
  box(width = 12, title = "Studie 1: Genauigkeit von Schätzparametern", status = "primary",collapsible = TRUE, collapsed = TRUE,
      withMathJax(includeMarkdown("texts/text_study_1.Rmd"))
  ),
  box(width = 12, title = "Studie 2: Zuverlässigkeit von LM und HLM", status = "primary",collapsible = TRUE, collapsed = TRUE,
      withMathJax(includeMarkdown("texts/text_study_2.Rmd"))
  ),
  box(width = 12, title = "Literatur", status = "primary", collapsible = TRUE, collapsed = TRUE,
       withMathJax(includeMarkdown("texts/literatur.Rmd")))
)
