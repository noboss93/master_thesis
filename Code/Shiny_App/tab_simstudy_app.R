fluidRow(
  box(width = 12, title = "Forschungsfrage", status = "primary", collapsible = TRUE,
      withMathJax(includeMarkdown("researchquestion.Rmd"))
  ),
  box(width = 12, title = "Studie 1: Wirksamkeit von Standartfehlern", status = "primary", collapsible = TRUE, collapsed = TRUE,
      withMathJax(includeMarkdown("text_study_1.Rmd"))
  ),
  box(width = 12, title = "Studie 2: Statistische Power von HLM", status = "primary", collapsible = TRUE, collapsed = TRUE,
      withMathJax(includeMarkdown("text_study_2.Rmd"))
  )
)
