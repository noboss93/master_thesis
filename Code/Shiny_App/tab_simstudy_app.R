fluidRow(
  box(width = 12, title = "Forschungsfrage", status = "primary",
      includeMarkdown("researchquestion.Rmd")
  ),
  box(width = 12, title = "Studie 1: Wirksamkeit von Standartfehlern", status = "primary",
      includeMarkdown("text_study_1.Rmd")
  ),
  box(width = 12, title = "Studie 2: Statistische Power von HLM", status = "primary",
      includeMarkdown("text_study_2.Rmd")
  )
)
