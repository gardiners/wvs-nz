# Knit to GitHub markdown for viewing.

library(here)
library(rmarkdown)

render(here("R/eda.Rmd"),
       output_file = here("output/eda.md"),
       output_format = "github_document")
