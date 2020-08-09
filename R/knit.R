# Knit to GitHub markdown for viewing.

library(here)
library(ezknitr)

ezknit(file = here("R/eda.Rmd"),
       out_dir = here("output"),
       keep_html = FALSE)
