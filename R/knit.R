# Knit to GitHub markdown for viewing.

library(here)
library(ezknitr)

ezspin(file = here("R/eda.R"),
       out_dir = here("output"),
       keep_html = FALSE)
