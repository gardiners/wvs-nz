# Knit to GitHub markdown for viewing.

library(here)
library(ezknitr)
library(tidyverse)

# Scripts to render (don't render self!)
scripts <- list.files(here("R"),
                      pattern = r"(\.R$)",
                      full.names = TRUE) %>%
  discard(~str_ends(.x, "knit.R"))

# Output directory
output <- here("output")

walk(scripts, function(script) {
  ezspin(file = script,
         out_dir = output,
         keep_html = FALSE)
})

