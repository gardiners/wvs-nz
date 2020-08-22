# Knit to GitHub markdown for viewing.

library(here)
library(ezknitr)
library(tidyverse)

# Scripts to render
scripts <- c("eda.R", "benchmark.R") %>% here("R", .)

# Output directory
output <- here("output")

walk(scripts, function(script) {
  ezspin(file = script,
         out_dir = output,
         keep_html = FALSE)
})

