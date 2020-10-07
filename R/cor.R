#' ---
#' title: "Correlations"
#' author: "Sam Gardiner"
#' date: "5 October 2020"
#' output: github_document
#' ---

#- setup, echo = FALSE
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warn = FALSE, fig.width = 10)


knitr::opts_chunk$set(echo = TRUE, message = FALSE, warn = FALSE, fig.width = 10)

#- init
library(here)
library(tidyverse)
library(WGCNA)
library(polycor)

windowsFonts("Arial Narrow" = windowsFont("Arial"))
theme_set(theme_bw())
theme_rotate_x <- theme(axis.text.x = element_text(angle = -90,
                                                   hjust = 0,
                                                   vjust = 0.5))


#' Load the data and select question variables.
news_scores <- readRDS(here("data", "news_scores.RDS"))

wvs_q <- readRDS(here("data", "nzl_coded.RDS")) %>%
  select(H_URBRURAL, D_INTERVIEW,
         matches(paste0("Q", c(1:260, 262, 269, 270, 274, 287, 288), "$"))) %>%
  mutate(H_URBRURAL = factor(H_URBRURAL,
                             levels = 1:2,
                             labels = c("Urban", "Rural"))) %>%
  rowwise() %>%
  mutate(Mentions = sum(c(Q18, Q19, Q20, Q21, Q22, Q23, Q24, Q25, Q26) == 1,
                        na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(news_scores, by = "D_INTERVIEW") %>%
  select(-D_INTERVIEW)

#' Compute heterogeneous correlation matrix. This is very computationally
#' expensive, so cache the result as RDS.

if (file.exists(here("data", "cor_ext.RDS"))){
  wvs_q_cor <- readRDS(here("data", "cor_ext.RDS"))
} else {
  wvs_q_cor <- hetcor(wvs_q, use = "pairwise")
  saveRDS(wvs_q_cor, here("data", "cor_ext.RDS"))
}