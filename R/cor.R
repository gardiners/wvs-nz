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
library(polycor)
library(glasso)
library(tidygraph)
library(ggraph)

windowsFonts("Arial Narrow" = windowsFont("Arial"))
theme_set(theme_bw())
theme_rotate_x <- theme(axis.text.x = element_text(angle = -90,
                                                   hjust = 0,
                                                   vjust = 0.5))


#' Load the data and select question variables.
wvs_q <- readRDS(here("data", "nzl_coded.RDS")) %>%
  select(matches(paste0("Q", c(1:260, 262, 269, 270, 274, 287, 288), "$")))

#' Compute heterogeneous correlation matrix. This is very computationally
#' expensive, so cache the result as RDS.

if (file.exists(here("data", "cor_ext.RDS"))){
  wvs_q_cor <- readRDS(here("data", "cor_ext.RDS"))
} else {
  wvs_q_cor <- hetcor(wvs_q, use = "pairwise")
  saveRDS(wvs_q_cor, here("data", "cor_ext.RDS"))
}

# Graphical lasso to estimate a sparse (regularised) correlation matrix

wvs_q_cor_complete <- wvs_q_cor$correlations
wvs_q_cor_complete[is.na(wvs_q_cor$correlations)] <- 0

if (file.exists(here("data", "glasso.RDS"))){
  wvs_glasso <- readRDS(here("data", "glasso.RDS"))
} else {
  wvs_glasso <- glasso(wvs_q_cor_complete,
                       rho = 0.1,
                       nobs = 1057)
  saveRDS(wvs_glasso, here("data", "glasso.RDS"))
}


# Remove self-edges (matrix diagonals) and build network.

glasso_net <- wvs_glasso$w %>%
  `diag<-`(0) %>%
  as_tbl_graph(directed = FALSE)


ggraph(glasso_net, layout = "fr") +
  geom_edge_link()
