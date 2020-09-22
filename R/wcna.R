#' ---
#' title: "Weighted correlation network analysis"
#' author: "Sam Gardiner"
#' date: "20 September  2020"
#' output: github_document
#' ---

#- setup, echo = FALSE
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warn = FALSE, fig.width = 10)

#- init
library(here)
library(tidyverse)
library(WGCNA)
library(polycor)
library(ggraph)
library(tidygraph)

theme_set(theme_bw())
theme_rotate_x <- theme(axis.text.x = element_text(angle = -90,
                                                   hjust = 0,
                                                   vjust = 0.5))


#' Load the data and select question variables. We exclude demographics from
#' this analysis.
wvs_q <- readRDS(here("data", "nzl_coded.RDS")) %>%
  select(matches(paste0("Q", 1:259, "$")))

#' Compute heterogeneous correlation matrix. This is very computationally
#' expensive, so cache the result as RDS.

if (file.exists(here("data", "cor.RDS"))){
  wvs_q_cor <- readRDS(here("data", "cor.RDS"))
} else {
  wvs_q_cor <- hetcor(wvs_q, use = "pairwise")
  saveRDS(wvs_q_cor, here("data", "cor.RDS"))
}

#' Find an appropriate soft threshold to consider variables joined by an edge:

wvs_abs <- abs(wvs_q_cor$correlations)
sft <- pickSoftThreshold.fromSimilarity(wvs_abs, moreNetworkConcepts = TRUE)

sft$fitIndices

# TODO: plot fit indices


wvs_wcn <- adjacency.fromSimilarity(wvs_q_cor$correlations,
                                    power = 6) %>%
  `class<-`("matrix")

wvs_net <- as_tbl_graph(wvs_wcn)


wvs_net %>%
  activate(edges) %>%
  top_n(weight, n = 1000) %>%
  ggraph() +
  geom_edge_link(aes(alpha = weight)) +
  geom_node_label(aes(label = name))
