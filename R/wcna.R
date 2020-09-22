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
library(visNetwork)

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
sft$powerEstimate

# TODO: plot fit indices

#' Find the adjacency matrix.
wvs_adj <- adjacency.fromSimilarity(wvs_abs,
                                    power = 4) %>%
  `class<-`("matrix") %>%
  `diag<-`(0)

#' Find a topological overlap matrix.
wvs_tom <- TOMsimilarity(wvs_adj)
wvs_tom_dis <- 1 - wvs_tom

#' Cluster questions using topological overlap
wvs_tree <- fastcluster::hclust(as.dist(wvs_tom_dis), method = "average")

plot(wvs_tree)

#' Find question modules.
q_mods <- cutreeDynamic(wvs_tree, distM = wvs_tom_dis, deepSplit = 4)
table(q_mods)

# Build the network and assign survey questions to their modules.
wvs_net <- as_tbl_graph(wvs_adj, directed = FALSE) %>%
  activate(nodes) %>%
  mutate(module = q_mods)

# Look at degree distributions:
wvs_deg_dists <- map_dfr(set_names(seq(0.025, 0.975, by = 0.025)) ,
                         function(fraction) {
                           wvs_net %>%
                             activate(edges) %>%
                             top_frac(fraction, weight) %>%
                             activate(nodes) %>%
                             mutate(degree = centrality_degree()) %>%
                             as_tibble()
                         },
                         .id = "fraction")

ggplot(wvs_deg_dists, aes(degree, colour = fraction)) +
  geom_density() +
  scale_colour_viridis_d()

#'  Visualise top 0.05 strongest associations.
wvs_net_reduced <- wvs_net %>%
  activate(nodes) %>%
  filter(module > 0) %>%
  activate(edges) %>%
  top_frac(0.05, weight) %>%
  activate(nodes) %>%
  filter(centrality_degree() > 0)

ggraph(wvs_net_reduced, weight = weight) +
  geom_edge_link(aes(width = weight, alpha = weight)) +
  geom_node_label(aes(label = name,
                      fill = factor(module)),
                  size = 2.5) +
  theme(legend.position = "none")




#' Which nodes are the most important connectors?
wvs_net_reduced %>%
  activate(edge) %>%
  filter(edge_is_between())



#' #' Interactive:
#' wvs_net_reduced %>%
#'   as.igraph() %>%
#'   visIgraph(randomSeed = 20200922)




                     