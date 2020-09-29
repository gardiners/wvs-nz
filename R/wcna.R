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

windowsFonts("Arial Narrow" = windowsFont("Arial"))
theme_set(theme_bw())
theme_rotate_x <- theme(axis.text.x = element_text(angle = -90,
                                                   hjust = 0,
                                                   vjust = 0.5))


#' Load the data and select question variables.
wvs_q <- readRDS(here("data", "nzl_coded.RDS")) %>%
  select(H_URBRURAL,
         matches(paste0("Q", c(1:260, 262, 269, 270, 274, 287, 288), "$"))) %>%
  mutate(H_URBRURAL = factor(H_URBRURAL,
                             levels = 1:2,
                             labels = c("Urban", "Rural")))

#' Compute heterogeneous correlation matrix. This is very computationally
#' expensive, so cache the result as RDS.

if (file.exists(here("data", "cor.RDS"))){
  wvs_q_cor <- readRDS(here("data", "cor.RDS"))
} else {
  wvs_q_cor <- hetcor(wvs_q, use = "pairwise")
  saveRDS(wvs_q_cor, here("data", "cor.RDS"))
}

wvs_abs <- abs(wvs_q_cor$correlations)
wvs_sqr <- wvs_q_cor$correlations^2

#' Visualise correlation matrix

wvs_sqr %>%
  as_tibble(rownames = "from") %>%
  pivot_longer(-from, names_to = "to") %>%
  ggplot(aes(from, to, fill = value)) +
  geom_raster() +
  scale_fill_viridis() +
  coord_fixed()
  

#' # Soft thresholding

#' Find an appropriate soft threshold to consider variables joined by an edge:

sft <- pickSoftThreshold.fromSimilarity(wvs_abs, moreNetworkConcepts = TRUE)

sft$fitIndices
sft$powerEstimate

# TODO: plot fit indices

#' Find the adjacency matrix.
wvs_adj <- adjacency.fromSimilarity(wvs_abs,
                                    power = sft$powerEstimate) %>%
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
  mutate(module = factor(q_mods, levels = 1:5))

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
  #filter(module != 0) %>%
  activate(edges) %>%
  top_frac(0.05, weight) %>%
  activate(nodes) %>%
  filter(centrality_degree() > 0,
         !node_is_isolated()) %>%
  mutate(neighbours1 = local_members(order = 1),
         neighbours2 = local_members(order = 2))

ggraph(wvs_net_reduced, layout = "fr") +
  geom_edge_bend(aes(width = weight, alpha = weight)) +
  geom_node_label(aes(label = name,
                      fill = module),
                  alpha = 3/4,
                  size = 2) +
  theme(legend.position = "none")

#' ### Examine neighbourhoods of interest

#' Urban vs rural
#' --------------

urbrural_tree <- wvs_net_reduced %>%
  convert(to_local_neighborhood,
          node = which(.N()$name == "H_URBRURAL"),
          order = 2)

urbrural_tree %>%
  ggraph(layout = "kk") +
  geom_edge_link() +
  geom_node_label(aes(label = name))

#' The only real association seems to be with Q189.

ggplot(wvs_q, aes(H_URBRURAL, Q189)) +
  geom_violin()

wvs_q %>%
  group_by(H_URBRURAL) %>%
  summarise(Q189_mean = mean(Q189, na.rm = TRUE))

#' It seems there are a couple of outliers in the urban group who think it is
#' "Always justifiable for a man to beat his wife".

#' Sources of news
#' ---------------

news_q <- paste0("Q", 201:208)

news_neighbours <- wvs_net_reduced %>%
  activate(nodes) %>%
  filter(name %in% news_q) %>%
  as_tibble() %>%
  pull(neighbours1) %>%
  unlist() %>%
  append(201:208) %>%
  unique()

# news_neighbourhood <- wvs_net_reduced %>%
#    activate(nodes) %>%
#    filter(node_distance_from(nodes = which(.N()$name %in% news_q),
#                              mode = "all") <= 2)

news_neighbourhood <- wvs_net_reduced %>%
  activate(nodes) %>%
  filter(1:nrow(.N()) %in% news_neighbours) %>%
  mutate(highlight = name %in% news_q)

ggraph(news_neighbourhood) +
  geom_edge_bend(aes(alpha = weight, width = weight)) +
  geom_node_label(aes(label = name, fill = highlight))

news_neighbourhood %>%
  activate(nodes) %>%
  as_tibble

# news_tree <- map(news_q,
#                  function(question){
#                    subgraph <- wvs_net_reduced %>%
#                      convert(to_local_neighborhood,
#                              node = which(.N()$name == question),
#                              order = 2)
#                    subgraph
# })
  




#' # Hard thresholding experiment

hard <- pickHardThreshold.fromSimilarity(wvs_abs)
hard$cutEstimate

hard_adj <- signumAdjacencyFunction(wvs_abs, hard$cutEstimate)

hard_net <- as_tbl_graph(hard_adj, directed = FALSE) 
  
hard_net %>%
  activate(nodes) %>%
  filter(centrality_degree(mode = "all", loops = FALSE) >= 1,
         !node_is_isolated()) %>%
  ggraph(layout = "dh") +
  geom_edge_link() +
  geom_node_label(aes(label = name), size = 2.25) +
  theme_graph() +
  theme(legend.position = "bottom")

