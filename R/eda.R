#' ---
#' title: "Exploratory data analysis"
#' author: "Sam Gardiner"
#' date: "1 August 2020"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warn = FALSE)

#' 
## ---------------------------------------------------------------------
library(tidyverse)
library(here)
library(naniar)
library(tidygraph)
library(ggraph)
library(lubridate)

#' 
#' ## File input
#' 
## ---------------------------------------------------------------------
nzl_raw <- read_csv(here("data/NZL.csv"))

#' 
#' ## Missingness?
#' 
## ---------------------------------------------------------------------
nzl_missing <- nzl_raw %>%
  select(starts_with("Q")) %>%
  mutate(across(where(is.numeric), ~if_else(.x %in% -1:-5, NA_real_, .x)))

# Case-wise completeness?
pct_complete_case(nzl_missing)
# Variable-wise completeness?
pct_complete_var(nzl_missing)
# Cell-wise completeness?
pct_complete(nzl_missing)

#' ### Demographics missingness?
#' 
#' **Notable**: patterns of missingness are structural. For example, people who
#' have missing data for spouse education also have missing data for spouse
#' income - because they don't have a spouse. NMAR. Imputation not sensible in
#' these cases.
#' 
## ----fig.height=8-----------------------------------------------------
nzl_missing %>%
  select(starts_with(paste0("Q", 260:287))) %>%
  gg_miss_upset(nsets = 10)

#' 
#' 
#' 
#' 
#' ## Rural vs city (small town vs bigger town)
#' 
#' Overwhelmingly city respondents:
#' 
## ---------------------------------------------------------------------
nzl_raw %>%
  select(H_SETTLEMENT, H_URBRURAL) %>%
  mutate(across(.fns = as.factor)) %>%
  summary()

#' 
#' However, a uniform-ish range of town sizes:
#' 
## ---------------------------------------------------------------------
nzl_raw %>%
  select(starts_with("G_")) %>%
  pivot_longer(everything()) %>%
  mutate(value = factor(value)) %>%
  ggplot(aes(value)) +
  geom_bar() +
  facet_wrap(~name)

#' 
#' ## News sources
#' 
## ---------------------------------------------------------------------
nzl_news <- nzl_raw %>%
  select(starts_with(paste0("Q", 201:208))) %>%
  pivot_longer(everything()) %>%
  mutate(value = factor(value,
                        levels = 1:5,
                        labels = c("Daily", "Weekly", "Monthly",
                                   "Less than monthly", "Never")))


ggplot(nzl_news, aes(value)) +
  geom_bar() +
  facet_wrap(~name) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

nzl_news %>%
  mutate(name = fct_collapse(name,
                             "Old media" = c("Q201P", "Q202P", "Q203P"),
                             "New media" = c("Q204P", "Q205P", "Q206P", "Q207P"),
                             "Word of mouth" = c("Q208P"))) %>%
  ggplot(aes(value)) +
  geom_bar() +
  facet_wrap(~name) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#' 
#' 
#' ## Correlations?
#' 
#' Scale everything and compare. Religion seems to come out as being the
#' strongest association with other values.
#' 
## ---------------------------------------------------------------------
# Scale and compute Pearson's correlation
nzl_corr <- nzl_missing %>%
  select(starts_with("Q")) %>%
  select(-Q_MODE)  %>%
  mutate(across(everything(),  scale)) %>%
  cor(use = "pairwise")

# Build a correlation network
nzl_corr_graph <- as_tbl_graph(nzl_corr, directed = FALSE)

# Filter out the low correlations and visualise
nzl_corr_trimmed <- nzl_corr_graph %>%
  activate(edges) %>%
  filter(from != to) %>%
  top_n(150, abs(weight)) %>%
  activate(nodes) %>%
  filter(centrality_degree() > 1)

  ggraph(nzl_corr_trimmed, layout = "circle")+
  geom_node_point() +
  geom_edge_diagonal(aes(colour = weight)) +
  geom_node_text(aes(label = name, x = x * 1.15, y = y * 1.15), size = 2.5) +
  scale_edge_colour_distiller(type = "div", palette = "RdBu", limits = c(-1, 1)) +
  theme_graph()

#' 
#' 
#' ## Interview date distribution
#' 
#' Binwidth is weeks.
#' 
## ---------------------------------------------------------------------
nzl_raw %>%
  select(J_INTDATE) %>%
  mutate(J_INTDATE = ymd(J_INTDATE)) %>%
  ggplot(aes(J_INTDATE)) +
  geom_histogram(binwidth = 7) +
  scale_x_date(breaks = "month", date_labels = "%B\n%Y") +
  labs(x = "Interview date")

#' 
