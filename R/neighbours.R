#' ---
#' title: "Q18:26 - I wouldn't want to live next door to..."
#' author: "Sam Gardiner"
#' date: "5 Oct 2020"
#' output: github_document
#' ---

#- setup, echo = FALSE
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warn = FALSE, fig.width = 10)

#- init
library(gamlss)
library(glmnet)
library(here)
library(tidyverse)
library(tidymodels)


theme_set(theme_bw())
theme_rotate_x <- theme(axis.text.x = element_text(angle = -90,
                                                   hjust = 0,
                                                   vjust = 0.5))

#' "Outgroups" dataset:

wvs <- readRDS(here("data", "nzl_coded.RDS")) 

out <- wvs %>%
  select(D_INTERVIEW, matches(paste0("Q", 1:290, "$"))) %>%
  rowwise() %>%
  mutate(Mentions = sum(c(Q18, Q19, Q20, Q21, Q22, Q23, Q24, Q25, Q26) == 1,
                        na.rm = TRUE)) %>%
  ungroup() 

out_long <- out %>%
  select(D_INTERVIEW, matches(paste0("Q", 18:26, "$"))) %>%
  rename("Drug addicts" = Q18,
         "Different race" = Q19,
         "AIDS" = Q20,
         "Immmigrants" = Q21,
         "Homosexuals" = Q22,
         "Different religion" = Q23,
         "Heavy drinkers" = Q24,
         "Unmarried couple" = Q25,
         "Different language" = Q26) %>%
  pivot_longer(-D_INTERVIEW) %>%
  group_by(name, value) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

out_long

#' Distributions:

out_long %>%
  filter(value == "Mentioned") %>%
  ggplot(aes(freq, reorder(name, freq))) +
  geom_bar(stat = "identity") +
  labs(title = "\"I wouldn't want to live next door to...\"",
       x = "Frequency",
       y = "Group")

ggplot(out, aes(Mentions)) +
  geom_histogram(binwidth = 1, center = 0) +
  scale_x_continuous(breaks = 0:9)


mean(out$Mentions)
var(out$Mentions)

histDist(out$Mentions, family = PO)

#' Is there a parsimonious model which predicts how many outgroups a respondent will _not_ want as neighbours?
#' 
#' Lasso regression:

# Limit to 1st degree neighbours from our exploratory graph.
out_neighbourhood <- readRDS(here("data", "out_neighbourhood.RDS"))
out_vars <- out_neighbourhood %>%
  activate(nodes) %>%
  pull(name)

out_modelvars <- out %>%
  select(out_vars,
         Mentions,
         -matches(paste0("Q", c(18:26, 260:290),  "$")))

out_recipe <- 
  recipe(formula = Mentions ~ ., data = out_modelvars) %>% 
  step_novel(all_nominal(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_naomit(all_predictors()) %>%
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors(), -all_nominal()) 

out_spec <- 
  linear_reg(mixture = 1) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet", family = "poisson") 

out_workflow <- 
  workflow() %>% 
  add_recipe(out_recipe) %>% 
  add_model(out_spec) 

out_fit <- out_workflow %>%
  fit(data = out_modelvars)

pull_workflow_fit(out_fit)
