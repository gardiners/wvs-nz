#' ---
#' title: "Differentially expressed values and beliefs"
#' author: "Sam Gardiner"
#' date: "25 August 2020"
#' output: github_document
#' ---

#- setup, echo = FALSE
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warn = FALSE, fig.width = 10)

#- init
library(tidyverse)
library(here)
library(broom)
library(caret)
library(doParallel)

wvs <- readRDS(here("data", "nzl_coded.RDS")) %>%
  select(H_URBRURAL, matches("Q[0-9]+")) %>%
  mutate(H_URBRURAL = factor(H_URBRURAL,
                             levels = 1:2,
                             labels = c("Urban", "Rural")))

#' ## Can we predict urban-rural status?
#' ### Univariate logistic regression model

#' Fit a univariate logistic regression for every predictor.
#- models, cache = TRUE
predictors <- names(wvs)[-1]
formulae <- map(predictors, reformulate, response = "H_URBRURAL") %>%
  set_names(predictors)
rur_models <- map(formulae, glm, family = binomial(), data = wvs) 

#' Compute a likelihood ratio test statistic, p value and adjusted p value for
#' each model.
rur_model_summaries <- map_dfr(rur_models, glance, .id = "predictor") %>%
  mutate(lr_t = null.deviance - deviance,
         df = df.null - df.residual,
         p = map2_dbl(lr_t, df, pchisq, lower.tail= FALSE),
         p_adj = p.adjust(p, "BH"))

#' Is anything significant at p < 0.1 after adjustment for multiple comparisons?
rur_model_summaries %>%
  filter(p_adj < 0.1) %>%
  ggplot(aes(p_adj, predictor)) +
  geom_point()

#' The most predictive variables are:
#' 
#' * Q281: To which of the following occupational groups do you belong?
#' * Q282: To which of the following occupational groups does your spouse belong?
#' * Q273: Marital/relationship status.
#' * Q140: Which of the following things have you done for reasons of security:
#' Preferred not to go out at night.

#' Examine these models:
#' top-predictive-models, results="asis"
c("Q140", "Q273", "Q281", "Q282") %>%
  set_names() %>%
  map(~tidy(rur_models[[.x]], exponentiate = TRUE)) %>%
  map(knitr::kable)


