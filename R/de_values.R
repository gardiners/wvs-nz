#' ---
#' title: "Differentially expressed values and beliefs"
#' author: "Sam Gardiner"
#' date: "25 August 2020"
#' output: github_document
#' ---

#- setup, echo = FALSE
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warn = FALSE, fig.width = 10)

#- init
library(mlogit)
library(here)
library(broom)
library(ordinal)
library(tidyverse)
library(ggExtra)

theme_set(theme_minimal())
theme_rotate_x <- theme(axis.text.x = element_text(angle = -90,
                                                   hjust = 0,
                                                   vjust = 0.5))

wvs <- readRDS(here("data", "nzl_coded.RDS")) %>%
  select(H_URBRURAL, matches("Q[0-9]+")) %>%
  mutate(H_URBRURAL = factor(H_URBRURAL,
                             levels = 1:2,
                             labels = c("Urban", "Rural")))

#' ## Are urban and rural respondents different?
#' 
#' On continuous responses, considered jointly.

wvs_numeric <- wvs %>%
  select(where(is.numeric), H_URBRURAL, -Q288R, -Q289CS, -Q261) %>%
  na.omit()

wvs_response <- wvs_numeric %>%
  select(-H_URBRURAL) %>%
  as.matrix()

#' ### PCA
wvs_pca <- prcomp(wvs_response, center = TRUE, scale = FALSE)
summary(wvs_pca)$importance[,1:5]

#' Scree plot:
wvs_pca_long <- t(summary(wvs_pca)$importance) %>%
  as_tibble(rownames = "Component") %>%
  mutate(Eigenvalue = `Standard deviation`^2)

ggplot(wvs_pca_long, aes(reorder(Component, -Eigenvalue), Eigenvalue)) +
  geom_point() +
  geom_hline(yintercept = mean(wvs_pca_long$Eigenvalue), linetype = "dashed") +
  theme_rotate_x +
  labs(x = "Component")

#' Reprojected observations:
#- reprojection
wvs_rotated <- data.frame(H_URBRURAL = wvs_numeric$H_URBRURAL,
                          wvs_pca$x)

p <- ggplot(wvs_rotated, aes(PC1, PC2, colour = H_URBRURAL, fill = H_URBRURAL)) +
  geom_point() +
  labs(x = "PC1 - 47.9%", y = "PC2 - 9.7%") +
  theme(legend.position = "bottom")
ggMarginal(p, groupColour = TRUE)

#' Loadings  
wvs_loadings <- wvs_pca$rotation %>%
  as_tibble(rownames = "Variable") %>%
  pivot_longer(-Variable)

#- loadings-plot, fig.height=12
wvs_loadings %>%
  filter(name %in% paste0("PC", 1:2)) %>%
  ggplot(aes(value, Variable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~name)

#' ### MANOVA
wvs_manova <- manova(wvs_response ~ wvs_numeric$H_URBRURAL)
summary(wvs_manova, test = "Hotelling")

#' Have we met the approximate assumptions for MANOVA?

# Multi-colinearity?
wvs_numeric_corr <- cor(wvs_response)^2 %>%
  as_tibble(rownames = "var1") %>%
  pivot_longer(-var1, "var2")

#- heatmap, fig.height = 12, fig.width = 12
ggplot(wvs_numeric_corr, aes(var1, var2, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c() +
  theme_rotate_x +
  labs(x = "", y = "") +
  coord_equal()

#' ## Can we predict urban-rural status?
#' ### Univariate logistic regression model

#' Fit a univariate logistic regression for every predictor.
#- models-rural-response, cache = TRUE
predictors <- names(wvs)[-1]
predictor_formulae <- map(predictors, reformulate, response = "H_URBRURAL") %>%
  set_names(predictors)
rur_models <- map(predictor_formulae, glm, family = binomial(), data = wvs) 

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
#'     * Farmer and farm owner are significant levels.
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

#' ## What does rural-urban status best predict?
#' 
#' Which variables are most affected by rural or urban status?

# Which modelling function do we need? Depends on the type of response variable.
find_model <- function(x) {
  UseMethod("find_model")
}

find_model.ordered <- function(x) {
  # Cumulative logit model for ordinal regression.
  safely(ordinal::clm)
}

find_model.factor <- function(x) {
  n <- nlevels(x)
  # Logistic regression for binary responses.
  if (n == 2) safely(partial(stats::glm, family = "binomial"))
  # Multinomial logit for other factors.
  else safely(mlogit::mlogit)
}

find_model.double <- function(x) {
  # Linear model for continuous responses
  safely(lm)
}

# Define and fit the models:
#- models-rural-predicts, cache = TRUE
response_models <- tibble(
  response = names(wvs)[-1],
  response_formula = map(response, ~reformulate("H_URBRURAL", .x)),
  response_type = map(response, ~class(wvs[[.x]])),
  model_fun = map(response, ~find_model(wvs[[.x]])),
  fitted_model = map2(model_fun, response_formula, 
                      ~exec(.x, .y, data = wvs))
)

response_summaries <- map(response_models$fitted_model, 
                          ~glance(.x$result))

