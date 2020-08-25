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


#' ## Random forest

#' Are there non-linearities in the predictors? We might find different
#' predictor importances with a non-linear regression method.
#' 
#' We're going to want all of our cores for this: start a cluster.
cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)

rf_control <- trainControl(verboseIter = TRUE,
                           allowParallel = TRUE)

#' Fit a random forest.
#- rf-fit, cache = TRUE
rf_model <- train(H_URBRURAL ~ .,
                  data = wvs,
                  method = "ranger",
                  na.action = "na.omit",
                  trControl = rf_control,
                  tuneLength = 10,
                  importance = "impurity",
                  num.trees = 10000)

stopCluster(cl)

rf_model$finalModel

rf_importance <- varImp(rf_model)

#' Which variables contribute the most to the random forest?
plot(rf_importance, top = 20)

#' These estimates seem to be extremely unstable: re-training the random forest
#' yields a new set of important predictors each time. However, with repeated
#' runs, a few predictors repeatedly float to the top: Q162, Q126 ("Hard to
#' say"), Q213, ("Would never do"), Q217 ("Would never do"), Q218 ("Would never
#' do"), Q286 ("Spent some savings and borrowed money"), Q173 ("An atheist".)
#' 
#' These are:
#' 
#' * Q126: Immigration increases the risks of terrorism ("Hard to say").
#' * Q162: It is not important for me to know about science in my daily life.
#' * Q173: Independently of whether you attend religious services or not, would
#' you say you are…? ("An atheist")
#' * Q213: Political actions: donating to a group or campaign ("Would never do")
#' * Q217: Political actions using the internet: Searching information about
#' politics and political events ("Would never do")
#' * Q218: Political actions using the internet: Signing an electronic petition
#' ("Would never do")
#' * Q286:  During the past year, did your family ("Spent some savings and borrowed money")
