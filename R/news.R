#' ---
#' title: "News sources and acceptance of outgroups"
#' author: "Sam Gardiner"
#' date: "25 August 2020"
#' output: github_document
#' ---

#- setup, echo = FALSE
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warn = FALSE, fig.width = 10)

#- init
library(here)
library(tidyverse)
library(naniar)
library(psych)
library(UpSetR)
library(ggfortify)

theme_set(theme_bw())
theme_rotate_x <- theme(axis.text.x = element_text(angle = -90,
                                                   hjust = 0,
                                                   vjust = 0.5))

wvs <- readRDS(here("data", "nzl_coded.RDS")) 

#' # Where do people get their news?

#' News sources are Q201:208. Outgroups and people with "undesirable" traits are
#' listed at Q18:26. `D_INTERVIEW` is a subject UID.

#' Extract the news variables and reorder so that frequency increases with
#' factor level.
news <- wvs %>%
  select(D_INTERVIEW, starts_with(paste0("Q", 201:208))) %>%
  mutate(across(where(is.ordered), fct_rev)) %>%
  rename(Newspaper = Q201,
         TV = Q202,
         Radio = Q203,
         "Mobile phone" = Q204,
         Email = Q205,
         Internet = Q206,
         "Social media" = Q207,
         Talking = Q208)

summary(news)

#' Fortunately the missingness is concentrated on a few individuals who didn't
#' answer this entire set of questions.

gg_miss_upset(news)


#' Distributions: it seems that most people interact with a news source daily.

news_long <- news %>%
  pivot_longer(-D_INTERVIEW)

ggplot(news_long, aes(value)) +
  geom_bar() +
  facet_wrap(~name) +
  theme_rotate_x

levels(news_long$value)

#' ## Correlation with other variables

#' The exploratory [network analysis](wcna.md) find the following variables are
#' reasonably associated with the news-consumption questions:
#'
#' * Q45 -"Please tell me for each one, if it were to happen, whether you think it would be a good thing, a bad thing, or don't you mind: **Greater respect for authority**."
#' * Q67 - "I am going to name a number of organizations. For each one, could you tell me how much confidence you have in them: is it a great deal of confidence, quite a lot of confidence, not very much confidence or none at all: **Television**."
#' * "Please tell me for each of the following actions whether you think it can always be justified, never be justified, or something in between:" 
#'   * Q182 - **Homosexuality**
#'   * Q193 - **Casual Sex**
#' * "What about these forms of political action and social activism that people can take? Please, tell me for each of them if you have done any of these things, whether you might do it or would never under any circumstances do it:"
#'   * Q213 - **Donating to a group or campaign**
#'   * Q214 - **Contacting a government official**
#'   * Q215 - **Encouraging others to take action about political issues**
#'   * Q216 - **Encouraging others to vote**
#' * "Now I’d like you to look at this card. I’m going to read out some other forms of political action that people can take using Internet and social media tools like Facebook, Twitter etc., and I’d like you to tell me, for each one, whether you have done any of these things, whether you might do it or would never under any circumstances do it"
#'   * Q217 - **Searching information about politics and political events **
#'   * Q218 - **Signing an electronic petition**
#'   * Q219 - **Encouraging other people to take any form of political action**
#'   * Q220 - **Organizing political activities, events, protests**
#' * Demographic traits:
#'   * Q262 - **Age**
#'   * Q270 - **Household size**
#'   * Q288 - **Self-rated household income decile**

#' ## PCA on news source consumption

#' Our news-consumption variables are all ordinal. We therefore compute a
#' polychoric correlation matrix.

#- polychor-ml, cache = TRUE
news_int <- news %>%
  select(-D_INTERVIEW) %>%
  mutate(across(everything(), as.integer))

news_pc <- principal(news_int, cor = "poly", nfactors = 8, rotate = "none")

#' Extract eigenvalues, scores and loadings:
news_eigen <- tibble(
  Eigenvalue = news_pc$values,
  PC = 1:length(Eigenvalue),
  "Variance Explained" = Eigenvalue / sum(Eigenvalue),
  "Cumulative" = cumsum(Eigenvalue) / sum(Eigenvalue))

news_scores <- as.data.frame(news_pc$scores) %>%
  cbind(D_INTERVIEW = news$D_INTERVIEW, .)

str(news_scores)

news_loadings <- as_tibble(news_pc$weights, rownames = "Variable" )

#' Visually:
ggplot(news_eigen, aes(PC, Eigenvalue)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 1, colour = "firebrick", linetype = "dashed") +
  labs(x = "Component", y = "Eigenvalue")

#' Score projection:
ggplot(news_scores, aes(PC1, PC2)) +
  geom_point() 

#' Loadings:

news_loadings %>%
  pivot_longer(starts_with("PC")) %>%
  mutate(PC = as.integer(str_extract(name, "[0-9]+"))) %>%
  filter(PC <= 4) %>%
  ggplot(aes(value, fct_rev(Variable)), hjust = 0.5) +
  geom_point() +
  geom_segment(aes(x = 0, xend = value, y = Variable, yend = Variable)) +
  geom_vline(xintercept = 0, alpha = 1/3) +
  facet_grid(cols = vars(PC))

ggplot(news_loadings, aes(PC1, PC2)) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(type = "closed"), alpha = 1/2) +
  geom_text(aes(label = Variable, x = PC1 * 1.05, y = PC2 * 1.05))

#' # Which groups aren't wanted as neighbours?

out <- wvs %>%
  select(D_INTERVIEW, matches(paste0("^Q", 18:26, "$"))) %>%
  rename("Drug addicts" = Q18,
         "Different race" = Q19,
         "AIDS" = Q20,
         "Immmigrants" = Q21,
         "Homosexuals" = Q22,
         "Different religion" = Q23,
         "Heavy drinkers" = Q24,
         "Unmarried couple" = Q25,
         "Different language" = Q26)

out_long <- out %>%
  pivot_longer(-D_INTERVIEW) %>%
  group_by(name, value) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

out_long

out_long %>%
  filter(value == "Mentioned") %>%
  ggplot(aes(freq, reorder(name, freq))) +
  geom_bar(stat = "identity") +
  labs(title = "\"I wouldn't want to live next door to...\"",
       x = "Frequency",
       y = "Group")

#' Focusing just on those who don't want "different" people as neighbours:
different <- out %>%
  select(starts_with("Different")) %>%
  rename_with(~str_extract(.x, r"((\w+)$)")) %>%
  mutate(across(everything(), ~as.integer(.x == "Mentioned"))) %>%
  cbind(out$D_INTERVIEW, .)

upset(different,
      sets = c("race", "religion", "language"),
      nintersects = NA,
      empty.intersections = "on")

#' These groups of people are a very small proportion (<5%) of our dataset, so
#' we're unlikely to be able to model them well.



#' # Do news sources modulate perception of outgroups?

#' Combine our data:
out_logical <- out %>%
  mutate(across(-D_INTERVIEW, ~.x == "Mentioned"))

combined_augmented <- full_join(out_logical, news_scores) %>%
  full_join(news) %>%
  na.omit()

#' Model for "Heavy drinkers"

drinkers1 <- glm(`Heavy drinkers` ~ Newspaper + TV + Radio + `Mobile phone` +
                   Email + Internet + `Social media`,
                 data = combined_augmented,
                 family = binomial())
summary(drinkers1)

drinkers_pc <- glm(`Heavy drinkers` ~ PC1 + PC2,
                   data = combined_augmented,
                   family = binomial())
summary(drinkers_pc)
autoplot(drinkers_pc)

#' Model for "AIDS"

AIDS1 <- glm(AIDS ~ Newspaper + TV + Radio + `Mobile phone` +
               Email + Internet + `Social media`,
             data = combined_augmented,
             family = binomial())
summary(AIDS1)
exp(coefficients(AIDS1))

AIDS_PC <- glm(AIDS ~ PC1 + PC2,
               data = combined_augmented,
               family = binomial())
summary(AIDS_PC)

exp(coefficients(AIDS_PC))




