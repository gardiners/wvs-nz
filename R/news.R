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
library(ggfortify)
library(broom)

theme_set(theme_minimal())
theme_rotate_x <- theme(axis.text.x = element_text(angle = -90,
                                                   hjust = 0,
                                                   vjust = 0.5))

wvs <- readRDS(here("data", "nzl_coded.RDS")) 

#' # Where do people get their news?

#' News sources are Q201:208. Outgroups and people with "undesirable" traits are
#' listed at Q18:26. `D_INTERVIEW` is a subject UID.

news <- wvs %>%
  select(D_INTERVIEW, starts_with(paste0("Q", 201:208)))

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


#' We could take the reciprocal of the period as a frequency. This new variable
#' is days per year per news source.

news_freq <- news %>%
  mutate(across(where(is.ordered),
                 ~case_when(.x == "Daily" ~ 365,
                            .x == "Weekly" ~ 52,
                            .x == "Monthly" ~ 12,
                            .x == "Less than monthly" ~ 0,
                            .x == "Never" ~ 0))) %>%
  rename(Newspaper = Q201,
         TV = Q202,
         Radio = Q203,
         "Mobile phone" = Q204,
         Email = Q205,
         Internet = Q206,
         "Social media" = Q207,
         "Conversation" = Q208)

news_freq_matrix <- news_freq %>%
  select(-D_INTERVIEW) %>%
  na.omit() %>%
  as.matrix()

#' ## PCA worth trying?

news_pc <- prcomp(news_freq_matrix, center = TRUE, scale = TRUE)
news_eigen <- tidy(news_pc, "eigenvalues")
news_loadings <- tidy(news_pc, "loadings")
news_scores <- tidy(news_pc, "scores")

#' Variance explained:
summary(news_pc)

#' Visually:
ggplot(news_eigen, aes(PC, std.dev^2)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 1, colour = "firebrick", linetype = "dashed") +
  labs(x = "Component", y = "Eigenvalue")

#' Score projection:
autoplot(news_pc,
         loadings = TRUE,
         loadings.label = TRUE,
         alpha = 1/2)

#' Loadings:
news_loadings %>%
  pivot_wider(names_from = PC,
              names_prefix = "PC ",
              values_from = value)

news_loadings %>%
  filter(PC <= 3) %>%
  ggplot(aes(value, column), hjust = 0.5) +
  geom_point() +
  geom_segment(aes(x = 0, xend = value, y = column, yend = column)) +
  geom_vline(xintercept = 0, alpha = 1/3) +
  facet_grid(cols = vars(PC))

#' # What do people think of outgroups?

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

#' # Do news sources modulate perception of outgroups?

#' Combine our data:
out_logical <- out %>%
  mutate(across(-D_INTERVIEW, ~case_when(.x == "Mentioned" ~ TRUE,
                                         .x == 'Not mentioned' ~ FALSE)))
news_augmented <- augment(news_pc, newdata = news_freq)

combined_augmented <- full_join(out_logical, news_augmented) %>%
  na.omit()

#' Model for "Heavy drinkers"

drinkers1 <- glm(`Heavy drinkers` ~ Newspaper + TV + Radio + `Mobile phone` +
                   Email + Internet + `Social media`,
                 data = combined_augmented,
                 family = binomial())
summary(drinkers1)

drinkers_pc <- glm(`Heavy drinkers` ~ .fittedPC1 + .fittedPC2,
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

AIDS_PC <- glm(AIDS ~ .fittedPC1 + .fittedPC2,
               data = combined_augmented,
               family = binomial())
summary(AIDS_PC)

exp(coefficients(AIDS_PC))




