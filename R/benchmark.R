#' ---
#' title: "Census benchmarking"
#' subtitle: "Is the sample representative?"
#' author: "Sam Gardiner"
#' date: "16 August 2020"
#' output: 
#'   github_document:
#'     toc: TRUE
#' ---

#- setup, echo = FALSE
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warn = FALSE,
                      fig.width = 8)

#- init
library(tidyverse, quietly = TUE)
library(here, quietly = TRUE)

# Helper functions for data cleaning
source(here("R", "helpers.R"))

# Universal ggplot theming 
theme_set(theme_bw())

#' # Datasets
#' ## Census
#' The dataset in use here is the [New Zealand 2018 Census totals by topic – national
#' highlights (updated)](https://www.stats.govt.nz/information-releases/2018-census-totals-by-topic-national-highlights-updated).
census_data_dir <- "data/2018-Census-totals-by-topic-national-highlights"

#' Load all of the census highlights into a list of dataframes and perform data
#' cleaning common to all.
census_datasets <- list.files(here(census_data_dir)) %>%
  set_names(str_replace_all,
            c("-2018-census-csv.csv$" = "",
              "-" = "_")) %>%
  map(function(dataset){
    read_csv(here(census_data_dir, dataset),
             col_types = cols(Code = col_character())) %>%
      rename_with(~"n",
                  .cols = matches("Census_usually_resident_population_count")) %>%
      filter(str_starts(Code, "Total", negate = TRUE))
  })

#' Age:
census_age <- census_datasets$age_single_years %>%
  transmute(age = as.integer(Code),
         n)

#' Sex:
census_sex <- census_datasets$sex
  
#' ## WVS data

#' @TODO: Repeat with cleaned WVS dataset when available.

nzl_coded <- readRDS(here("data", "nzl_coded.RDS"))

#' Age:

wvs_age <- nzl_coded %>%
  select(age = Q262) %>%
  filter(age > 0 & age <= 120)

#' Sex:

wvs_sex <- nzl_coded %>%
  select(sex = Q260)

#' # Checks for representativeness

#' ## Age
# ------------------------------------------------------------------------------
#' The WVS sampling frame was the NZ electoral roll. Therefore, people under 18
#' have been systematically excluded from the survey. We need to do the same to the
#' census data for comparability.

#+ age-comparison, fig.cap = "KDE for age distributions for whole population and WVS respondents."
# Combine the WVS and census datasets for plotting.
# Expand the census summary into individual rows (all five million):
census_age_expanded <- census_age %>%
  filter(age >= 18) %>%
  with(rep(age, n))

# Denote the source of each observation in the combined dataframe.
combo_age <- rbind(
  cbind(wvs_age, data_source = "WVS"),
  data.frame(age = census_age_expanded, data_source = "Census")
)

ggplot(combo_age, aes(age, fill = data_source)) +
  geom_density(alpha = 1/2)

#' These appear to be different distributions. Test more formally with
#' Kolmogorov-Smirnov test:

ks.test(wvs_age$age, census_age_expanded)

#' The distribution of ages of survey respondents does not match the population,
#' even considering the removal of under-18s. The age distribution of
#' respondents is somewhat shifted to the right compared to the population -
#' they are older than expected if they were a representative sample.
#'
#' ## Sex
# ------------------------------------------------------------------------------
#' Have males and females responded to the survey in the same proportions
#' as they occur in the population?

#- sex-comparison, fig.cap = "Sex proportions in census and survey datasets."
census_sex_tab <- census_sex %>%
  select(sex = Sex, n) %>%
  mutate(freq = n / sum(n))

wvs_sex_tab <- wvs_sex %>%
  na.omit() %>%
  group_by(sex) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

combo_sex <- rbind(cbind(census_sex_tab, data_source = "Census"),
                   cbind(wvs_sex_tab, data_source = "WVS"))

ggplot(combo_sex, aes(freq, data_source, fill = sex)) +
  geom_bar(stat = "identity", width = 1, colour = "white") +
  geom_text(aes(label = round(freq, 3)),
            position = position_stack(vjust = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.position = "top", legend.title = element_blank())

#' These appear to be different distributions. Test more formally with a
#' one-sample test of proportions, treating the census data as population
#' proportion:

prop.test(x = t(wvs_sex_tab$n),
          p = census_sex_tab$freq[1])

#' The sample sex proportions are not representative of the population. Females
#' are over-represented in the survey responses.

#' ## Immigration and birthplace
# ------------------------------------------------------------------------------

#' Did immigrants to New Zealand respond to the survey at the same rate they
#' exist in the population?



