---
title: "Census benchmarking"
subtitle: "Is the sample representative?"
author: "Sam Gardiner"
date: "16 August 2020"
output: 
  github_document:
    toc: TRUE
---



```r
library(tidyverse, quietly = TRUE)
library(here, quietly = TRUE)

# Helper functions for data cleaning
source(here("R", "helpers.R"))

# Universal ggplot theming 
theme_set(theme_minimal())
```

# Utility functions



Add a frequency column, using a count column.


```r
add_freq <- function(data, count_col = n) {
  mutate(data, freq = {{ count_col }} / sum({{ count_col }}))
}
```

Combine two datasets, denoting their sources in a new column "data_source".


```r
combine_sets <- function(x, y, x_name = "Census", y_name = "WVS") {
  if(any(names(x) != names(y))) error("Column names must match.")
  rbind(
    cbind(x, data_source = x_name),
    cbind(y, data_source = y_name)
  )
}
```

Build a stacked barplot that compares two datasets on a single factor.

@param data A data frame.
@param factor_col The factor on which to stack the bars. Will be assigned to
  the fill aesthetic.
@param source_indicator A factor indicating which dataset the observation
  comes from.


```r
comparison_barplot <- function(data, factor_col, source_indicator = data_source) {
  ggplot(data, aes(freq, {{source_indicator}}, fill = {{factor_col}})) +
    geom_bar(stat = "identity", width = 1, colour = "white") +
    geom_text(aes(label = round(freq, 3)),
              position = position_stack(vjust = 0.5)) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(legend.position = "top", legend.title = element_blank())
}
```

# Datasets
## Census



The dataset in use here is the [New Zealand 2018 Census totals by topic –
national highlights
(updated)](https://www.stats.govt.nz/information-releases/2018-census-totals-by-topic-national-highlights-updated).


```r
census_data_dir <- "data/2018-Census-totals-by-topic-national-highlights"
```

Load all of the census highlights into a list of dataframes and perform data
cleaning common to all.


```r
census <- list.files(here(census_data_dir)) %>%
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
```

Age:


```r
census_age <- census$age_single_years %>%
  transmute(age = as.integer(Code),
            n)
```

Sex:


```r
census_sex <- census$sex
```

Education:


```r
census_education <- census$post_school_qualification_level_of_attainment
```

Place of birth:


```r
census_birth <- census$birthplace
```

## WVS data


```r
nzl_coded <- readRDS(here("data", "nzl_coded.RDS"))
```

Age:


```r
wvs_age <- nzl_coded %>%
  select(age = Q262) %>%
  filter(age > 0 & age <= 120)
```

Sex:


```r
wvs_sex <- nzl_coded %>%
  select(sex = Q260)
```

Education:


```r
wvs_education <- nzl_coded %>%
  select(education = Q275)
```

Place of birth:


```r
wvs_birth <- nzl_coded %>%
  select(country = Q266)
```

# Checks for representativeness
## Age



The WVS sampling frame was the NZ electoral roll. Therefore, people under 18
have been systematically excluded from the survey. We need to do the same to the
census data for comparability.


```r
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
```

![KDE for age distributions for whole population and WVS respondents.](benchmark/age-comparison-1.png)

These appear to be different distributions. Test more formally with
Kolmogorov-Smirnov test:


```r
ks.test(wvs_age$age, census_age_expanded)
```

```
## Warning in ks.test(wvs_age$age, census_age_expanded): p-value will be
## approximate in the presence of ties
```

```
## 
## 	Two-sample Kolmogorov-Smirnov test
## 
## data:  wvs_age$age and census_age_expanded
## D = 0.26675, p-value < 2.2e-16
## alternative hypothesis: two-sided
```

The distribution of ages of survey respondents does not match the population,
even considering the removal of under-18s. The age distribution of
respondents is somewhat shifted to the right compared to the population -
they are older than expected if they were a representative sample.

## Sex



Have males and females responded to the survey in the same proportions
as they occur in the population?


```r
census_sex_tab <- census_sex %>%
  select(sex = Sex, n) %>%
  add_freq()

wvs_sex_tab <- wvs_sex %>%
  na.omit() %>%
  count(sex) %>%
  add_freq()

# Combine and plot
combo_sex <- combine_sets(census_sex_tab, wvs_sex_tab)
comparison_barplot(combo_sex, sex)
```

![Sex proportions in census and survey datasets.](benchmark/sex-comparison-1.png)

These appear to be different distributions. Test more formally with a
one-sample test of proportions, treating the census data as population
proportion:


```r
prop.test(x = t(wvs_sex_tab$n),
          p = census_sex_tab$freq[1])
```

```
## 
## 	1-sample proportions test with continuity correction
## 
## data:  t(wvs_sex_tab$n), null probability census_sex_tab$freq[1]
## X-squared = 18.866, df = 1, p-value = 1.402e-05
## alternative hypothesis: true p is not equal to 0.4935487
## 95 percent confidence interval:
##  0.3952487 0.4563752
## sample estimates:
##         p 
## 0.4255319
```

The sample sex proportions are not representative of the population. Females
are over-represented in the survey responses.

## Education



Does education level differ between the sample and the census population? It
is difficult to directly compare, as the datasets code different education
levels. So, we code to secondary vs. post-secondary.



```r
# Code both census and wvs datasets to binary for post-secondary education.
census_educ_tab <-  census_education %>%
  transmute(post_school = case_when(Code == "00" ~ FALSE,
                                    as.integer(Code) %in% 2:10 ~ TRUE),
            n) %>%
  count(post_school) %>%
  na.omit() %>%
  add_freq()

wvs_educ_tab <- wvs_education %>%
  mutate(education = as.integer(education),
         post_school = case_when(education <= 4 ~ FALSE,
                                 education > 4 ~ TRUE)) %>%
  count(post_school) %>%
  na.omit() %>%
  add_freq()

# Combine and plot:
combo_educ <- combine_sets(census_educ_tab, wvs_educ_tab)
comparison_barplot(combo_educ, post_school) +
  scale_fill_discrete(labels = c("Secondary", "Post-secondary"))
```

![Education proportions in census and survey datasets.](benchmark/education-comparison-1.png)

Once again, these look to be markedly different distributions. We can test:


```r
prop.test(x = t(wvs_educ_tab$n),
          p = census_educ_tab$freq[1])
```

```
## 
## 	1-sample proportions test with continuity correction
## 
## data:  t(wvs_educ_tab$n), null probability census_educ_tab$freq[1]
## X-squared = 104.16, df = 1, p-value < 2.2e-16
## alternative hypothesis: true p is not equal to 0.4832927
## 95 percent confidence interval:
##  0.2951893 0.3533226
## sample estimates:
##         p 
## 0.3235867
```

The survey sample is not representative. The survey respondents are more
likely to have completed post-secondary education compared to the general
population.

## Immigration and birthplace



Did immigrants to New Zealand respond to the survey at the same rate they
exist in the population? Given that the survey sampling frame is drawn from
the New Zealand electoral roll, which comprises only citizens, it seems
unlikely that immigrants are well-represented in the survey respondents.

The census captures the names of all countries of birth; however the survey
only records New Zealand, United Kingdom and Other. We therefore recode the
census data to match this scheme.


```r
wvs_birth_tab <- wvs_birth %>%
  count(country) %>%
  na.omit() %>%
  add_freq() %>%
  arrange(freq)

# Combine various census countries-of-origin into United Kingdom level:
census_birth_tab <- census_birth %>%
  mutate(country = case_when(as.integer(Code) %in% c(0, 9999) ~ NA_character_,
                             Birthplace == "New Zealand" ~ "New Zealand",
                             Birthplace %in% c("England",
                                               "Scotland",
                                               "Northern Ireland",
                                               "Wales",
                                               "Channel Islands",
                                               "Isle of Man",
                                               "Gibraltar",
                                               "United Kingdom (not further defined)")
                             ~ "United Kingdom",
                             TRUE ~ "Other")) %>% 
  count(country) %>%
  na.omit() %>%
  add_freq() %>%
  arrange(freq)

# Combine and plot:
combo_birth <- combine_sets(census_birth_tab, wvs_birth_tab)
comparison_barplot(combo_birth, factor_col = country)
```

![plot of chunk unnamed-chunk-24](benchmark/unnamed-chunk-24-1.png)

These again appear to be different distributions. We can check formally:


```r
birth_stat <- chisq.test(x = wvs_birth_tab$n, p = census_birth_tab$freq)
birth_stat$expected
```

```
## [1]  58.99819 223.47254 748.52927
```

```r
birth_stat$observed
```

```
## [1] 100 134 797
```

```r
birth_stat
```

```
## 
## 	Chi-squared test for given probabilities
## 
## data:  wvs_birth_tab$n
## X-squared = 67.456, df = 2, p-value = 2.25e-15
```

Once again, we find that the survey sample is not representative of the
population. Native-born New Zealanders and those born in the UK are slightly
over-represented in the sample, while those with other origins are
under-represented.
