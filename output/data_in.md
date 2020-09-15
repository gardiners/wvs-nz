

```r
# Data ingest, coding and cleansing
```


```r
library(tidyverse)
library(haven)
library(here)
library(labelled)
```

Helper function to generate regexes from question numbers, such that they match
the variety of question names in the dataset.


```r
match_questions <- function(q_number, prefix = "Q", suffix = "[a-zA-Z]?$") {
  paste0(prefix, q_number, suffix)
}
```

# Data ingest

Read SPSS data file, since all of the factor levels are already somewhat coded in
the SPSS metadata.


```r
nzl_raw <- read_spss(here("data", "WVS_Wave_7_New_Zealand_Spss_v1.4.sav"),
                     user_na = TRUE)
```

# Variable coding

Decide which of our questions are ordinal, on a 10-point scale (and
therefore may end up being treated as continuous), continuous or nominal.


```r
q_numbers <- lst(
  ordinal = c(1:6, 27:47, 51:55, 58:89, 113:118, 121, 131:138, 141:143, 146:148,
              169:172, 196:199, 201:208, 221, 222, 224:239, 253, 255:259, 275:278, 287),
  scale_10_point = c(48, 49, 50, 90, 106:110, 112, 120, 158:164, 176:195, 240:252,
                     288),
  continuous = c(261, 262, 270),
  # These are sort-of ordinal but will need manual coding. They are currently
  # treated as categorical:
  badly_ordered = c(119, 221, 222, 254),
  # Everything else is nominal (including binary)
  nominal = (1:290)[!(1:290 %in% c(ordinal, scale_10_point, continuous))]
)
```

Some of the variable names have suffixes in the NZ dataset. So, we build
regexes for each variable name so that we can use them with tidyselect.


```r
q_names <- map(q_numbers, match_questions)
```

Code the factor levels according to their labels in SPSS. For our ordinal
variables set ordered = TRUE, otherwise leave them unordered. The
10-point-scale variables do not need any recoding


```r
nzl_coded <- nzl_raw %>%
  # Remove existing labels
  zap_labels() %>%
  # Convert all types of missing values to NA
  mutate(across(starts_with("Q"), ~ifelse(.x < 0, NA, .x))) %>%
  # Restore labels from SPSS data
  copy_labels_from(nzl_raw) %>%
  drop_unused_value_labels() %>%
  # Covert labels to factors ready for analysis
  mutate(
    across(matches(q_names$ordinal), as_factor, ordered = TRUE),
         across(matches(q_names$nominal), as_factor)) %>%
  # Remove label metadata so that result is a plain tibble/data frame.
  zap_labels()
```

# Other data cleaning


```r
nzl_clean <- nzl_coded %>%
  # Fix "number of children" variable, Q274
  mutate(Q274 = if_else(Q274 == "No children",
                        0L,
                        as.integer(Q274)))
```

# Serialise for other scripts


```r
saveRDS(nzl_coded, here("data", "nzl_coded.RDS"))
```

