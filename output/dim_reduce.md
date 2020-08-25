---
title: "Dimension reduction"
author: "Sam Gardiner"
date: "25 August 2020"
output: 
  github_document:
    toc: TRUE
---



```r
library(tidyverse, quietly = TRUE)
library(here, quietly = TRUE)
library(FactoMineR)
library(factoextra)
library(missMDA)
```

```r
wvs <- readRDS(here("data", "nzl_coded.RDS")) %>%
  select(starts_with("Q"),
         -Q_MODE)

# Factor analysis of mixed data (FAMD)

wvs_impute <- imputeFAMD(wvs)
wvs_famd <- FAMD(wvs, tab.disj = wvs_impute$tab.disj, graph = FALSE)

fviz_screeplot(wvs_famd)
```

![plot of chunk data-in](dim_reduce/data-in-1.png)

```r
fviz_famd_ind(wvs_famd)
```

![plot of chunk data-in](dim_reduce/data-in-2.png)

The plots suggest there probably isn't much point in performing FAMD over the
whole set of questions. What about subsets?
