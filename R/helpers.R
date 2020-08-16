# Helper functions for data cleaning and analysis

#' Re-encode missing values as NA
#'
#' @param data A raw WVS data frame.
#' @param codes The set of missingness codes. Defaults to -1:-5.
#'
#' @return The dataframe with all instances of the specified codes re-encoded as
#'  NA.
code_missing <- function(data, codes = -1:-5){
  data %>%
    mutate(across(where(is.numeric),
                  ~if_else(.x %in% codes, NA_real_, .x)))
}