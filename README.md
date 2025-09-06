
# barn

<!-- badges: start -->
<!-- badges: end -->

The goal of barn is to provide pipeable functions to simplify
    preprocessing of tabular data prior to machine learning modeling.
    Users can combine multiple datasets, define feature engineering steps
    (such as creating new predictors from nominal or numeric columns), and
    then split the data back into preprocessed datasets ready to be used
    in machine learning workflows.

## Installation

You can install the development version of barn like so:

``` r
options(
  repos = c(getOption("repos"), "jrosell" = "https://jrosell.r-universe.dev")
)
pak::pak("barn")
```

## Example

This is a basic example shows you how to do feature engineering:

``` r
library(barn)
full <- tibble::tibble(a = c(1, 2), b = c(2, 1), c = c("M", "M"), y = c(1))
holdout <- tibble::tibble(a = c(1, 1), b = c(2, 2), c = c("R", "M"))
harvested <- barn(full, holdout) |>
    plant_new_numeric_factors() |>
    plant_label_encode() |>
    plant_new_nominal_pairs() |>
    plant_count_encode() |>
    harvest() 
full_prep <- harvested[["full"]]
full_prep
# # A tibble: 2 × 14
#   a_num a_cat b_num b_cat c_cat a_b_cat a_c_cat b_c_cat a_cat_count b_cat_count
#   <dbl> <int> <dbl> <int> <int>   <int>   <int>   <int>       <int>       <int>
# 1     1     1     2     2     1       1       0       3           3           3
# 2     2     2     1     1     1       3       3       0           1           1
# # ℹ 4 more variables: c_cat_count <int>, a_b_cat_count <int>, a_c_cat_count <int>,
# #   b_c_cat_count <int>
holdout_prep <- harvested[["holdout"]]
holdout_prep
# # A tibble: 2 × 14
#   a_num a_cat b_num b_cat c_cat a_b_cat a_c_cat b_c_cat a_cat_count b_cat_count
#   <dbl> <int> <dbl> <int> <int>   <int>   <int>   <int>       <int>       <int>
# 1     1     1     2     2     2       1       1       4           3           3
# 2     1     1     2     2     1       1       0       3           3           3
# # ℹ 4 more variables: c_cat_count <int>, a_b_cat_count <int>, a_c_cat_count <int>,
# #   b_c_cat_count <int>
```

