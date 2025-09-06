test_that("barn can be created from one data.frames", {
  a <- data.frame(x = c(1))
  expect_s3_class(barn(a), "barn")
  expect_s3_class(barn(a)$combined, "data.frame")
  expect_length(barn(a)$combined, 1)
})


test_that("barn combine the common columns of two data.frames", {
  a <- data.frame(x = c(1))
  b <- data.frame(x = c(1), y = c(2))
  expect_s3_class(barn(a, b), "barn")
  expect_s3_class(barn(a, b)$combined, "data.frame")
  expect_length(barn(a, b)$combined, 1)
})


test_that("harvest splits the combined dataset into the preprocesed datasets", {
  a <- data.frame(x = c(1))
  b <- data.frame(x = c(1), y = c(2))
  harvested <- barn(a, b) |> harvest()
  expect_type(harvested, "list")
  expect_length(harvested, 2)
  expect_named(harvested, c("a", "b"))
  expect_s3_class(harvested[["a"]], "data.frame")
  expect_length(harvested[["a"]], 1)
})


test_that("plant_new_numeric_factors create factors from numerics", {
  df <- data.frame(x = c(1))
  result <- barn(df) |>
    plant_new_numeric_factors() |>
    harvest() |>
    purrr::pluck(1) |>
    tibble::as_tibble()
  expect_named(result, c("x_num", "x_cat"))
  expect_type(result$x_num, "double")
  expect_s3_class(result$x_cat, "factor")
})


test_that("plant_label_encode encode nominal columns as integers ", {
  df <- data.frame(x = c(1))
  result <- barn(df) |>
    plant_new_numeric_factors() |>
    plant_label_encode() |>
    harvest() |>
    purrr::pluck(1) |>
    tibble::as_tibble()
  expect_named(result, c("x_num", "x_cat"))
  expect_type(result$x_num, "double")
  expect_equal(class(result$x_cat), "integer")
})

test_that("plant_new_nominal_pairs create pairwise interactions from nominal columns", {
  df <- data.frame(a = c(1), b = c(2))
  result <- barn(df) |>
    plant_new_numeric_factors() |>
    plant_label_encode() |>
    plant_new_nominal_pairs() |>
    harvest() |>
    purrr::pluck(1) |>
    tibble::as_tibble()
  expect_named(result, c("a_num", "a_cat", "b_num", "b_cat", "a_b_cat"))
  expect_equal(class(result$a_b_cat), "integer")
})

test_that("plant_count_encode encode nominal columns with its frequencies", {
  df <- data.frame(a = c(1), b = c(2))
  result <- barn(df) |>
    plant_new_numeric_factors() |>
    plant_label_encode() |>
    plant_new_nominal_pairs() |>
    plant_count_encode() |>
    harvest() |>
    purrr::pluck(1) |>
    tibble::as_tibble()
  expect_named(
    result,
    c(
      "a_num",
      "a_cat",
      "b_num",
      "b_cat",
      "a_b_cat",
      "a_cat_count",
      "b_cat_count",
      "a_b_cat_count"
    )
  )
  expect_equal(class(result$a_b_cat_count), "integer")
})
