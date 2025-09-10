#' Internal helper function for package development
#' @examples
#' if (FALSE) {
#'   devtools::load_all(); rebuild_package_and_check(build_site = TRUE)
#'   # rhub::rhub_setup() # first time
#'   rhub::rhub_check(platforms = "windows", r_versions = "4.3")
#'   rhub::rhub_check(platforms = "linux")
#'   usethis::use_version(which = "dev", push = FALSE)
#'   usethis::use_github_release()
#'   tools::showNonASCIIfile("R/barn.R")
#' }
#'
#' @noRd
rebuild_package_and_check <- function(build_site = FALSE) {
  usethis::use_description(list(
    "Title" = "Preprocessing and Feature Engineering Steps before Modeling",
    "Description" = "The package provides pipeable functions to simplify preprocessing 
    of tabular data prior to machine learning modeling. Users can combine multiple 
    datasets, define feature engineering steps (such as creating new predictors from 
    nominal or numeric columns), and then split the data back into preprocessed 
    datasets ready to be used in machine learning workflows.",
    "Version" = "0.0.0.9000",
    "Authors@R" = utils::person(
      "Jordi",
      "Rosell",
      email = "jroselln@gmail.com",
      role = c("aut", "cre"),
      comment = c(ORCID = "0000-0002-4349-1458")
    ),
    "URL" = "https://jrosell.github.io/barn",
    "BugReports" = "https://github.com/jrosell/barn/issues",
    Language = "en"
  ))
  # usethis::use_pkgdown_github_pages()
  usethis::use_package("R", type = "Depends", min_version = "4.3.0")
  usethis::use_mit_license()
  suggests_packages <- c(
    "pak",
    "devtools",
    "here",
    "pkgdown",
    "usethis",
    "rhub",
    "roxygen2",
    "purrr",
    "testthat"
  )
  suggests_packages |>
    purrr::walk(\(x) usethis::use_package(x, type = "Suggests"))

  imports_packages <- c(
    "rlang",
    "dplyr",
    "cli",
    "crayon",
    "tibble (>= 3.1.0)",
    "stringr",
    "ellmer",
    "glue"
  )
  imports_packages |>
    purrr::walk(\(x) usethis::use_package(x, type = "Imports"))

  usethis::use_package_doc(open = FALSE)
  usethis::use_import_from(
    "rlang",
    c(
      ".data",
      ".env",
      "enquos",
      "as_label",
      "eval_tidy"
    )
  )
  usethis::use_import_from(
    "dplyr",
    c(
      "bind_rows",
      "summarize",
      "bind_cols",
      "across",
      "where",
      "everything",
      "n_distinct",
      "contains"
    )
  )
  usethis::use_import_from(
    "tidyr",
    c(
      "pivot_longer",
      "pivot_wider",
      "separate"
    )
  )
  usethis::use_import_from(
    "stringr",
    c(
      "str_detect",
      "str_replace_all",
      "str_replace"
    )
  )
  usethis::use_import_from(
    "utils",
    c(
      "combn"
    )
  )
  usethis::use_import_from(
    "ellmer",
    c(
      "chat_ollama"
    )
  )
  usethis::use_import_from(
    "glue",
    c(
      "glue"
    )
  )
  devtools::load_all()
  devtools::document()
  devtools::check(document = FALSE) # rcmdcheck::rcmdcheck(repos = FALSE).
  if (build_site == TRUE) {
    pkgdown::build_site(preview = FALSE) # # usethis::use_pkgdown_github_pages()
    utils::browseURL(
      here::here("docs", "index.html"),
      browser = getOption("browser")
    )
  }
  devtools::load_all()
}

#' Internal helper function for package documentation
#' @noRd
generate_documentation <- \(x) {
  chat <- ellmer::chat_ollama(
    system_prompt = readLines(
      "https://raw.githubusercontent.com/simonpcouch/chores/2c8ba1d0736a33b9aba161f0ef42c6474406f376/inst/prompts/roxygen-prefix.md"
    ),
    model = "qwen2.5-coder:3b"
  )
  chat$chat(glue::glue("{x}")) |>
    str_replace_all("\n\n", "\n") |>
    str_replace_all("\r\n\r\n", "\r\n") |>
    cat()
}


#' Internal helper function for package tests
#' @noRd
generate_tests <- \(x) {
  chat <- ellmer::chat_ollama(
    system_prompt = readLines("inst/prompts/testthat-generate.md"),
    model = "qwen2.5-coder:3b"
  )
  chat$chat(glue::glue("{x}")) |>
    str_replace_all("\n\n", "\n") |>
    str_replace_all("\r\n\r\n", "\r\n") |>
    cat()
}
