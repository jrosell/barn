#' Combine datasets to preprocess
#'
#' @description
#' Combine multiple data frames based on their common columns.
#' That's the first step for preprocessing with the `barn` package.
#' When printing it shows the characteristics of the combined datasets.
#'
#' @param ... One or more data frames to be combined in a  `barn` object.
#' @param nominal_sufix An optional string for dealing with nominal variables. Defaults to "_cat".
#' @param numeric_sufix An optional string for dealing with numeric variables. Defaults to "_num".
#'
#' @returns A barn object containing the combined data frame, row counts,
#'
#' @examples
#' full <- data.frame(id = 1:3, p1 = c("A", "B", "C"), p2 = 10:12, y = 1:3)
#' holdout <- data.frame(id = 4:5, p1 = c("D", "E"), p2 = 1:2)
#' original <- data.frame(id = 1:2, p1 = c("F", "G"), p2 = 3:4, y = 4:5)
#' print(barn(full, holdout, original))
#'
#' @export
#' @rdname barn
barn <- function(
  ...,
  nominal_sufix = "_cat",
  numeric_sufix = "_num"
) {
  quos <- rlang::enquos(...)
  datasets <- lapply(quos, rlang::eval_tidy)
  .names <- names(quos)
  if (is.null(.names) || any(.names == "")) {
    .names <- sapply(quos, rlang::as_label)
  }

  common_cols <- Reduce(intersect, lapply(datasets, colnames))

  attr(datasets, "row_count") <- sapply(datasets, nrow)
  attr(datasets, ".names") <- .names

  datasets_common <- datasets |>
    lapply(function(df) df[, common_cols, drop = FALSE])
  row_counts <- sapply(datasets_common, nrow)
  combined <- bind_rows(datasets_common)
  structure(
    list(
      combined = combined,
      row_count = attr(datasets, "row_count"),
      dataset = .names,
      nominal_sufix = nominal_sufix,
      numeric_sufix = numeric_sufix
    ),
    class = "barn"
  )
}


#' @param x An object of class "barn".
#' @param form_width An integer specifying the minimum column width (in characters). Default is 30.
#' @param ... Extra arguments.
#' @export
#' @rdname barn
print.barn <- function(
  x,
  form_width = 30,
  ...
) {
  stopifnot(inherits(x, "barn"))
  combined <- x$combined
  nominal_sufix <- x$nominal_sufix
  numeric_sufix <- x$numeric_sufix
  light_grey <- crayon::make_style("#AAAAAA")
  cli::cli_h1(format("Barn", width = form_width))
  cli::cli_h3(format("Settings", width = form_width))
  cat(light_grey(paste0(
    "nominal_sufix: ",
    x$nominal_sufix,
    "\n"
  )))
  cat(light_grey(paste0(
    "numeric_sufix: ",
    x$numeric_sufix,
    "\n"
  )))
  cli::cli_h3(format("Datasets", width = form_width))
  cat(light_grey(paste0(
    "# The combined dataset: ",
    nrow(combined),
    " x ",
    ncol(combined),
    "\n"
  )))
  df <- tibble::as_tibble(data.frame(
    dataset = x$dataset,
    row_count = x$row_count
  ))
  print(df, width = form_width)

  cli::cli_h3(format("Exploratory Data Analysis", width = form_width))
  x1 <- combined |>
    summarize(across(
      (where(is.factor) | contains(nominal_sufix)),
      list(
        unique___factor = \(x) n_distinct(x),
        missing___factor = \(x) sum(is.na(x))
      ),
      .names = "{.col}___{.fn}"
    ))
  x2 <- combined |>
    summarize(across(
      -(where(is.factor) | contains(nominal_sufix)),
      list(
        unique___numeric = \(x) n_distinct(x),
        missing___numeric = \(x) sum(is.na(x))
      ),
      .names = "{.col}___{.fn}"
    ))
  inputs <- bind_cols(x1, x2) |>
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "value"
    ) |>
    separate(
      .data[["variable"]],
      into = c("variable", "stat", "type"),
      sep = "___"
    ) |>
    pivot_wider(names_from = "stat", values_from = "value")
  tibble::as_tibble(inputs) |> print(n = Inf)
  invisible(x)
}


#' Split the combined dataset
#'
#' @description
#' Splits the `combined` data frame from a `barn` object back into a named list
#' containing the preprocessed predictors.
#'
#' @param barn_obj An object of class `"barn"`, created by [barn()].
#'
#' @returns
#' A named list of data frames, one for each dataset originally passed to
#' [barn()].
#'
#' @examples
#' full <- data.frame(id = 1:3, p1 = c("A", "B", "C"), p2 = 10:12, y = 1:3)
#' holdout <- data.frame(id = 4:5, p1 = c("D", "E"), p2 = 1:2)
#' original <- data.frame(id = 1:2, p1 = c("F", "G"), p2 = 3:4, y = 4:5)
#' harvested <- barn(full, holdout) |> harvest()
#' names(harvested)
#' harvested[["full"]]
#'
#' @export
#' @rdname harvest
harvest <- function(barn_obj) {
  stopifnot(inherits(barn_obj, "barn"))

  combined <- barn_obj$combined
  counts <- barn_obj$row_count
  dataset <- barn_obj$dataset

  splits <- vector("list", length(counts))
  start <- 1

  for (i in seq_along(counts)) {
    end <- start + counts[i] - 1
    splits[[i]] <- combined[start:end, , drop = FALSE]
    start <- end + 1
  }

  names(splits) <- dataset

  splits
}

#' New factors from numerical columns
#'
#' @description
#' A function to transform numeric and character columns in a `barn` object into new factor columns.
#' It appends "_num" for numeric columns, "_cat" for character columns, and renames both to factors.
#' Original columns are deleted from the combined data frame within the barn object.
#' @param barn_obj A `barn` object, created by [barn()].
#' @param numeric_suffx The suffix for new numeric factor columns. Default is "_num".
#' @param nominal_suffix The suffix for new nominal factor columns. Default is "_cat".
#'
#' @returns
#' The modified `barn_obj` with the transformed combined data frame.
#' @export
plant_new_numeric_factors <- function(
  barn_obj,
  numeric_suffx = "_num",
  nominal_suffix = "_cat"
) {
  stopifnot(inherits(barn_obj, "barn"))
  combined <- barn_obj$combined
  num_cols <- names(combined)[sapply(combined, \(x) {
    is.numeric(x) & !is.factor(x)
  })]
  character_cols <- names(combined)[sapply(combined, is.character)]
  factor_cols <- names(combined)[sapply(combined, is.factor)]
  cat_cols <- names(combined)[sapply(combined, \(x) {
    is.character(x) | is.factor(x)
  })]

  for (col in num_cols) {
    new_numeric <- paste0(col, numeric_suffx)
    combined[[new_numeric]] <- combined[[col]]
    new_factor <- paste0(col, nominal_suffix)
    combined[[new_factor]] <- as.factor(combined[[col]])
    combined[[col]] <- NULL
  }
  for (col in character_cols) {
    new_factor <- paste0(col, nominal_suffix)
    combined[[new_factor]] <- as.factor(combined[[col]])
    combined[[col]] <- NULL
  }
  for (col in factor_cols) {
    new_factor <- paste0(col, nominal_suffix)
    combined[[new_factor]] <- as.factor(combined[[col]])
    combined[[col]] <- NULL
  }
  barn_obj$combined <- combined
  barn_obj
}


#' Encode labels in a barn object
#'
#' @description
#' Transform nominal columns from factors to integers.
#'
#' @param barn_obj An instance of class "barn".
#'
#' @returns The modified `barn_obj` with the transformed combined data frame.
#'
#' @export
plant_label_encode <- function(barn_obj) {
  stopifnot(inherits(barn_obj, "barn"))
  combined <- barn_obj$combined
  character_cols <- names(combined)[sapply(combined, is.character)]
  for (col in character_cols) {
    combined[[col]] <- as.factor(combined[[col]])
  }
  factor_cols <- names(combined)[sapply(combined, is.factor)]
  for (col in factor_cols) {
    combined[[col]] <- as.integer(combined[[col]])
  }
  barn_obj$combined <- combined
  barn_obj
}

#' Create new nominal pairs
#'
#' @description
#' A function to create new features based on combinations of categorical columns in a barn object.
#'
#' @param barn_obj An object inheriting from the "barn" class.
#' @param nominal_suffix A character string that specifies the suffix for the newly created columns. Optional, default is "_cat".
#'
#' @returns
#' The modified `barn_obj` with the transformed combined data frame.
#' @export
plant_new_nominal_pairs <- function(
  barn_obj,
  nominal_suffix = "_cat"
) {
  stopifnot(inherits(barn_obj, "barn"))
  combined <- barn_obj$combined

  cat_cols <- colnames(combined)[str_detect(colnames(combined), nominal_suffix)]
  if (length(cat_cols) < 2) {
    warning("Not enough categorical columns for the requested degree")
    return(barn_obj)
  }
  pairs <- combn(cat_cols, 2)
  for (i in seq_len(ncol(pairs))) {
    cols <- pairs[, i]
    new_col_name <- paste(cols, collapse = "_") |>
      str_replace_all(nominal_suffix, "")
    new_col_name <- paste0(new_col_name, nominal_suffix)
    c1 <- as.integer(combined[[cols[1]]])
    c2 <- as.integer(combined[[cols[2]]])
    n2 <- if (is.factor(combined[[cols[2]]])) {
      nlevels(combined[[cols[2]]])
    } else {
      max(c2, na.rm = TRUE) + 1
    }
    combined[[new_col_name]] <- as.double((c1 - 1) * n2 + (c2 - 1))
  }
  barn_obj$combined <- combined
  barn_obj
}

#' Encode categorical columns with counts
#'
#' @description
#' Frequency encoding of nominal variables.
#'
#' @param barn_obj A Barn object, created by [barn()].
#' @param nominal_suffix The suffix applied to column names. Defaults to "_cat".
#'
#' @returns
#' The modified `barn_obj` with the transformed combined data frame.
#'
#' @export
plant_count_encode <- function(
  barn_obj,
  nominal_suffix = "_cat"
) {
  stopifnot(inherits(barn_obj, "barn"))
  combined <- barn_obj$combined
  cat_cols <- colnames(combined)[str_detect(colnames(combined), nominal_suffix)]
  for (cat_col in cat_cols) {
    counts <- table(combined[[cat_col]], useNA = "ifany")
    new_col <- paste0(cat_col, "_count")
    combined[[new_col]] <- counts[as.character(combined[[cat_col]])] |>
      as.integer()
  }
  barn_obj$combined <- combined
  barn_obj
}


#' Extract decimals in numeric features
#'
#' @description
#' Creates new integer columns by extracting specific digits from numeric columns.
#' This function emulates a feature engineering technique often used in machine learning.
#'
#' @param barn_obj A `barn` object, created by [barn()].
#' @param numeric_sufix The suffix used to identify numeric columns to process. Defaults to "_num".
#' @param from The starting digit position to extract (e.g., 1 for the first decimal place). Defaults to 1.
#' @param to The ending digit position to extract (e.g., 9 for the ninth decimal place). Defaults to 9.
#'
#' @returns
#' The modified `barn_obj` with the transformed combined data frame.
#'
#' @examples
#' df <- tibble::tibble(x_num = c(1.234, 5.678, NA))
#' b <- barn(df) |> plant_decimals_extract(from = 1, to = 3)
#' harvest(b)[[1]]
#' @export
plant_decimals_extract <- function(
  barn_obj,
  numeric_sufix = "_num",
  from = 1,
  to = 10
) {
  stopifnot(inherits(barn_obj, "barn"))
  if (!is.numeric(from) || !is.numeric(to)) {
    stop("'from' and 'to' must be numeric.")
  }
  if (from <= 0 || to <= 0) {
    stop("'from' and 'to' must be positive integers.")
  }
  if (from > to) {
    stop("'from' must be less than or equal to 'to'.")
  }
  combined <- barn_obj$combined
  numeric_cols <- names(combined)[str_detect(names(combined), numeric_sufix)]
  if (length(numeric_cols) == 0) {
    warning("No numeric columns found with the specified suffix. Skipping.")
    return(barn_obj)
  }
  for (c in numeric_cols) {
    for (k in from:to) {
      original_name <- str_replace(c, numeric_sufix, "")
      new_col_name <- paste0(original_name, "_d", k, numeric_sufix)
      combined[[new_col_name]] <- as.integer((combined[[c]] * 10^k) %% 10)
      combined[[new_col_name]][is.na(combined[[new_col_name]])] <- -1
    }
  }
  barn_obj$combined <- combined
  barn_obj
}


#' Round numeric features to specified precisions
#'
#' @description
#' Creates new numeric columns by rounding existing numeric columns
#' at specified decimal precisions. This is useful for feature engineering,
#' where different rounding granularities may capture meaningful patterns.
#'
#' @param barn_obj A `barn` object, created by [barn()].
#' @param numeric_sufix The suffix used to identify numeric columns to process.
#'   Defaults to "_num".
#' @param precisions A numeric vector specifying the number of decimal
#'   places to round to (e.g., `c(9, 8)`).
#'
#' @returns
#' The modified `barn_obj` with the transformed combined data frame.
#'
#' @examples
#' df <- tibble::tibble(x_num = c(1.23456789))
#' b <- barn(df) |> plant_decimals_round(precisions = c(2, 3))
#' harvest(b)[[1]]
#' harvest(b)[[1]]$x_r2_num
#' harvest(b)[[1]]$x_r3_num
#' @export
plant_decimals_round <- function(
  barn_obj,
  numeric_sufix = "_num",
  precisions = c(9, 8)
) {
  stopifnot(inherits(barn_obj, "barn"))
  if (!is.numeric(precisions)) {
    stop("'precisions' must be a numeric vector.")
  }

  combined <- barn_obj$combined
  numeric_cols <- names(combined)[str_detect(names(combined), numeric_sufix)]
  if (length(numeric_cols) == 0) {
    warning("No numeric columns found with the specified suffix. Skipping.")
    return(barn_obj)
  }

  for (c in numeric_cols) {
    original_name <- str_replace(c, numeric_sufix, "")
    for (r in precisions) {
      new_col_name <- paste0(original_name, "_r", r, numeric_sufix)
      combined[[new_col_name]] <- round(combined[[c]], r)
    }
  }

  barn_obj$combined <- combined
  barn_obj
}

#' Summarize Barn Object
#'
#' @description
#' A function to group and summarize to add aggregations to a \code{barn_obj} using specified variables and expressions.
#' WARNING: Risk of overfitting and bad generalization if not done
#' when resampling.
#'
#' @param barn_obj An object of class 'barn'.
#' @param .by Varible(s) to group by. Currently unused; must be empty.
#' @param ... Expressions to compute summarizing values.
#'
#' @returns A modified \code{barn_obj} with summarized data in the combined slot.
#' @export
plant_summarize <- function(barn_obj, .by = NULL, ...) {
  stopifnot(inherits(barn_obj, "barn"))
  combined <- barn_obj$combined

  # capture summarise expressions
  dots <- rlang::enquos(...)

  # turn grouping vars into characters
  by_syms <- rlang::ensyms(.by)
  by_vars <- purrr::map_chr(by_syms, rlang::as_string)

  # summarise
  agg <- combined |>
    dplyr::group_by(!!!by_syms) |>
    dplyr::summarise(!!!dots, .groups = "drop")

  # join back on character var names
  combined <- dplyr::left_join(combined, agg, by = by_vars)

  barn_obj$combined <- combined
  barn_obj
}
