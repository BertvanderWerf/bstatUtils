#' Convert Delimited Text into Indicator Columns
#'
#' Splits each entry in a character vector into binary (0/1) columns, useful for multiple-response questionnaire items.
#' Optionally supports "Other" and "Missing" columns.
#'
#' @param x Character vector to parse into multiple columns.
#' @param sep Character string used to split multiple responses (default: `";"`).
#' @param items Optional character vector specifying expected items.
#'   If provided, entries not matching these items are grouped into an "Other" column.
#' @param other_name Name for the "Other" column. Default: `"Other"`.
#' @param other_sep Separator used when collapsing unmatched values into the "Other" column. Default: `"|"`.
#' @param na_col Character string naming the missing-value column, or `NULL` to omit it.
#'   For example, set `na_col = "Missing"` to include one.
#' @param ignore_case Logical; if TRUE, ignores letter case in value matching.
#'
#' @returns
#' A data frame where each specified or detected item becomes a numeric (0/1) column.
#' Optionally includes character `"Other"` and numeric `"Missing"` columns if requested.
#'
#' @examples
#' text_to_columns(c("A;B", "B", "C", NA))
#' text_to_columns(c("apples; pears", "pears; peaches", ""), sep = ";")
#' text_to_columns(c("A;B", "B;C;Z", "B"), items = c("A", "B", "C"))
#'
#' @export
text_to_columns <- function(
    x,
    sep = ";",
    items = NULL,
    other_name = "Other",
    other_sep = "|",
    na_col = NULL,
    ignore_case = FALSE
) {

  bstatErr::check_string(sep)
  bstatErr::check_string(other_name)
  bstatErr::check_string(other_sep)
  bstatErr::check_string(na_col, allow_null=TRUE)
  bstatErr::check_logical(ignore_case)


  ##--- Helper to trim white space ---##
  trim_ws <- function(x) gsub("^\\s+|\\s+$", "", x)

  ##--- Identify missing entries ---##
  is_missing <- is.na(x)
  x_trimmed <- ifelse(is_missing, "", trim_ws(as.character(x)))

  ##--- Standardize case if required ---##
  if (ignore_case) x_trimmed <- tolower(x_trimmed)

  ##--- Split text by delimiter ---##
  split_list <- strsplit(x_trimmed, sep, fixed = TRUE)
  split_list <- lapply(split_list, trim_ws)
  split_list[is_missing] <- list(character(0))

  ##--- Identify unique items ---##
  all_unique <- sort(unique(unlist(split_list)))
  all_unique <- all_unique[all_unique != ""]

  ##--- Define item set to use ---##
  if (is.null(items)) {
    item_set <- all_unique
    use_other <- FALSE
  } else {
    item_set <- if (ignore_case) tolower(trim_ws(items)) else trim_ws(items)
    use_other <- TRUE
  }

  ##--- Prepare final list of columns ---##
  out <- vector(
    "list",
    length = length(item_set) + as.integer(use_other) + as.integer(!is.null(na_col))
  )

  names(out) <- c(item_set,
                  if (use_other) other_name,
                  if (!is.null(na_col)) as.character(na_col))

  ##--- Fill item indicator columns ---##
  for (j in seq_along(item_set)) {
    out[[j]] <- as.numeric(vapply(split_list,
                                  function(row) item_set[j] %in% row, integer(1)))
  }

  ##--- Handle "Other" entries ---##
  if (use_other) {
    out[[other_name]] <- vapply(split_list, function(row) {
      others <- setdiff(row, item_set)
      if (length(others)) paste(others, collapse = other_sep) else NA_character_
    }, character(1))
  }

  ##--- Handle NA indicator column ---##
  if (!is.null(na_col)) {
    out[[as.character(na_col)]] <- as.numeric(is_missing)
  }

  ##--- Combine all into data frame ---##
  df <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)

  ##--- Convert non-character columns to numeric ---##
  for (nm in names(df)) {
    if (nm != other_name) df[[nm]] <- as.numeric(df[[nm]])
  }

  ##--- Return final data frame ---##
  df
}
