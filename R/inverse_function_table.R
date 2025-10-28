#' reciprocal
#'
#' Computes the reciprocal (1/x) of its argument.
#'
#' @param x Numeric vector.
#' @return Numeric vector of reciprocals.
#' @examples
#' reciprocal(c(2, 4))
#'
#' @export
reciprocal <- function(x) {
  if (!is.numeric(x)) stop("reciprocal: input must be numeric", call. = FALSE)
  1/x
}

#' sqr
#'
#' Computes the square of its argument.
#'
#' @param x Numeric vector.
#' @return Numeric vector of squared values.
#' @examples
#' sqr(c(3, 4))
#'
#' @export
sqr <- function(x) {
  if (!is.numeric(x)) stop("sqr: input must be numeric", call. = FALSE)
  x * x
}

#' @title Table of Inverse Functions
#'
#' @description
#' Holds the mapping of functions to their inverses by function name (as character strings, not functions).
#'
#' @details
#' The table can be queried or expanded with new function-inverse pairs.
#'
#' @export
inverse_function_table <- data.frame(
  function_name      = c("log", "identity", "reciprocal", "sqrt"),
  inverse_function_name = c("exp", "identity", "reciprocal", "sqr"),
  stringsAsFactors = FALSE
)

#' Get the Inverse Function Name
#'
#' Retrieves the inverse function name for a given function name, searching both directions.
#'
#' @param fname Character. Function name.
#' @return Character vector of the inverse function name(s), or NULL if not found.
#' @examples
#' get_inverse_function("log")
#'
#' @export
get_inverse_function <- function(fname, table = NULL) {
  if (is.null(table)) table <- get("inverse_function_table", envir = parent.frame())
  idx <- which(fname == table$function_name)
  if (length(idx) > 0) return(table$inverse_function_name[idx])
  idx <- which(fname == table$inverse_function_name)
  if (length(idx) > 0) return(table$function_name[idx])
  NULL
}

#' Add Inverse Function Mapping
#'
#' Safely adds a function/inverse pair to the inverse function table.
#'
#' @param fname Character. Name of the function.
#' @param inv_fname Character. Name of the inverse function.
#' @param table Data frame. Registry of function-inverse pairs.
#' @param env Environment to assign updated table.
#' @return Updated table invisibly.
#' @examples
#' add_inverse_function("tan", "atan", env = globalenv())
#'
#' @export
add_inverse_function <- function(fname, inv_fname,
                                 table = inverse_function_table,
                                 env = globalenv()) {
  # Prevent duplication of entries
  if (fname %in% c(table$function_name, table$inverse_function_name) ||
      inv_fname %in% c(table$function_name, table$inverse_function_name))
    stop("Function or inverse already present in table.", call. = FALSE)

  # Check function exists in current environment and is a function of one argument
  for (f in c(fname, inv_fname)) {
    obj <- tryCatch(get(f, mode = "function", inherits = TRUE), error = function(e) NULL)
    if (is.null(obj) || !is.function(obj))
      stop(sprintf("'%s' must be an accessible R function.", f), call. = FALSE)
  }

  # Add to table (assign by reference by default, or explicit env)
  new_row <- data.frame(function_name = fname, inverse_function_name = inv_fname, stringsAsFactors = FALSE)
  table <- rbind(table, new_row)
  assign("inverse_function_table", table, envir = env)
  invisible(table)
}
