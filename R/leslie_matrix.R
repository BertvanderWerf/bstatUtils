#' Construct a Leslie Matrix
#'
#' Builds a Leslie matrix either from the size parameter `n` or a survival
#' probability vector `p`. Either `n` or `p` must be provided.
#'
#' @param n Integer. Number of age classes minus one. Used to construct a symbolic Leslie matrix.
#' @param p Numeric vector. Survival probabilities used to build a numeric Leslie matrix.
#' @param use_subscripts Logical. If TRUE, parameter indices show as Unicode subscripts.
#' @param add_lambda Logical. If TRUE, appends lambda (λ) or L (depending on use_subscripts) terms for symbolic formulations for the determinant.
#'
#' @return A matrix (numeric or character) representing the Leslie matrix.
#' @examples
#' leslie_matrix(n = 3)
#' leslie_matrix(p = c(0.8, 0.6, 0.4))
#' @export
leslie_matrix <- function(n = NULL, p = NULL, use_subscripts = TRUE, add_lambda = TRUE) {

  # Validate inputs -----------------------------------------------------------
  bstatErr::check_numeric(n, allow_null = TRUE)
  # bstatErr::check_numeric_vec(p, allow_null = TRUE)

  if (is.null(n) && is.null(p)) {
    stop("Both 'n' and 'p' are NULL. Provide at least one argument.", call. = FALSE)
  }

  # Case 1: probability vector (numeric Leslie matrix) ------------------------
  if (!is.null(p)) {
    if (length(p)<1) {
      stop("The length of parameter P should be at least 1.", call. = FALSE)
    }
    M <- cbind(rbind(p, diag(1 - p)), c(1, 0 * p))
    return(M)
  }

  # Case 2: size parameter (symbolic matrix) ---------------------------------
  if (n<2) {
    stop("The value of n cannot be smaller than 2.", call. = FALSE)
  }
  n <- n - 1
  i <- 0:(n - 1)
  lambda_symbol <- ""
  P <- NULL

  if (use_subscripts) {
    subscripts <- c('\u2080','\u2081','\u2082','\u2083','\u2084','\u2085','\u2086','\u2087','\u2088','\u2089')
    P <- sapply(i, function(idx) {
      digits <- strsplit(as.character(idx), '')[[1]]
      paste0("p", paste(subscripts[as.numeric(digits) + 1], collapse = ''))
    })
  } else {
    P <- paste('p', i, sep = '_')
  }

  if (isTRUE(add_lambda)) {
    if (use_subscripts) {
      lambda_symbol <- "-\u03BB"
    } else {
      lambda_symbol <- '-L'
    }
  }


  # Initialize symbolic Leslie matrix ----------------------------------------
  M <- matrix('0', n + 1, n + 1)
  M[1, ] <- c(P, '1')

  for (j in 1:n) {
    M[j + 1, j] <- paste0("1-", P[j])
  }

  # Add lambda (λ) notation if requested -------------------------------------
  if (isTRUE(add_lambda)) {
    rows <- nrow(M)
    cols <- ncol(M)
    lambda_matrix <- matrix(rep(c(lambda_symbol, rep("", rows)), length(M))[1:length(M)],
                            nrow = rows, ncol = cols)
    M <- matrix(paste0(M, lambda_matrix), nrow = rows, ncol = cols)
    M[M == paste0("0", lambda_symbol)] <- lambda_symbol
  }

  return(M)
}
