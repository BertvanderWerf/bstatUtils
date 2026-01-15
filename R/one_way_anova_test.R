#' Perform One-Way Analysis of Variance (ANOVA)
#'
#' Conducts a univariate one-way ANOVA test comparing means across groups.
#' Returns results as an \code{htest} object for consistency with base R statistical tests.
#'
#' @param y Numeric vector of response values (observations).
#' @param group Factor or grouping variable indicating group membership for each observation.
#'   Must have the same length as \code{y}.
#'
#' @returns
#' An object of class \code{htest} containing:
#' \itemize{
#'   \item \code{statistic} — F-statistic value
#'   \item \code{p.value} — associated p-value from the F-distribution
#'   \item \code{method} — description of the test performed
#'   \item \code{data.name} — names of the response and group variables
#'   \item \code{parameter} — numerator (df1) and denominator (df2) degrees of freedom
#' }
#'
#' @details
#' The function wraps \code{\link[stats]{aov}} and extracts key statistics (F-value, p-value, df)
#' from the ANOVA summary table. Results conform to the standard \code{htest} class for
#' compatibility with other statistical functions and printing methods.
#'
#' @examples
#' # Simple one-way ANOVA
#' set.seed(42)
#' response <- rnorm(30, mean = 5, sd = 1)
#' groups <- factor(rep(c("A", "B", "C"), 10))
#' one_way_anova_test(response, groups)
#'
#' @export
one_way_anova_test <- function(y, group) {
  # Capture input names for reporting
  y_name <- as.character(deparse(substitute(y)))
  group_name <- as.character(deparse(substitute(group)))

  # Validate inputs
  y <- bstatErr::check_numeric_vector(y)
  # empty levels will be dropped in aov
  group <- bstatErr::check_factor(group, allow_empty_levels = TRUE)

  if (length(y) != length(group)) {
    stop("`y` and `group` must have the same length.")
  }

  # Perform one-way ANOVA
  anova_model <- stats::aov(y ~ group)
  anova_summary <- summary(anova_model)[[1]]

  # Extract F-statistic, p-value, and degrees of freedom
  f_statistic <- anova_summary$`F value`[1]
  p_value <- anova_summary$`Pr(>F)`[1]
  df_numerator <- anova_summary$Df[1]
  df_denominator <- anova_summary$Df[2]

  # Construct htest object
  result <- list(
    statistic = c("F" = f_statistic),
    p.value = p_value,
    method = "Univariate one-way analysis of variance",
    data.name = paste(y_name, "and", group_name),
    parameter = c(df1 = df_numerator, df2 = df_denominator)
  )

  class(result) <- c("htest", "list")
  result
}
