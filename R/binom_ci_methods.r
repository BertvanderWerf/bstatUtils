#' Binomial proportion confidence intervals
#'
#' Compute a confidence interval for a single binomial proportion.
#'
#' These methods are useful for quantities such as sensitivity, specificity,
#' prevalence, or any other proportion estimated from binomial data.
#'
#' @param n Number of successes. Must be a single integer-like numeric value
#'   satisfying `0 <= n <= N`.
#' @param N Total number of trials. Must be a single positive integer-like
#'   numeric value.
#' @param method Character string specifying the interval method. Supported
#'   methods are `"wald"`, `"wilson"`, `"clopper-pearson"`,
#'   `"agresti-coull"`, `"jeffreys"`, `"logit"`, and `"likelihood"`.
#'   The default is `"wilson"`.
#' @param conf.level Confidence level in `(0, 1)`. Default is `0.95`.
#' @param cc Continuity correction used only by the logit method when `n = 0`
#'   or `n = N`. Must be non-negative. Default is `0.5`.
#'
#' @return A data frame with one row and columns `n`, `N`, `estimate`, `method`,
#'   `conf.level`, `lower`, and `upper`.
#'
#' @details
#' Method summaries:
#' \describe{
#'   \item{`"wald"`}{Normal approximation interval
#'   \eqn{\hat p \pm z_{1-\alpha/2}\sqrt{\hat p(1-\hat p)/N}}.
#'   Easy to compute but often inaccurate for small samples or extreme
#'   proportions.}
#'   \item{`"wilson"`}{Wilson score interval obtained by inverting the score
#'   test. Often a good general-purpose frequentist default.}
#'   \item{`"clopper-pearson"`}{Exact binomial interval based on beta
#'   quantiles. Conservative but guarantees at least nominal coverage under the
#'   binomial model.}
#'   \item{`"agresti-coull"`}{Adjusted Wald interval using
#'   \eqn{\tilde N = N + z^2} and \eqn{\tilde n = n + z^2/2}.}
#'   \item{`"jeffreys"`}{Bayesian equal-tailed interval under the Jeffreys prior
#'   \eqn{\mathrm{Beta}(1/2, 1/2)}.}
#'   \item{`"logit"`}{Wald interval on the logit scale, back-transformed to the
#'   probability scale. A continuity correction is used at the boundaries.}
#'   \item{`"likelihood"`}{Likelihood-ratio interval obtained by numerically
#'   inverting the binomial likelihood-ratio test.}
#' }
#'
#' @references
#' Wilson EB (1927). Probable inference, the law of succession, and statistical
#' inference. \emph{Journal of the American Statistical Association}, 22,
#' 209-212.
#'
#' Clopper CJ, Pearson ES (1934). The use of confidence or fiducial limits
#' illustrated in the case of the binomial. \emph{Biometrika}, 26, 404-413.
#'
#' Agresti A, Coull BA (1998). Approximate is better than “exact” for interval
#' estimation of binomial proportions. \emph{The American Statistician}, 52,
#' 119-126.
#'
#' Brown LD, Cai TT, DasGupta A (2001). Interval estimation for a binomial
#' proportion. \emph{Statistical Science}, 16, 101-133.
#'
#' @examples
#' binom_ci(19, 20)
#' binom_ci(19, 20, method = "clopper-pearson")
#' binom_ci_all(19, 20)
#'
#' @export
binom_ci <- function(n,
                     N,
                     method = c(
                       "wilson", "wald", "clopper-pearson",
                       "agresti-coull", "jeffreys", "logit", "likelihood"
                     ),
                     conf.level = 0.95,
                     cc = 0.5) {
  method <- match.arg(method)

  validate_binom_inputs(n = n, N = N, conf.level = conf.level, cc = cc)

  x <- as.numeric(n)
  N <- as.numeric(N)
  p_hat <- x / N
  alpha <- 1 - conf.level
  z <- stats::qnorm(1 - alpha / 2)

  interval <- switch(
    method,
    "wald" = wald_ci(x, N, z),
    "wilson" = wilson_ci(x, N, z),
    "clopper-pearson" = clopper_pearson_ci(x, N, alpha),
    "agresti-coull" = agresti_coull_ci(x, N, z),
    "jeffreys" = jeffreys_ci(x, N, alpha),
    "logit" = logit_ci(x, N, z, cc),
    "likelihood" = likelihood_ci(x, N, conf.level)
  )

  interval["lower"] <- max(0, interval["lower"])
  interval["upper"] <- min(1, interval["upper"])

  data.frame(
    n = x,
    N = N,
    estimate = p_hat,
    method = method,
    conf.level = conf.level,
    lower = unname(interval["lower"]),
    upper = unname(interval["upper"]),
    row.names = NULL,
    check.names = FALSE
  )
}

#' Compute all implemented binomial confidence intervals
#'
#' Convenience wrapper that evaluates all supported methods in [binom_ci()] for
#' the same observed count and total.
#'
#' @param n Number of successes.
#' @param N Total number of trials.
#' @param conf.level Confidence level in `(0, 1)`. Default is `0.95`.
#' @param cc Continuity correction used by the logit method at the boundaries.
#'   Default is `0.5`.
#'
#' @return A data frame with one row per method.
#'
#' @examples
#' binom_ci_all(19, 20)
#'
#' @export
binom_ci_all <- function(n, N, conf.level = 0.95, cc = 0.5) {
  methods <- c(
    "wald", "wilson", "clopper-pearson", "agresti-coull",
    "jeffreys", "logit", "likelihood"
  )

  out <- lapply(
    methods,
    function(current_method) {
      binom_ci(
        n = n,
        N = N,
        method = current_method,
        conf.level = conf.level,
        cc = cc
      )
    }
  )

  do.call(rbind, out)
}

validate_binom_inputs <- function(n, N, conf.level, cc) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 0 || n != floor(n)) {
    stop(
      "`n` must be a single non-missing integer-like numeric value >= 0.",
      call. = FALSE
    )
  }

  if (!is.numeric(N) || length(N) != 1L || is.na(N) || N <= 0 || N != floor(N)) {
    stop(
      "`N` must be a single non-missing positive integer-like numeric value.",
      call. = FALSE
    )
  }

  if (n > N) {
    stop("`n` must not exceed `N`.", call. = FALSE)
  }

  if (!is.numeric(conf.level) || length(conf.level) != 1L || is.na(conf.level) ||
      conf.level <= 0 || conf.level >= 1) {
    stop("`conf.level` must be a single numeric value in (0, 1).", call. = FALSE)
  }

  if (!is.numeric(cc) || length(cc) != 1L || is.na(cc) || cc < 0) {
    stop("`cc` must be a single non-missing numeric value >= 0.", call. = FALSE)
  }

  invisible(NULL)
}

wald_ci <- function(x, N, z) {
  p_hat <- x / N
  se <- sqrt(p_hat * (1 - p_hat) / N)
  c(lower = p_hat - z * se, upper = p_hat + z * se)
}

wilson_ci <- function(x, N, z) {
  p_hat <- x / N
  denom <- 1 + z^2 / N
  center <- (p_hat + z^2 / (2 * N)) / denom
  half_width <- z * sqrt(p_hat * (1 - p_hat) / N + z^2 / (4 * N^2)) / denom
  c(lower = center - half_width, upper = center + half_width)
}

clopper_pearson_ci <- function(x, N, alpha) {
  lower <- if (x == 0) 0 else stats::qbeta(alpha / 2, x, N - x + 1)
  upper <- if (x == N) 1 else stats::qbeta(1 - alpha / 2, x + 1, N - x)
  c(lower = lower, upper = upper)
}

agresti_coull_ci <- function(x, N, z) {
  N_tilde <- N + z^2
  x_tilde <- x + z^2 / 2
  p_tilde <- x_tilde / N_tilde
  se <- sqrt(p_tilde * (1 - p_tilde) / N_tilde)
  c(lower = p_tilde - z * se, upper = p_tilde + z * se)
}

jeffreys_ci <- function(x, N, alpha) {
  c(
    lower = stats::qbeta(alpha / 2, x + 0.5, N - x + 0.5),
    upper = stats::qbeta(1 - alpha / 2, x + 0.5, N - x + 0.5)
  )
}

logit_ci <- function(x, N, z, cc) {
  if (x == 0 || x == N) {
    p_use <- (x + cc) / (N + 2 * cc)
    se <- sqrt(1 / (x + cc) + 1 / (N - x + cc))
  } else {
    p_use <- x / N
    se <- sqrt(1 / x + 1 / (N - x))
  }

  eta <- stats::qlogis(p_use)
  c(
    lower = stats::plogis(eta - z * se),
    upper = stats::plogis(eta + z * se)
  )
}

likelihood_ci <- function(x, N, conf.level) {
  cutoff <- stats::qchisq(conf.level, df = 1)

  if (x == 0) {
    upper <- stats::uniroot(
      function(p) 2 * (0 - dbinom_loglik(x, N, p)) - cutoff,
      interval = c(.Machine$double.eps, 1 - 1e-12)
    )$root

    return(c(lower = 0, upper = upper))
  }

  if (x == N) {
    lower <- stats::uniroot(
      function(p) 2 * (0 - dbinom_loglik(x, N, p)) - cutoff,
      interval = c(1e-12, 1 - .Machine$double.eps)
    )$root

    return(c(lower = lower, upper = 1))
  }

  p_hat <- x / N
  target <- function(p) 2 * (dbinom_loglik(x, N, p_hat) - dbinom_loglik(x, N, p)) - cutoff

  c(
    lower = stats::uniroot(target, interval = c(1e-12, p_hat - 1e-12))$root,
    upper = stats::uniroot(target, interval = c(p_hat + 1e-12, 1 - 1e-12))$root
  )
}

dbinom_loglik <- function(x, N, p) {
  if (p <= 0 || p >= 1) {
    return(-Inf)
  }

  x * log(p) + (N - x) * log1p(-p)
}
