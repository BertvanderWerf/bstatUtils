#' Increment Package Version in DESCRIPTION Using desc
#'
#' Increments the version in DESCRIPTION by "major", "minor", "patch", or "build".
#' Here, "build" is translated to "dev".
#'
#' @param folder Package folder containing DESCRIPTION.
#' @param increase Character. One of "major", "minor", "patch", "build".
#' @return Named character vector with old and new version.
#' @export
increment_version_desc <- function(folder = ".", increase = c("patch", "minor", "major", "build")) {
  increase <- match.arg(increase)
  if (increase == "build") increase <- "dev"

  # Use desc package
  d <- desc::desc(file = file.path(folder, "DESCRIPTION"))
  old_version <- as.character(d$get_version())
  d$bump_version(which = increase)
  new_version <- as.character(d$get_version())
  d$write(file = file.path(folder, "DESCRIPTION"))
  c(old_version = old_version, new_version = new_version)
}
