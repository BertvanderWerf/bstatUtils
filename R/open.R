#' Open a File or URL Using the System Default Application
#'
#' Opens a file or URL using the default program on Windows, macOS, or Linux.
#' Uses \code{shell.exec()} on Windows, \code{open} on macOS, and \code{xdg-open} on Linux.
#'
#' @param target Character scalar, file path or URL to open.
#'
#' @returns
#' Invisibly returns \code{TRUE} if the open command was executed (success not guaranteed), or \code{FALSE} if execution failed or OS is unsupported.
#'
#' @examples
#' # Open a webpage (platform dependent)
#' open("https://r-project.org")
#'
#' # Open a file (platform dependent)
#' # open("path/to/your/file.txt")
#'
#' @export
open <- function(target) {
  # Validate input
  bstatErr::check_string(target)

  sysname <- Sys.info()[["sysname"]]
  success <- FALSE

  if (.Platform$OS.type == "windows") {
    # Windows: shell.exec
    tryCatch({
      shell.exec(target)
      success <- TRUE
    }, error = function(e) {
      warning("Failed to open '", target, "' with shell.exec(): ", e$message)
      success <<- FALSE
    })
  } else if (sysname == "Darwin") {
    # macOS: open
    cmd <- sprintf('open "%s"', target)
    res <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
    success <- (res == 0)
    if (!success)
      warning("Failed to open '", target, "' with open (macOS).")
  } else if (.Platform$OS.type == "unix") {
    # Linux: xdg-open
    cmd <- sprintf('xdg-open "%s"', target)
    res <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
    success <- (res == 0)
    if (!success)
      warning("Failed to open '", target, "' with xdg-open (Linux).")
  } else {
    warning(
      "Unsupported OS: open works on Windows, macOS, and Linux only."
    )
  }

  invisible(success)
}
