#' Update Named List Elements by Name
#'
#' Merges a list of existing values with a new set of updates matched by name.
#' Optionally creates or updates entries in the original list, matching by
#' supplied parameter names.
#'
#' @param old_list Named list; existing values to update.
#' @param new_list Named list or vector; new values to update or add (should match names in \code{param_names}).
#' @param param_names Character vector of valid parameter names.
#'
#' @return A named list with values from \code{old_list} updated by those in \code{new_list}.
#' @examples
#' update_by_name(list(a=1, b=2), list(b=5), c('a', 'b'))
#' update_by_name(NULL, c(a=3, b=4), c('a', 'b'))
#'
#' @export
update_by_name <- function(old_list, new_list, param_names) {
  # Handle NULL new_list: return old_list as list
  if (is.null(new_list)) {
    return(as.list(old_list))
  }
  # If new_list has no names, coerce using param_names (must exist)
  if (is.null(names(new_list))) {
    return(assign_list_circular(new_list, n = length(param_names), names = param_names))
  }
  # Coerce vector new_list to list, preserving names
  if (!is.list(new_list)) {
    name_vec <- names(new_list)
    new_list <- as.list(new_list)
    names(new_list) <- name_vec
  }
  # Handle NULL old_list: require that all param_names are present in new_list
  if (is.null(old_list)) {
    if (!all(param_names %in% names(new_list))) {
      stop("Undefined parameter name(s) in new_list.")
    }
    return(new_list[param_names])
  }
  # Ensure all new names are valid
  if (!all(names(new_list) %in% param_names)) {
    stop("Some names in new_list are not in param_names.", call. = FALSE)
  }
  # Merge by name: update old_list with values from new_list
  for (nm in names(new_list)) {
    old_list[[nm]] <- new_list[[nm]]
  }
  old_list
}
