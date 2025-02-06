#' Function to extract a specific element from a list
#'
#' This function is intended as a helper function for piping with output from
#' [send_prompt()] when using `return_mode = "full"`. It allows to
#' extract a specific element from the list returned by [send_prompt()], which
#' can be useful for further piping.
#'
#' @param list A list, typically the output from [send_prompt()] with `return_mode = "full"`
#' @param name_of_element A character string with the name of the element to extract from the list
#'
#' @return The extracted element from the list
#'
#' @export
#'
#' @example inst/examples/extract_from_return_list.R
#'
#' @family miscellaneous_helpers
extract_from_return_list <- function(list, name_of_element = "response") {
  if (!is.list(list)) stop("Input must be a list")

  if (!name_of_element %in% names(list)) stop("Element not found in list")

  return(list[[name_of_element]])
}
