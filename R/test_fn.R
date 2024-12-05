#' Title
#'
#' Description
#'
#' @param no_arg No default
#' @param null_arg Default NULL
#' @param call_arg Call
#' @param string_arg String
#' @param number_arg Number
#' @param integer_arg Integer
#' @param logical_arg Logical
#' @param vector_arg_numeric Numeric vector
#' @param vector_arg_character Character vector
#' @param vector_arg_unknown Unknown vector
#' @param vector_arg_mixed Mixed vector
#' @param vector_arg_match Match vector
#' @param list_arg List
#' @param named_list_arg Named list
#' @param nested_list_arg Nested list
#' @param nested_named_list_arg Nested named list
#'
#' @return ...
#' @export
test_fn <- function(
    no_arg,
    null_arg = NULL,
    call_arg = list.files(),
    string_arg = "string",
    number_arg = 123.45,
    integer_arg = 123,
    logical_arg = TRUE,
    vector_arg_numeric = c(1),
    vector_arg_character = c("a"),
    vector_arg_unknown = c(),
    vector_arg_mixed = c(1, "a", TRUE),
    vector_arg_match = c("a", "b", "c"),
    list_arg = list(1, 2, 3),
    named_list_arg = list(a = 1, b = 2, c = 3),
    nested_list_arg = list(list(1, 2), list(3, 4)),
    nested_named_list_arg = list(a = list(1, 2), b = list(3, 4))
) {
  return(invisible(NULL))
}
