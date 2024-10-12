# Generic validation functions with object name included in error messages
validate_single_string <- function(input, allow_null = FALSE, arg_name = deparse(substitute(input))) {
  if (!allow_null && is.null(input)) {
    stop(sprintf("`%s` cannot be NULL.", arg_name))
  }

  if (!is.null(input) && (!is.character(input) || length(input) != 1)) {
    stop(sprintf("`%s` must be a single string.", arg_name))
  }
}

validate_function_list <- function(input_list, allow_null = FALSE, arg_name = deparse(substitute(input_list))) {
  if (!allow_null && is.null(input_list)) {
    stop(sprintf("`%s` cannot be NULL.", arg_name))
  }

  if (!is.null(input_list) && (!is.list(input_list) ||
                               !all(vapply(input_list, is.function, logical(1))))) {
    stop(sprintf("`%s` must be a list of functions, an empty list, or NULL.", arg_name))
  }
}

validate_mode_structure <- function(mode, allow_null = FALSE, arg_name = deparse(substitute(mode))) {
  if (!allow_null && is.null(mode)) {
    stop(sprintf("`%s` cannot be NULL.", arg_name))
  }

  if (!is.null(mode)) {
    if (!is.list(mode)) {
      stop(sprintf("`%s` must be a list or NULL.", arg_name))
    }

    if (!is.null(mode$name) && (!is.character(mode$name) || length(mode$name) != 1)) {
      stop(sprintf("`%s$name` must be a single string or NULL.", arg_name))
    }

    if (!is.null(mode$modifier) && !is.function(mode$modifier)) {
      stop(sprintf("`%s$modifier` must be a function or NULL.", arg_name))
    }

    if (!is.null(mode$extractor) && !is.function(mode$extractor)) {
      stop(sprintf("`%s$extractor` must be a function or NULL.", arg_name))
    }
  }
}

validate_object_class <- function(obj, class_name, allow_null = FALSE, arg_name = deparse(substitute(obj))) {
  if (!allow_null && is.null(obj)) {
    stop(sprintf("`%s` cannot be NULL.", arg_name))
  }

  if (!is.null(obj) && !inherits(obj, class_name)) {
    stop(sprintf("`%s` must be of class '%s' or NULL.", arg_name, class_name))
  }
}
