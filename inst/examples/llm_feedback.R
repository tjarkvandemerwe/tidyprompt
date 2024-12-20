# Example usage within a validation function similar to the one in 'answer_as_integer()':
validation_fn <- function(x, min = 0, max = 100) {
  if (x != floor(x)) { # Not a whole number
    return(llm_feedback(
      "You must answer with only an integer (use no other characters)."
    ))
  }
  if (!is.null(min) && x < min) {
    return(llm_feedback(glue::glue(
      "The number should be greater than or equal to {min}."
    )))
  }
  if (!is.null(max) && x > max) {
    return(llm_feedback(glue::glue(
      "The number should be less than or equal to {max}."
    )))
  }
  return(TRUE)
}

# This validation_fn would be part of a prompt_wrap();
#   see the `answer_as_integer()` function for an example of how to use it
