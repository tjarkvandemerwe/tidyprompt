# Example usage within an extraction function similar to the one in 'quit_if()':
extraction_fn <- function(x) {
  quit_detect_regex <- "NO ANSWER"

  if (grepl(quit_detect_regex, x)) {
      return(llm_break(
        object_to_return = NULL,
        success = TRUE
      ))
  }

  return(x)
}

# This extraction_fn would be part of a prompt_wrap()
