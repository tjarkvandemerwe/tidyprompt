#' Create a new prompt object
#'
#' This function constructs a `prompt` object that contains the question,
#' expected reply format, a function to extract the reply, and validation
#' functions to ensure the response is valid.
#'
#' @param prompt A string containing the question or prompt text.
#' @param reply_format A string describing the desired format or structure of the reply (optional).
#' @param reply_extract A function that extracts the reply from the full response (optional).
#' @param validation A list of validation functions, each checking the extracted response for validity.
#'   If any function returns `FALSE`, the prompt is sent again, including the previous response
#'   and validation error (optional).
#'
#' @return A `prompt` object with the specified components.
#'
#' @examples
#' # Create a simple prompt with no validation
#' p <- prompt("What is your name?")
#'
#' # Create a prompt with a format, reply extraction, and validation
#' p <- prompt(
#'   "Enter your age:",
#'   reply_format = "numeric",
#'   reply_extract = as.numeric,
#'   validation = list(function(x) x > 0)
#' )
#'
#' @export
new_prompt <- function(
    base_prompt, # Single string

    modifiers = list(), # List of functions that modify base_prompt string

    mode = list(
      name = NULL, # Name of mode
      modifier = NULL, # Function that modifies prompt based on mode (applied after regular modifiers)
      extractor = NULL # Function that extracts from LLM-reply based on mode (applied before regular extractors)
    ),

    tools = list(), # List of functions that LLM can call

    extractors = list(), # List of functions that extract from LLM-reply
    validators = list(), # List of functions that validate extracted data

    llm_provider = NULL # llm_provider object
) {
  # Validate base_prompt
  if (!is.character(base_prompt) || length(base_prompt) != 1) {
    stop("`base_prompt` must be a single string.")
  }

  # Validate function lists
  function_lists <- list(
    modifiers = modifiers, tools = tools, extractors = extractors, validators = validators
  )
  lapply(names(function_lists), function(name) {
    if (!is.null(function_lists[[name]]) &&
        (!is.list(function_lists[[name]]) || !all(vapply(function_lists[[name]], is.function, logical(1))))) {
      stop(sprintf("`%s` must be a list of functions, an empty list, or NULL.", name))
    }
  })

  # Validate mode
  validate_mode <- function(mode) {
    if (!is.null(mode) && !is.list(mode)) {
      stop("`mode` must be a list or NULL.")
    }

    if (is.null(mode) || length(mode) == 0) {
      return()  # Valid if `mode` is NULL or an empty list
    }

    # Check that mode is a list and has the expected structure
    if (!is.null(mode$name) ||
        (is.character(mode$name) & length(mode$name) != 1) ||
        !is.null(mode$modifier) && !is.character(mode$modifier)) {
      stop("`mode$name` must be a single string or NULL.")
    }

    if (!is.null(mode$modifier) && !is.function(mode$modifier)) {
      stop("`mode$modifier` must be a single function or NULL.")
    }

    if (!is.null(mode$extractor) && !is.function(mode$extractor)) {
      stop("`mode$extractor` must be a single function or NULL.")
    }
  }
  validate_mode(mode)

  # Validate llm_provider
  if (!is.null(llm_provider) && !inherits(llm_provider, "llm_provider")) {
    stop("`llm_provider` must be an llm_provider object or NULL.")
  }

  # Define function which constructs the prompt from the base_prompt and modifiers
  construct <- function() {
    # Append mode modifier at the end of modifiers
    all_modifiers <- c(modifiers)
    if (!is.null(mode$modifier)) {
      all_modifiers <- c(all_modifiers, mode$modifier)
    }

    if (length(all_modifiers) == 0) {
      return(base_prompt)
    }

    # Use Reduce to apply all modifiers to the base prompt
    return(Reduce(function(prompt, modifier) {
      modifier(prompt)
    }, all_modifiers, init = base_prompt))
  }

  # Construct and return the prompt object with construct_prompt as a method
  prompt_object <- structure(
    list(
      base_prompt = base_prompt,
      modifiers = modifiers,
      mode = mode,
      tools = tools,
      extractors = extractors,
      validators = validators,
      llm_provider = llm_provider,
      construct_prompt = construct_prompt # Attach the function as a method
    ),
    class = "prompt"
  )

  return(prompt_object)
}


# Example modifiers
modifier1 <- function(prompt) paste(prompt, "with modifier 1")
modifier2 <- function(prompt) paste(prompt, "and modifier 2")

# Create a new prompt object
prompt <- new_prompt(
  base_prompt = "This is the base prompt.",
  modifiers = list(modifier1, modifier2)
)

# Call the construct_prompt method
final_prompt <- prompt$construct_prompt()
print(final_prompt) # Output: "This is the base prompt. with modifier 1 and modifier 2"







#' User-friendly prompt constructor
#'
#' A wrapper for `new_prompt()` that allows for creating prompts more easily.
#'
#' @inheritParams new_prompt
#' @return A `prompt` object.
#' @export
prompt <- function(prompt,
                   reply_format = NULL,
                   reply_extract = NULL,
                   validation = NULL) {
  new_prompt(prompt, reply_format, reply_extract, validation)
}
