#' Prompt object
#'
#' @param base_prompt The base prompt string
#' @param modifiers List of functions that modify base_prompt string. Applied before mode modifier
#' @param mode List with mode name, mode modifier, and mode extractor. Mode modifier is applied after regular modifiers;
#'  mode extractor is applied before regular extractors
#' @param tools Named list of functions that LLM can call
#' @param extractors List of functions that extract content from LLM-reply. During query_llm,
#' extracted content is passed to the validators and ultimately returned to the user.
#' @param validators List of functions that validate extracted content
#' @param llm_provider llm_provider object
#'
#' @return A prompt object
#' @export
new_prompt <- function(
    base_prompt, # Single string

    modifiers = list(), # List of functions that modify base_prompt string

    mode = list(
      name = NULL, # Name of mode
      modifier = NULL, # Function that modifies prompt based on mode (applied after regular modifiers)
      extractor = NULL # Function that extracts from LLM-reply based on mode (applied before regular extractors)
    ),

    tools = list(), # Named list of functions that LLM can call. Names are provided as tool descriptions to the LLM

    extractors = list(), # List of functions that extract from LLM-reply
    validators = list(), # List of functions that validate extracted data

    llm_provider = NULL # llm_provider object
) {
  validate_single_string(base_prompt, allow_null = FALSE)
  validate_function_list(modifiers, allow_null = TRUE)
  validate_mode_structure(mode, allow_null = TRUE)
  validate_function_list(tools, allow_null = TRUE)
  validate_function_list(extractors, allow_null = TRUE)
  validate_function_list(validators, allow_null = TRUE)
  validate_object_class(llm_provider, "llm_provider", allow_null = TRUE)

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
    constructed_prompt <- Reduce(function(prompt, modifier) {
      modifier(prompt)
    }, all_modifiers, init = base_prompt)

    # Add tools to constructed prompt
    if (length(tools) > 0) {
      constructed_prompt <- glue::glue(
        "{constructed_prompt}

        You have the following tools available:"
      )

      for (i in 1:length(tools)) {
        tool_name <- names(tools)[i]
        tool_function <- tools[[i]]
        arguments <- formals(tool_function)
        arguments_string <- paste(names(arguments), collapse = ", ")

        # TODO: Add more information about the tool, potentially based on
        #   docstring-documentation provided within the function?
        #     E.g., description, what is returned, example usage, etc.
        constructed_prompt <- glue::glue(
          "{constructed_prompt}

            Tool_name: {tool_name}
            Arguments: {arguments_string}"
        )
      }

      constructed_prompt <- glue::glue(
        "{constructed_prompt}

        You can use a tool by typing TOOL[tool_name](arguments)."
      )
    }

    return(constructed_prompt)
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
      construct = construct
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
  modifiers = list(modifier1, modifier2),
  tools = list(
    `mean` = mean
  )
)

prompt$construct()
