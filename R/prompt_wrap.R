#### 1 Prompt wrap object & functions ####

#' Create a prompt wrap object
#'
#' @param prompt_text Text of the prompt; is typically supplied for the first prompt in a prompt list,
#'  and not for the rest of the prompts in the list.
#' @param type Type of the prompt; can be "unspecified", "mode", or "tool".
#' This is used to determine the order the prompt wrappers when constructing the final prompt text.
#' Mode prompts and toolset prompts are placed at the bottom of the prompt list (toolset after mode).
#' @param modify_fn Function that modifies the prompt text; takes two arguments: original_prompt_text and modify_fn_args.
#' This function will be applied to the previous prompt text in the prompt list.
#' @param modify_fn_args List of arguments to be passed to the modify_fn.
#' @param tool_functions List of tool functions to be added to the prompt.
#' @param extractor_functions List of functions that extract content from the response to the prompt.
#' Should return the extracted object on successful extraction, or a 'llm_feedback' object upon failure.
#' @param validation_functions List of functions that validate the (extracted) response to the prompt.
#' Should return TRUE on successful validation, or a 'llm_feedback' object upon failure.
#' @param llm_provider LLM provider object to be used for this prompt.
#' @param max_retries Maximum number of retries for this prompt.
#'
#' @return A prompt wrap object
#' @export
create_prompt_wrap <- function(
    prompt_text = NULL,
    type = c("unspecified", "mode", "toolset"),
    modify_fn = NULL,
    modify_fn_args = list(),
    validation_functions = list(),
    extractor_functions = list(),
    tool_functions = list(),
    llm_provider = NULL,
    max_retries = 10
) {
  # match.arg for type
  type <- match.arg(type)

  # If prompt_text is provided, modify_fn should not be provided
  if (!is.null(prompt_text) && !is.null(modify_fn)) {
    stop("Both prompt_text and modify_fn cannot be provided together.")
  }

  # Create a list to hold the prompt details
  prompt_wrap <- list(
    prompt_text = prompt_text,
    type = type,
    modify_fn = modify_fn,
    modify_fn_args = modify_fn_args,
    validation_functions = validation_functions,
    extractor_functions = extractor_functions,
    tool_functions = tool_functions,
    llm_provider = llm_provider,
    max_retries = max_retries
  )

  class(prompt_wrap) <- "prompt_wrap"
  return(prompt_wrap)
}


#' Create a base prompt wrap object
#'
#' Function to create a prompt wrap object which will be at the base of the
#' prompt list. Typically, prompt wrappers will be added to this base prompt.
#'
#' @param prompt_text Text of the prompt
#' @param ... Additional arguments to be passed to create_prompt_wrap
#'
#' @return A prompt wrap object
#' @export
#'
#' @examples
#' base_prompt <- tidyprompt("Enter a number between 1 and 10")
#'
#' # Note that if your base_prompt is only text, you do not need to use tidyprompt.
#' # You can also use the other functions directly with a string of text as the
#' # base prompt.
tidyprompt <- function(prompt_text, ...) {
  return(create_prompt_wrap(prompt_text = prompt_text, ...))
}

# We will organise base_prompt and all the prompt_wraps in a list, a prompt list
# This list can be a regular list but needs to meet certain criteria, checked in the
# validate_prompt_list function. This function will also convert a single prompt wrap
# to a list. Anywhere that we work with a prompt list, this function should be called first



#' Validate a prompt list
#'
#' Tidyprompt will use a list of prompt_wrap objects to organise a base prompt and
#' modifications to that base prompt, plus extractor and validation functions which
#' need to be applied after the prompt is sent to the LLM. An LLM provider,
#' to be used for evaluation of the final prompt, can also be kept within one of
#' the prompt wrap objects within the prompt list.
#'
#' @param prompt_wrap_or_list A single string, a prompt_wrap object, or a list
#' of prompt_wrap objects.
#'
#' @return A valid prompt list. If the input contains errors and cannot be turned
#' into a valid prompt list, an error will be thrown.
#' @export
validate_prompt_list <- function(prompt_wrap_or_list) {
  # If prompt_wrap_or_list is a single string, we will create a prompt from that
  if (methods::is(prompt_wrap_or_list, "character") && length(prompt_wrap_or_list) == 1) {
    prompt_wrap_or_list <- create_prompt_wrap(prompt_wrap_or_list)
  }

  if (methods::is(prompt_wrap_or_list, "prompt_wrap")) {
    prompt_wrap_or_list <- list(prompt_wrap_or_list)
  }

  if (!is.list(prompt_wrap_or_list)) {
    stop("The input should be a list of prompt_wrap objects")
  }
  if (length(prompt_wrap_or_list) == 0) {
    stop("The input list should not be empty")
  }

  # Check if all elements in the list are prompt_wrap objects
  if (!all(sapply(prompt_wrap_or_list, is, "prompt_wrap"))) {
    stop("All elements in the list should be prompt_wrap objects")
  }

  # The first element should contain prompt_text
  if (is.null(prompt_wrap_or_list[[1]]$prompt_text)) {
    stop("The first element of the prompt list should contain prompt_text")
  }

  # All elements past the first should contain modify_fn
  if (length(prompt_wrap_or_list) > 1 && !all(sapply(prompt_wrap_or_list[-1], function(x) !is.null(x$modify_fn)))) {
    stop("All elements past the first should contain modify_fn")
  }

  # Check modify_fn structure for elements that have it
  for (i in seq_along(prompt_wrap_or_list)) {
    if (!is.null(prompt_wrap_or_list[[i]]$modify_fn)) {
      if (length(formals(prompt_wrap_or_list[[i]]$modify_fn)) != 2) {
        stop("The modify_fn should take exactly two arguments")
      }
      if (!identical(names(formals(prompt_wrap_or_list[[i]]$modify_fn)), c("original_prompt_text", "modify_fn_args"))) {
        stop("The modify_fn arguments should be named exactly original_prompt_text and modify_fn_args")
      }
    }
  }

  return(invisible(prompt_wrap_or_list))
}

# Function to correct the order of a prompt list, to be used
# before constructing the final prompt text/when passing final prompt to the LLM
# Typically we want modes and toolsets to be at the bottom


#' Correct the order of a prompt list
#'
#' Reorder prompt wrap objects in a prompt list so that "mode" and "toolset" prompts
#' are placed at the bottom, and the final prompt can be properly constructed.
#'
#' @param prompt_list A list of prompt_wrap objects
#'
#' @return A list of prompt_wrap objects with the order corrected.
#' 'Mode' and 'toolset' prompts are placed at the bottom of the list.
#' @export
correct_prompt_list_order <- function(prompt_list) {
  # Validate that the input is a proper prompt list
  prompt_list <- validate_prompt_list(prompt_list)

  # Extract types from the prompt list, defaulting to "unspecified" if type is NULL
  types <- sapply(prompt_list, function(x) {
    if (!is.null(x$type)) x$type else "unspecified"
  })

  # Reorder the prompt list:
  # 1. Keep "unspecified" and other types at the top
  # 2. Place "mode" below them
  # 3. Place "toolset" at the bottom
  reordered_list <- c(
    prompt_list[types != "mode" & types != "toolset"], # Non-mode and non-toolset items
    prompt_list[types == "mode"],                      # Mode items
    prompt_list[types == "toolset"]                    # Toolset items
  )

  return(reordered_list)
}

#' Construct a final prompt text from a prompt list
#'
#' @param prompt_wrap_or_list A single string, a prompt_wrap object, or a list
#' of prompt_wrap objects.
#'
#' @return A character string containing the final prompt text.
#' @export
construct_prompt_text <- function(prompt_wrap_or_list) {
  prompt_list <- validate_prompt_list(prompt_wrap_or_list) |>
    correct_prompt_list_order()

  prompt_text <- prompt_list[[1]]$prompt_text
  if (length(prompt_list) > 1) {
    for (i in 2:length(prompt_list)) {
      prompt_text <- prompt_list[[i]]$modify_fn(prompt_text, prompt_list[[i]]$modify_fn_args)
    }
  }

  return(prompt_text)
}


#' Set the LLM provider for a prompt list
#'
#' @param prompt_wrap_or_list A single string, a prompt_wrap object, or a list
#' of prompt_wrap objects.
#' @param llm_provider llm_provider object to be used for evaluation
#' of the final prompt.
#'
#' @return A prompt list with the LLM provider set. The LLM provider
#' will be set for the base prompt in the list.
#' @export
set_llm_provider <- function(prompt_wrap_or_list, llm_provider) {
  prompt_list <- validate_prompt_list(prompt_wrap_or_list)

  prompt_list[[1]]$llm_provider <- llm_provider

  return(prompt_list)
}


#' Get the LLM provider from a prompt list
#'
#' @param prompt_list A list of prompt_wrap objects
#'
#' @return The LLM provider object from the prompt list. The first
#' LLM provider found in the list will be returned (starting from the base prompt).
#' If no LLM provider is found in the list, NULL will be returned.
#' @export
get_llm_provider_from_prompt_list <- function(prompt_list) {
  prompt_list <- validate_prompt_list(prompt_list)

  llm_provider <- NULL
  for (i in seq_along(prompt_list)) {
    if (!is.null(prompt_list[[i]]$llm_provider)) {
      llm_provider <- prompt_list[[i]]$llm_provider
      break
    }
  }

  return(llm_provider)
}

#' Get the validator and extractor functions from a prompt list
#'
#' @param prompt_list A list of prompt_wrap objects
#'
#' @return A list with two elements: 'extractors' and 'validations'.#'
#' 'extractors' is a list of extractor functions from the prompt list.#'
#' 'validations' is a list of validation functions from the prompt list.
#'
#' @export
get_extractors_and_validators_from_prompt_list <- function(prompt_list) {
  prompt_list <- validate_prompt_list(prompt_list) |>
    correct_prompt_list_order()

  extractors <- list()
  validators <- list()
  for (prompt_wrap in rev(prompt_list)) { # Reverse order
    for (fn in prompt_wrap$extractor_functions) {
      extractors <- c(extractors, fn)
    }
    for (fn in prompt_wrap$validation_functions) {
      validators <- c(validators, fn)
    }
  }
  return(list(extractors = extractors, validators = validators))
}



#### 2 Example prompt wrappers ####


#' Add text to a prompt
#'
#' Add text to a prompt by appending a prompt wrapper to the prompt list.
#' The text will be added to the end of the prompt text.
#'
#' @param prompt_wrap_or_list A single string, a prompt_wrap object, or a list
#' of prompt_wrap objects.
#' @param text Text to be added to the prompt.
#' @param sep Separator to be used between the original prompt text and the added text.
#'
#' @return A prompt list with an added prompt wrapper object which
#' will append the text to the end of the prompt text.
#' @export
add_text <- function(prompt_wrap_or_list, text, sep = "\n\n") {
  prompt_list <- validate_prompt_list(prompt_wrap_or_list)

  new_wrap <- create_prompt_wrap(
    modify_fn = function(original_prompt_text, modify_fn_args) {
      text <- modify_fn_args$text
      sep <- modify_fn_args$sep
      return(paste(original_prompt_text, text, sep = sep))
    },
    modify_fn_args = list(text = text, sep = sep)
  )

  return(c(prompt_list, list(new_wrap)))
}


#' Add a mode to a prompt (example function)
#'
#' @param prompt_wrap_or_list A single string, a prompt_wrap object, or a list
#' of prompt_wrap objects.
#'
#' @return A prompt list with an added mode prompt wrapper object.
#' @export
add_example_mode <- function(prompt_wrap_or_list) {
  prompt_list <- validate_prompt_list(prompt_wrap_or_list)

  new_wrap <- create_prompt_wrap(
    type = "mode",
    modify_fn = function(original_prompt_text, modify_fn_args) {
      return(glue::glue(
        "{original_prompt_text}

        Provide your answer in a way that a 5-year old would understand."
      ))
    },
    modify_fn_args = list()
  )

  # (May also want to add extractors for a mode, etc.)

  return(c(prompt_list, list(new_wrap)))
}


#' Make LLM answer as an integer (between min and max)
#'
#' @param prompt_wrap_or_list A single string, a prompt_wrap object, or a list
#' of prompt_wrap objects.
#' @param min (optional) Minimum value for the integer
#' @param max (optional) Maximum value for the integer
#' @param add_instruction_to_prompt (optional) Add instruction for replying
#' as an integer to the prompt text. Useful for debugging if extractors/validators
#' are working as expected (without instruction the answer should fail the
#' validation function, initiating a retry).
#'
#' @return A prompt list with an added prompt wrapper object which
#' will ensure that the LLM response is an integer.
#' @export
answer_as_integer <- function(
    prompt_wrap_or_list, min = NULL, max = NULL, add_instruction_to_prompt = FALSE
) {
  prompt_list <- validate_prompt_list(prompt_wrap_or_list)

  new_wrap <- create_prompt_wrap(
    modify_fn = function(original_prompt_text, modify_fn_args) {
      min <- modify_fn_args$min
      max <- modify_fn_args$max

      new_prompt_text <- original_prompt_text

      if (add_instruction_to_prompt) {
        new_prompt_text <- glue::glue(
          "{new_prompt_text}

        You must answer with only an integer (use no other characters)."
        )

        if (!is.null(min) && !is.null(max)) {
          new_prompt_text <- glue::glue(
            "{new_prompt_text}
          Enter an integer between {min} and {max}."
          )
        } else if (!is.null(min)) {
          new_prompt_text <- glue::glue(
            "{new_prompt_text}
          Enter an integer greater than or equal to {min}."
          )
        } else if (!is.null(max)) {
          new_prompt_text <- glue::glue(
            "{new_prompt_text}
          Enter an integer less than or equal to {max}."
          )
        }
      }

      return(new_prompt_text)
    },
    extractor_functions = list(
      function(x) {
        extracted <- suppressWarnings(as.integer(x))
        if (is.na(extracted)) {
          return(create_llm_feedback("You must answer with only an integer (use no other characters)."))
        }
        return(extracted)
      }
    ),
    validation_functions = list(
      function(x) {
        if (!is.null(min) && x < min) {
          return(create_llm_feedback(glue::glue(
            "The number should be greater than or equal to {min}."
          )))
        }
        if (!is.null(max) && x > max) {
          return(create_llm_feedback(glue::glue(
            "The number should be less than or equal to {max}."
          )))
        }

        return(TRUE)
      }
    )
  )

  return(c(prompt_list, list(new_wrap)))
}


#### 3 Example usage ####

if (FALSE) {

  # Create prompt with extra text
  pw <- tidyprompt(
    prompt_text = "Enter a number between 1 and 10",
    validation_functions = list(
      function(x) {
        if (x < 1 || x > 10) {
          return("The number should be between 1 and 10")
        }
        return(TRUE)
      }
    ),
    llm_provider = create_ollama_llm_provider()
  ) |>
    add_text("Some text added to the end of the prompt.")

  # Construct prompt text from list of prompt wrappers
  construct_prompt_text(pw) |>
    cat()

  # Example; starting from string, adding mode in the middle but placed at the end
  "Hi there!" |>
    add_text("Some text added to the end of the prompt.") |>
    add_example_mode() |> # This will be placed at the end, even though we add it in the middle
    add_text("More text to add (to be placed before the mode)") |>
    construct_prompt_text() |>
    cat()

  # Example passing to query_llm
  "Hi, how are you?" |>
    add_text("Maybe write a poem to express yourself?") |>
    add_example_mode() |>
    set_llm_provider(create_ollama_llm_provider()) |>
    send_prompt()

}
