# Create a constructor for Prompt S3 class
create_prompt_wrap <- function(
    prompt_text = NULL,
    type = c("unspecified", "mode", "toolset"),
    modify_fn = NULL,
    modify_fn_args = list(),
    validation_functions = list(),
    extractor_functions = list(),
    llm_provider = NULL
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
    llm_provider = llm_provider
  )

  class(prompt_wrap) <- "prompt_wrap"
  return(prompt_wrap)
}

# Wrapper function to create a prompt
#   (which is also just a prompt_wrap, but with a promp_text and not a modify_fn)
create_prompt <- function(prompt_text, ...) {
  return(create_prompt_wrap(prompt_text = prompt_text, ...))
}

# We will organise base_prompt and all the prompt_wraps in a list, a prompt list
# This list can be a regular list but needs to meet certain criteria, checked in the
# validate_prompt_list function. This function will also convert a single prompt wrap
# to a list. Anywhere that we work with a prompt list, this function should be called first
validate_prompt_list <- function(prompt_wrap_or_list) {
  # If prompt_wrap_or_list is a single string, we will create a prompt from that
  if (is(prompt_wrap_or_list, "character") && length(prompt_wrap_or_list) == 1) {
    prompt_wrap_or_list <- create_prompt(prompt_wrap_or_list)
  }

  if (is(prompt_wrap_or_list, "prompt_wrap")) {
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


# Construct prompt text from prompt list
construct_prompt_text <- function(prompt_wrap_or_list) {
  prompt_list <- validate_prompt_list(prompt_wrap_or_list) |>
    correct_prompt_list_order()

  prompt_text <- prompt_list[[1]]$prompt_text
  for (i in 2:length(prompt_list)) {
    prompt_text <- prompt_list[[i]]$modify_fn(prompt_text, prompt_list[[i]]$modify_fn_args)
  }

  return(prompt_text)
}

# Example function that adds text at the end of an existing prompt
add_text_to_prompt <- function(prompt_wrap_or_list, text, sep = "\n\n") {
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

# Example mode function
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

#### 2 Example usage ####

# Create prompt with extra text
pw <- create_prompt(
  prompt_text = "Enter a number between 1 and 10",
  validation_functions = list(
    function(x) {
      if (x < 1 || x > 10) {
        return("The number should be between 1 and 10")
      }
      return(TRUE)
    }
  )
) |>
  add_text_to_prompt("Some text added to the end of the prompt.")

# Construct prompt text from list of prompt wrappers
construct_prompt_text(pw) |>
  cat()

# Example; starting from string, adding mode in the middle but placed at the end
"Hi there!" |>
  add_text_to_prompt("Some text added to the end of the prompt.") |>
  add_example_mode() |> # This will be placed at the end, even though we add it in the middle
  add_text_to_prompt("More text to add (to be placed before the mode)") |>
  construct_prompt_text() |>
  cat()
