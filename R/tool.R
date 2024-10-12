#' Example function to pass to LLM
#'
#' @param location ...
#' @param unit ...
#'
#' @return ...
temperature_in_location <- function(
    location = c("Amsterdam", "Utrecht", "Enschede"),
    unit = c("Celcius", "Fahrenheit")
) {
  #' llm_tool::name temperature_in_location
  #'
  #' llm_tool::description Get the temperature in a location
  #'
  #' llm_tool::param location Location, must be one of: "Amsterdam", "Utrecht", "Enschede"
  #' llm_tool::param unit Unit, must be one of: "Celcius", "Fahrenheit"
  #'
  #' llm_tool::return The temperature in the specified location and unit
  #'
  #' llm_tool::example
  #' temperature_in_location("Amsterdam", "Fahrenheit")
  location <- match.arg(location)
  unit <- match.arg(unit)

  temperature_celcius <- switch(
    location,
    "Amsterdam" = 32.5,
    "Utrecht" = 19.8,
    "Enschede" = 22.7
  )

  if (unit == "Celcius") {
    return(temperature_celcius)
  } else {
    return(temperature_celcius * 9/5 + 32)
  }
}




#' Extract a specific section from a function's docstring-like documentation block
#'
#' This is a helper function for extract_function_docs.
#'
#' @param doc_lines A character vector of lines from a function's documentation block
#' @param section_keyword The keyword to search for in the documentation block, e.g., '@param'
#'
#' @return The extracted section as a character string or list
#' @export
extract_doc_section <- function(doc_lines, section_keyword) {
  # Identify lines that contain the section keyword
  section_starts <- grep(paste0("^\\s*#'\\s*", section_keyword), doc_lines)

  # Stop if the section keyword is not found
  if (length(section_starts) == 0) {
    return(character(0))  # Return an empty character vector if no section found
  }

  # Function to clean a single line
  clean_line <- function(line) {
    line <- gsub("^\\s*#'\\s*", "", line)  # Remove leading "#' "
    line <- gsub("\\s*#'$", "", line)      # Remove trailing "#'"
    return(trimws(line))  # Trim any remaining whitespace
  }

  # For parameters, we want to return all of them
  if (section_keyword == "llm_tool::param") {
    param_lines <- doc_lines[section_starts]

    # Clean up the lines
    param_lines <- sapply(param_lines, clean_line)

    # Remove section keyword from the start of each line; also remove leading "'# ' if it's there
    param_lines <- gsub(paste0("^", section_keyword, "\\s*"), "", param_lines)
    param_lines <- param_lines |> stats::setNames(NULL)

    params_list <- list()
    for (param_line in param_lines) {
      param_parts <- strsplit(param_line, " ", fixed = TRUE)[[1]]
      param_name <- param_parts[1]
      param_description <- paste(param_parts[-1], collapse = " ")
      params_list[[param_name]] <- param_description
    }

    return(params_list)
  }

  # For other sections, keep the existing logic but clean all lines
  section_start <- section_starts[1]

  # Determine where the section ends (next section or end of doc_lines)
  next_section <- grep("^\\s*#'\\s*llm_tool::", doc_lines)
  next_after_current <- next_section[next_section > section_start]

  if (length(next_after_current) == 0) {
    end_of_section <- length(doc_lines) + 1
  } else {
    end_of_section <- min(next_after_current)
  }

  # Extract and clean the section lines
  section_lines <- doc_lines[section_start:(end_of_section - 1)]
  section_lines <- sapply(section_lines, clean_line)

  # Combine the lines and trim whitespace
  trimmed_section <- trimws(paste(section_lines, collapse = "\n"))  # Join with newlines

  # Remove the section keyword from the start of the trimmed section
  trimmed_section <- gsub(paste0("^", section_keyword, "\\s*"), "", trimmed_section)

  # For title, return just the first line
  if (section_keyword == "") {
    return(strsplit(trimmed_section, "\n")[[1]][1])
  }

  return(trimmed_section)
}

#' Extract docstring-documentation from a function
#'
#' @param func A function object which has internal, roxygen-like documentation,
#' with the 'llm_tool::' tags: 'name', 'description', 'param', 'return', and 'example'
#' (e.g., llm_tool::name my_function_name).
#'
#' Note that for 'example' it must be a one-line example of how the function is used in R,
#' this will be converted to how LLM should call the function in text (slightly different
#' syntax).
#'
#' @return A list with the following elements:
#'  - name: The name of the function
#'  - description: A description of the function
#'  - parameters: A named list of parameters with descriptions
#'  - return_value: A description of the return value
#'  - example: An example of how the LLM should call the function
#' @export
extract_function_docs <- function(func) {
  # Convert the function to a character string
  func_text <- utils::capture.output(print(func))

  # Find documentation lines
  doc_lines <- grep("^\\s*#'", func_text, value = TRUE)

  # Extract parameters, return values, and examples using the generic function
  name <- extract_doc_section(doc_lines, "llm_tool::name")
  description <- extract_doc_section(doc_lines, "llm_tool::description")
  params <- extract_doc_section(doc_lines, "llm_tool::param")
  return_value <- extract_doc_section(doc_lines, "llm_tool::return")
  example <- extract_doc_section(doc_lines, "llm_tool::example")

  # Convert example to how LLM should call it
  converted_example <- sub("^(\\w+)\\((.*)\\)$", "FUNCTION[\\1](\\2)", example)
  converted_example <- gsub("(\"[^\"]*\")", "\\1", converted_example) # Keep the quotes around the arguments

  # Return the results as a list
  return(list(
    name = name,
    description = description,
    parameters = params,
    return_value = return_value,
    example = converted_example
  ))
}

# # Example usage:
# docs <- extract_function_docs(temperature_in_location)
# print(docs)

# Extractor function for tool prompts
tool_extractor <- function(llm_response, tool_functions) {
  # Check if the response contains a function call
  function_call <- stringr::str_match(llm_response, "FUNCTION\\[(.*?)\\]\\((.*?)\\)")

  # If no function call is found, return the response
  if (is.na(function_call[1, 1]) || nrow(function_call) == 0) {
    return(llm_response)
  }

  # Extract the function name and arguments
  function_name <- function_call[2]
  arguments <- function_call[3]

  # Clean up the arguments by removing quotes and splitting them
  arguments_list <- strsplit(gsub("\"", "", arguments), ",\\s*")[[1]]

  # Find the function in the list of tool functions
  tool_function <- tool_functions[[function_name]]

  # If the function is not found, return an error message
  if (is.null(tool_function)) {
    return(create_llm_feedback(glue::glue("Error: Function '{function_name}' not found.")))
  }

  # Call the tool function with the arguments and capture errors
  error <- FALSE
  result <- tryCatch({
    # Call the tool function with the arguments
    do.call(tool_function, as.list(arguments_list))
  }, error = function(e) {
    error <- TRUE
    # Capture the error message
    glue::glue("Error: {e$message}")
  })

  if (!error) {
    # Create some context around the result
    result <- glue::glue(
      "function called: {function_name}
      arguments used: {glue::glue_collapse(arguments_list, sep = ', ')}
      result: {result}"
    )
  }

  # Return the result (or the error feedback)
  return(create_llm_feedback(as.character(result)))
}



#' Add function-calling to prompt
#'
#' @param prompt_wrap_or_list ...
#' @param tool_functions ...
#'
#' @return ...
#' @export
add_tools <- function(prompt_wrap_or_list, tool_functions = list()) {
  prompt_list <- validate_prompt_list(prompt_wrap_or_list)

  # Check if tool_functions is single function, if so, convert to list
  if (length(tool_functions) == 1 && is.function(tool_functions))
    tool_functions <- list(tool_functions)
  # Check if is list & if all elements in list are functions
  if (
    !is.list(tool_functions) ||
    !all(sapply(tool_functions, is.function))
  )
    stop("tool_functions must be a single function or a list of functions.")
  if (length(tool_functions) == 0)
    stop("No tool functions provided.")

  # Convert tool_functions to named list, taking the name from the function documentation
  tool_functions <- setNames(tool_functions, sapply(tool_functions, function(f) {
    docs <- extract_function_docs(f)
    return(docs$name)
  }))

  # TODO: add extractor which will be responsible for tool execution

  # Add tool_functions as an attribute to the extractor
  attr(tool_extractor, "tool_functions") <- tool_functions

  new_wrap <- create_prompt_wrap(
    type = "tool",
    modify_fn = function(original_prompt_text, modify_fn_args) {
      new_prompt <- glue::glue(
        "{original_prompt_text}

        If you need more information, you can call functions to help you.
        To call a function, type:
          FUNCTION[<function name here>](<argument 1>, <argument 2>, etc...)

        The following functions are available:"
      )

      for (tool_function in modify_fn_args$tool_functions) {
        docs <- extract_function_docs(tool_function)

        new_prompt <- glue::glue(
          "{new_prompt}

          function name: {docs$name}
          description: {docs$description}
          arguments:
            {
              paste(
                sapply(names(docs$parameters), function(param_name) {
                  param_description <- docs$parameters[[param_name]]
                  glue::glue(\"  - {param_name}: {param_description}\")
                }),
                collapse = '\n            '
              )
            }
          return value: {docs$return_value}
          example usage: {docs$example}"
        )
      }

      new_prompt <- glue::glue(
        "{new_prompt}

        After you call a function, wait until you receive more information."
      )

      return(new_prompt)
    },
    modify_fn_args = list(tool_functions = tool_functions),
    extractor_functions = list(tool_extractor)
  )

  return(c(prompt_list, list(new_wrap)))
}

if (FALSE) {

  "Hi, what is the weather in Enschede?" |>
    add_tools(tool_functions = list(temperature_in_location)) |>
    send_prompt(llm_provider = create_ollama_llm_provider())

  # For test:
  # tool_functions <- list(temperature_in_location)
  # # Convert tool_functions to named list, taking the name from the function documentation
  # tool_functions <- setNames(tool_functions, sapply(tool_functions, function(f) {
  #   docs <- extract_function_docs(f)
  #   return(docs$name)
  # }))
  #
  # llm_response <- 'Let me call it with Enschede as the location and Celsius as the unit...
  # FUNCTION[temperature_in_location]("Enschede", "Celcius")'
  #
  # tool_extractor(llm_response, tool_functions)

}
