#' Enable R function calling for prompt evaluation by a LLM
#'
#' This function adds the ability for the a LLM to call R functions.
#' Users can specify a list of functions that the LLM can call, and the
#' prompt will be modified to include information, as well as an
#' accompanying extraction function to call the functions (handled by
#' [send_prompt()]). Functions should contain docstring-like documentation
#' within them, as this will be parsed to provide the LLM with information
#' about the function's purpose and its arguments.
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param tool_functions A list of R functions that the LLM can call.
#' These functions should contain docstring-like documentation within them.
#' See [add_tools_extract_documentation()] for more details.
#'
#' @return A [tidyprompt()] with an added [prompt_wrap()] which
#' will allow the LLM to call R functions
#'
#' @export
#'
#' @example inst/examples/add_tools.R
#'
#' @seealso [answer_as_code()] [add_tools_extract_documentation()]
#'
#' @family pre_built_prompt_wraps
#' @family llm_tools
#' @family add_tools
add_tools <- function(prompt, tool_functions = list()) {
  prompt <- tidyprompt(prompt)

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

  # Convert tool_functions to named list
  tool_names <- sapply(substitute(tool_functions)[-1], deparse)
  # Ensure the names of tool_functions match the captured names
  names(tool_functions) <- tool_names

  modify_fn <- function(original_prompt_text) {
    new_prompt <- glue::glue(
      "{original_prompt_text}

        If you need more information, you can call functions to help you.
        To call a function, type:
          FUNCTION[<function name here>](<argument 1>, <argument 2>, etc...)

        The following functions are available:"
    )

    for (tool_fn_name in names(tool_functions)) {
      tool_function <- tool_functions[[tool_fn_name]]
      docs <- add_tools_extract_documentation(tool_function, tool_fn_name)

      fn_llm_text <- glue::glue("  function name: {tool_fn_name}")
      if (length(docs$description) > 0)
        fn_llm_text <- glue::glue("{fn_llm_text}\n  description: {docs$description}")
      if (length(docs$parameters) > 0) {
        fn_llm_text <- glue::glue(
          "{fn_llm_text}\n  arguments:",
          paste(
            sapply(names(docs$parameters), function(param_name) {
              param_description <- docs$parameters[[param_name]]
              glue::glue("    - {param_name}: {param_description}")
            }),
            collapse = "\n"
          )
        )
      }
      if (length(docs$return_value) > 0)
        fn_llm_text <- glue::glue("{fn_llm_text}\n  return value: {docs$return_value}")
      if (length(docs$example) > 0)
        fn_llm_text <- glue::glue("{fn_llm_text}\n  example usage: {docs$example}")

      new_prompt <- glue::glue("{new_prompt}\n\n{fn_llm_text}")
    }

    new_prompt <- glue::glue(
      "{new_prompt}

        After you call a function, wait until you receive more information."
    )

    return(new_prompt)
  }

  extraction_fn <- function(llm_response) {
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
      return(llm_feedback(glue::glue("Error: Function '{function_name}' not found.")))
    }

    # Call the tool function with the arguments and capture errors
    result <- tryCatch({
      do.call(tool_function, as.list(arguments_list))
    }, error = function(e) {
      glue::glue("Error in {print(e$call) |> capture.output()}: {e$message}")
    })

    # Create some context around the result
    argument_names <- names(formals(tool_function))
    string_of_named_arguments <-
      paste(argument_names, arguments_list, sep = " = ") |>
      paste(collapse = ", ")

    result_string <- glue::glue(
      "function called: {function_name}
      arguments used: {string_of_named_arguments}
      result: {result}"
    )

    # Return the result (or the error feedback)
    return(llm_feedback(result_string, tool_result = TRUE))
  }
  # Add environment with tool functions as an attribute to the extraction function
  environment_with_tool_functions <- new.env()
  environment_with_tool_functions$tool_functions <- tool_functions
  attr(extraction_fn, "environment") <- environment_with_tool_functions

  # # Add tool_functions as an attribute to the extraction
  # attr(extraction_fn, "tool_functions") <- tool_functions

  prompt_wrap(prompt, modify_fn, extraction_fn, type = "tool")
}



#' Extract docstring-documentation from a function
#'
#' This function parses docstring-like documentation from a function object.
#' This is used to extract information about the function's name, description,
#' parameters, return value, and example usage. 'add_tools()' uses this function
#' to provide an LLM with information about the functions it can call. For an
#' example of how such documentation within a function, see the 'example_usage'
#' vignette.
#'
#' @param func A function object which has internal, docstring-like, roxygen-like documentation,
#' with the 'llm_tool::' tags: 'name', 'description', 'param', 'return', and 'example'
#' (e.g., llm_tool::name my_function_name).
#' @param name The name of the function (optional)
#'
#' @details Note that for 'example' it must be a one-line example of how the function is used in R,
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
#'
#' @example inst/examples/add_tools.R
#'
#' @family add_tools
add_tools_extract_documentation <- function(func, name = NULL) {
  stopifnot(
    is.function(func),
    is.null(name) || (is.character(name) & length(name) == 1)
  )

  docs <- list()
  if (is.null(name))
    name <- deparse(substitute(func))

  # Convert the function to a character string
  func_text <- utils::capture.output(print(func))
  # Find documentation lines
  doc_lines <- grep("^\\s*#'", func_text, value = TRUE)

  if (length(doc_lines) > 0) {
    # Extract inline documentation from the function
    docs$description <- add_tools_extract_llm_documentation_section(doc_lines, "llm_tool::description")
    docs$parameters <- add_tools_extract_llm_documentation_section(doc_lines, "llm_tool::param")
    docs$return_value <- add_tools_extract_llm_documentation_section(doc_lines, "llm_tool::return")
    docs$example <- add_tools_extract_llm_documentation_section(doc_lines, "llm_tool::example")
  } else {
    # Docstring documentation not found,
    #   so let's try to extract from the function's help file
    docs <- add_tools_extract_helpfile_documentation(name)
  }

  # Convert example to how LLM should call it
  docs$example <- gsub("\\b(\\w+)\\(", "FUNCTION[\\1](", docs$example)

  for (name in names(docs)) {
    if (length(docs[[name]]) == 0) {
      docs[[name]] <- NULL
    }
  }

  docs
}



#' Extract a specific section from a function's docstring-like documentation block
#'
#' This is an internal helper function for add_tools_extract_documentation
#'
#' @param doc_lines A character vector of lines from a function's documentation block
#' @param section_keyword The keyword to search for in the documentation block,
#' e.g., 'llm_tool::description'
#'
#' @return The extracted section as a character string or list
#'
#' @noRd
#' @keywords internal
add_tools_extract_llm_documentation_section <- function(doc_lines, section_keyword) {
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



#' Extract documentation from a function's help file
#'
#' This function extracts documentation from a function's help file. It is used
#' as a fallback when the function does not contain docstring-like documentation.
#'
#'
#' @param name A name of a function
#'
#' @return A list with the following elements:
#' - name: The name of the function
#' - description: A description of the function
#' - parameters: A named list of parameters with descriptions
#' - return_value: A description of the return value
#' - example: An example of how the LLM should call the function. This is
#'  not extracted from the help file, but is an empty character string
#'  (because examples tend to be more complex and contain other functions,
#'  which may confuse the LLM)
#'
#' @noRd
#' @keywords internal
add_tools_extract_helpfile_documentation <- function(name) {
  # Get the package name if the function is from a package
  if (grepl("::", name)) {
    # Extract function name and package name
    func_name <- gsub(".*::", "", name)
    package_name <- gsub("::.*", "", name)
  } else {
    # Get the package name from the environment of the function
    func <- get(name, mode = "function", envir = base::globalenv())
    package_env <- environment(func)
    package_name <- environmentName(package_env)
  }

  # Get the function's help file
  help_file <- utils::help(name, package = as.character(package_name))

  if (length(help_file) == 0) {
    # No help file found
    # Argument names without description:
    args <- formals(func)
    for (x in names(args))
      args[[x]] <- ""

    return(list(
      name = name,
      description = character(0),
      parameters = args,
      return_value = character(0),
      example = character(0)
    ))
  }

  # Extract the text from the help file
  help_text <- tools:::Rd2txt(utils:::.getHelpFile(as.character(help_file))) |>
    capture.output()
  # help_text |> cat()
  # help_text

  # Function to dynamically parse help text
  parse_help_text <- function(help_text) {
    # Helper function to remove overstruck sequences
    remove_overstrike <- function(text) {
      repeat {
        # Replace overstruck sequences
        new_text <- gsub("(.)(\\x08)(.)", "\\3", text, perl = TRUE)
        if (identical(new_text, text)) break
        text <- new_text
      }
      return(text)
    }

    # Clean up and remove formatting artifacts from all lines
    clean_text <- remove_overstrike(help_text)
    clean_text <- gsub("\\\\b", "", clean_text) # Remove remaining `\b` escape sequences
    clean_text <- trimws(clean_text) # Remove leading/trailing whitespace

    # Extract the first non-empty line as the title
    title <- clean_text[nzchar(clean_text)][1]
    title_index <- which(clean_text == title)[1]
    clean_text <- clean_text[-title_index] # Remove the title line

    # Find headers dynamically by looking for lines that match header pattern
    header_pattern <- "^[[:space:]]*[[:alpha:][:space:]\\(\\)]+:\\s*$"
    header_indices <- grep(header_pattern, clean_text)
    headers <- clean_text[header_indices]
    headers <- gsub(":", "", headers) # Remove colon for easier matching
    headers <- trimws(headers)

    # Initialize result list
    parsed_result <- list(Title = title)

    # Loop through headers and extract content
    for (i in seq_along(headers)) {
      current_header <- headers[i]
      current_start <- header_indices[i]
      next_start <- ifelse(i < length(header_indices),
                           header_indices[i + 1] - 1,
                           length(clean_text))

      # Extract content for the current section
      section_content <- clean_text[(current_start + 1):next_start]
      # Remove empty lines
      section_content <- section_content[nzchar(section_content)]

      # For "Arguments" section, process differently
      if (current_header == "Arguments") {
        parsed_result[["Arguments"]] <- section_content
      } else {
        # For other sections, join the lines
        section_text <- paste(section_content, collapse = "\n")
        parsed_result[[current_header]] <- section_text
      }
    }

    # Process Arguments section to extract argument names and descriptions
    if ("Arguments" %in% names(parsed_result)) {
      args_content <- parsed_result[["Arguments"]]
      args_list <- list()
      arg_name <- NULL
      arg_desc_lines <- c()

      for (line in args_content) {
        # Check if the line is an argument name
        arg_line_pattern <- "^\\s*([a-zA-Z0-9_\\.]+):\\s*(.*)$"
        matches <- regexec(arg_line_pattern, line)
        match <- regmatches(line, matches)[[1]]

        if (length(match) >= 2 && nzchar(match[2])) {
          # Save previous argument description if exists
          if (!is.null(arg_name)) {
            arg_desc <- paste(arg_desc_lines, collapse = " ")
            arg_desc <- trimws(arg_desc)
            args_list[[arg_name]] <- arg_desc
          }
          # Start new argument
          arg_name <- match[2]
          arg_desc_lines <- if (nzchar(match[3])) match[3] else c()
        } else if (!is.null(arg_name)) {
          # Continuation of argument description
          arg_desc_lines <- c(arg_desc_lines, line)
        }
      }
      # Save the last argument description
      if (!is.null(arg_name)) {
        arg_desc <- paste(arg_desc_lines, collapse = " ")
        arg_desc <- trimws(arg_desc)
        args_list[[arg_name]] <- arg_desc
      }
      parsed_result[["Arguments"]] <- args_list
    }

    # Return parsed result
    return(parsed_result)
  }

  parsed_help <- parse_help_text(help_text)

  return(list(
    name = name,
    description = paste0(parsed_help$Title, ": ", parsed_help$Description),
    parameters = parsed_help$Arguments,
    return_value = parsed_help$Value,
    example = character(0)
  ))
}
