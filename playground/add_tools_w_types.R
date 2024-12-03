#' Add tidyprompt function documentation to a function
#'
#' This function adds documentation to a custom function. This documentation
#' is used to extract information about the function's name, description, arguments,
#' and return value. This information is used to provide an LLM with information
#' about the functions, so that the LLM can call R functions. The intended
#' use of this function is to add documentation to custom functions that do not
#' have help files; [add_tools_get_documentation()] will extract the documentation
#' from help files when available.
#'
#' @details If the function already has documentation, it will be overwritten
#' by the documentation provided in this function (in terms of extraction
#' by [add_tools_get_documentation()]). Thus, it is possible to override
#' the help file documentation by adding custom documentation
#'
#' @param func A function object
#' @param description A description of the function and its purpose.
#' For native function calling (e.g., with OpenAI), this will be added to the
#' description which is added to the system prompt. For text-based function
#' calling, this will be added to prompt which introduces the function
#' @param arguments A named list of arguments (arguments). Each argument
#' should be a list, which may contain:
#' \itemize{
#'  \item '$description': A description of the argument and its purpose.
#'  Not required for native function calling (e.g., with OpenAI), but
#'  recommended for text-based function calling
#'  \item '$type': The type of the argument. This should be one conforming
#'  to a JSON schema type (e.g., "string", "number", "boolean", "object", "array").
#'  It may also be set to 'match.arg', equivalent to R functions having a
#'  vector of possible values (e.g., c("yes", "no")) on which 'match.arg()'
#'  will be called. The possible values should then be passed as a vector
#'  under 'default_value'
#'  \item '$default_value': The default value of the argument. This is only
#'  required when 'type' is set to 'match.arg'. It should be a vector of
#'  possible values for the argument
#' }
#' @param return_value A description what the function returns or
#' of its possible side-effects. Not required for native function calling
#' (e.g., with OpenAI), but recommended for text-based function calling.
#' For native function-calling, this will be added to the description
#' which is added to the system prompt
#' @param name The name of the function (optional). If not provided, the function
#' name will be extracted from the function object. Use this parameter
#' to override the function name if necessary
#'
#' @return The function object with the documentation added as an attribute
#'
#' @export
#'
#' @example inst/examples/add_tools.R
tools_add_docs <- function(
    func,
    description,
    arguments = list(),
    return_value,
    name = NULL
) {
  stopifnot(
    is.function(func),
    (is.null(name) | is.character(name) & length(name) == 1),
    is.character(description) & length(description) == 1,
    is.list(arguments),
    length(arguments == 0) | !is.null(names(arguments)),
    is.null(return_value) | is.character(return_value) & length(return_value) == 1
  )

  if (is.null(name))
    name <- deparse(substitute(func))

  attr(func, "tidyprompt_fn_docs") <- list(
    name = name,
    description = description,
    arguments = arguments,
    return_value = return_value
  )

  return(func)
}



#' Extract documentation from a function
#'
#' This function extracts documentation from a help file (if available,
#' i.e., when the function is part of a package) or from documentation added
#' by [add_tools_add_documentation()]. The extracted documentation includes
#' the function's name, description, arguments, and return value.
#' This information is used to provide an LLM with information about the functions,
#' so that the LLM can call R functions.
#'
#' @details This function will prioritize documentation added by
#' [add_tools_add_documentation()] over documentation from a help file.
#' Thus, it is possible to override the help file documentation by adding
#' custom documentation
#'
#' @param func A function object. The function should belong to a package
#' and have documentation available in a help file, or it should
#' have documentation added by [add_tools_add_documentation()]
#' @param name The name of the function if already known (optional).
#' If not provided it will be extracted from the documentation or the
#' function object's name
#'
#' @return A list with the following elements:
#'  - name: The name of the function
#'  - description: A description of the function
#'  - arguments: A named list of arguments with descriptions
#'  - return_value: A description of the return value
#'
#' @export
#'
#' @example inst/examples/add_tools.R
#'
#' @family add_tools
tools_get_docs <- function(func, name = NULL) {
  stopifnot(
    is.function(func),
    is.null(name) || (is.character(name) & length(name) == 1)
  )

  docs <- list()
  if (is.null(name))
    name <- deparse(substitute(func))

  if (!is.null(attr(func, "tidyprompt_fn_docs"))) {
    docs <- attr(func, "tidyprompt_fn_docs")

    stopifnot(
      is.list(docs),
      !is.null(names(docs)),
      all(c("name", "description", "arguments") %in% names(docs)),
      is.character(docs$name) & length(docs$name) == 1,
      is.character(docs$description) & length(docs$description) == 1,
      is.list(docs$arguments),
      !is.null(names(docs$arguments)),
      is.null(docs$return_value) || (is.character(docs$return_value) & length(docs$return_value) == 1)
    )
  } else {
    docs <- tools_generate_docs(func, name)
  }

  remove_empty <- function(x) {
    if (is.list(x)) {
      # Recursively apply to each element
      x <- lapply(x, remove_empty)
      # Remove elements with zero length
      x <- x[lengths(x) > 0]
    }
    return(x)
  }

  remove_empty(docs)
}

# Attempts to generate docs using formals & helpfile (if available)
tools_generate_docs <- function(name) {
  # Get the package name and function
  if (grepl("::", name)) {
    # Extract function name and package name
    func_name <- gsub(".*::", "", name)
    package_name <- gsub("::.*", "", name)
    func <- getExportedValue(package_name, func_name)
  } else {
    # Get the function and its package
    func <- get(name, mode = "function")
    package_env <- environment(func)
    package_name <- environmentName(package_env)
  }

  if (package_name == "R_GlobalEnv") {
    stop(paste0(
      "No documentation found for function in global environment.",
      " Use `add_tools_add_documentation()` to add documentation to function",
      " (please note: you cannot pipe from function creation towards `add_tools_add_documentation()`)"
    ))
  }

  # Get arguments, defaults, and likely types based on formals
  args <- get_args_defaults_types(func)

  # Get the function's help file
  help_file <- utils::help(name, package = as.character(package_name))

  if (length(help_file) == 0) { # No help file found
    # Return without function description, arg descriptions, or return description
    return(list(
      name = name,
      arguments = args,
    ))
  }

  # Extract the text from the help file
  help_text <- tools::Rd2txt(utils:::.getHelpFile(help_file[[1]])) |>
    utils::capture.output()

  # Parse the help text
  parsed_help <- parse_help_text(help_text)

  # Add descriptions to arguments
  for (arg_name in names(args)) {
    description <- parsed_help[["Arguments"]][[arg_name]]
    if (!is.null(description)) {
      args[[arg_name]]$description <- description
    }
  }

  # Return list with function name, description, arguments, and return value
  return(list(
    name = name,
    description = paste0(parsed_help$Title, ": ", parsed_help$Description),
    arguments = args,
    return_value = parsed_help$Value
  ))
}

# Get default argument & types from function formals
get_args_defaults_types <- function(func) {
  args_formals <- formals(func)
  arg_names <- names(args_formals)

  arguments <- list()
  for (arg_name in arg_names) {
    default_value <- args_formals[[arg_name]]
    if (missing(default_value)) {
      # If missing default value, "default_value" will not be added to list
      # Note: this is different from adding 'NULL' as "default_value"
      #   (In that case, the default value is NULL and not missing)
      arguments[[arg_name]] <- list(
        type = "unknown"
      )
      next
    }

    arguments[[arg_name]] <- list(
      default_value = default_value,
      type = infer_type_from_default(default_value)
    )
  }

  return(arguments)
}

infer_type_from_default <- function(default_value) {
  if (is.numeric(default_value)) {
    return("numeric")
  } else if (is.logical(default_value)) {
    return("logical")
  } else if (is.character(default_value)) {
    return("character")
  } else if (is.call(default_value)) {
    func_name <- as.character(default_value[[1]])
    if (func_name == "c") {
      if (length(default_value) > 1) { # Assuming it a match.arg-type vector
        return("match.arg")
      } else {
        return("vector")
      }
    } else if (func_name == "list") {
        return("list")
    } else {
      return("call")
    }
  } else {
    return("unknown")
  }
}

# Function to dynamically parse help text from help file
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
  clean_text <- gsub("\\\\b", "", clean_text) # Remove remaining \b escape sequences
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

fn_meta <- add_tools_get_documentation_from_helpfile("prompt_wrap")

convert_fn_meta_to_r_json_schema <- function(fn_meta) {
  # Initialize the schema
  schema <- list(
    type = "function",
    "function" = list(
      name = fn_meta$name,
      # description = fn_meta$description,
      parameters = list(
        type = "object",
        properties = list(),
        required = character(0),
        additionalProperties = FALSE
      )
    )
  )

  # Process arguments
  args <- fn_meta$arguments
  properties <- list()
  required_args <- character(0)

  for (arg_name in names(args)) {
    arg <- args[[arg_name]]

    # Initialize property
    prop <- list(
      # description = arg$description
    )

    # Map R types to JSON types
    r_type <- arg$type
    if (is.null(r_type) || r_type == "unknown") {
      # Default to "string" if type is unknown
      prop$type <- "string"
    } else if (r_type == "character") {
      prop$type <- "string"
    } else if (r_type == "integer") {
      prop$type <- "integer"
    } else if (r_type == "numeric") {
      prop$type <- "number"
    } else if (r_type == "logical") {
      prop$type <- "boolean"
    } else if (r_type == "match.arg") {
      prop$type <- "string"
      prop$enum <- arg$default_value
    }
    else if (r_type %in% c("list", "vector")) {
      prop$type <- "array"
    } else if (r_type == "call") {
      prop$type <- "object"
    } else {
      prop$type <- "string"
    }

    # Add to required arguments if there's no default value
    if (!"default_value" %in% names(arg)) {
      required_args <- c(required_args, arg_name)
    }

    # Add property to properties list
    properties[[arg_name]] <- prop
  }

  # Set properties and required fields
  schema[["function"]]$parameters$properties <- properties
  if (length(required_args) > 0) {
    schema[["function"]]$parameters$required <- required_args
  }

  return(schema)
}
