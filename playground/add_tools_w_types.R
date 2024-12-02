add_tools_get_documentation_from_helpfile <- function(name) {
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
      " Use add_tools_add_documentation() to add documentation to function",
      " (please note: you cannot pipe from function creation towards add_tools_add_documentation())"
    ))
  }

  # Get the function's help file
  help_file <- utils::help(name, package = as.character(package_name))
  if (length(help_file) == 0) {
    # No help file found
    # Argument names without description:
    args_formals <- formals(func)
    args <- list()
    for (arg_name in names(args_formals)) {
      default_value <- args_formals[[arg_name]]
      arg_type <- infer_type_from_default(default_value)
      args[[arg_name]] <- list(
        description = "",
        type = arg_type,
        default = get_default_value_as_string(default_value)
      )
    }

    return(list(
      name = name,
      description = character(0),
      arguments = args,
      return_value = character(0)
    ))
  }

  # Extract the text from the help file
  help_text <- tools::Rd2txt(utils:::.getHelpFile(help_file[[1]])) |>
    utils::capture.output()

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

  parsed_help <- parse_help_text(help_text)

  # Get argument types from function formals
  args_formals <- formals(func)
  arg_names <- names(args_formals)

  # Build the arguments list with descriptions, types, and default values
  args <- list()
  for (arg_name in arg_names) {
    default_value <- args_formals[[arg_name]]
    arg_type <- infer_type_from_default(default_value)
    description <- parsed_help[["Arguments"]][[arg_name]]
    if (is.null(description)) description <- ""
    default_value_str <- get_default_value_as_string(default_value)
    args[[arg_name]] <- list(
      description = description,
      type = arg_type,
      default = default_value_str
    )
  }

  return(list(
    name = name,
    description = paste0(parsed_help$Title, ": ", parsed_help$Description),
    arguments = args,
    return_value = parsed_help$Value
  ))
}

infer_type_from_default <- function(default_value) {
  if (missing(default_value) || is.null(default_value)) {
    return("character")
  } else if (is.symbol(default_value) && as.character(default_value) == "") {
    return("character")
  } else if (is.numeric(default_value)) {
    return("numeric")
  } else if (is.logical(default_value)) {
    return("logical")
  } else if (is.character(default_value)) {
    return("character")
  } else if (is.call(default_value)) {
    # It's a call, check if it's c() or list()
    func_name <- as.character(default_value[[1]])
    if (func_name == "c" || func_name == "list") {
      # Check if the vector/list is empty
      if (length(default_value) > 1) {
        # Non-empty vector/list: take the first element
        first_element <- default_value[[2]]
        return(infer_type_from_default(first_element))
      } else {
        # Empty vector/list
        return(func_name)
      }
    } else {
      return("call")
    }
  } else {
    return("unknown")
  }
}

get_default_value_as_string <- function(default_value) {
  if (missing(default_value) || is.null(default_value)) {
    return("NULL")
  }
  if (identical(default_value, quote(expr=))) {
    return(NULL)
  } else if (is.call(default_value) && (as.character(default_value[[1]]) %in% c("c", "list"))) {
    func_name <- as.character(default_value[[1]])
    if (length(default_value) > 1) {
      # Non-empty vector/list: take the first element
      first_element <- default_value[[2]]
      return(deparse(first_element))
    } else {
      # Empty vector/list: return 'c()' or 'list()'
      return(paste0(func_name, "()"))
    }
  } else {
    return(deparse(default_value))
  }
}

add_tools_get_documentation_from_helpfile("prompt_wrap")
