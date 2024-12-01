#' Enable R function calling for prompt evaluation by a LLM
#'
#' This function adds the ability for the a LLM to call R functions.
#' Users can specify a list of functions that the LLM can call, and the
#' prompt will be modified to include information, as well as an
#' accompanying extraction function to call the functions (handled by
#' [send_prompt()]). Documentation for the functions is extracted from
#' the help file (if available), or from documentation added by
#' [add_tools_add_documentation()].
#'
#' @details Note that this method of function calling is purely text-based.
#' This makes it suitable for any LLM and any LLM provider. However,
#' 'native' function calling (where the LLM model provider restricts the
#' model to special tokens that can be used to call functions) may perform
#' better in terms of accuracy and efficiency. 'tidyprompt' may support
#' 'native' function calling in the future
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param tool_functions An R function or a list of R functions that the LLM can call.
#' If the function has been documented in a help file (e.g., because it is part of a
#' package), the documentation will be parsed from the help file. If it is a custom
#' function, documentation should be added with [add_tools_add_documentation()]
#'
#' @return A [tidyprompt()] with an added [prompt_wrap()] which
#' will allow the LLM to call R functions
#'
#' @export
#'
#' @example inst/examples/add_tools.R
#'
#' @seealso [answer_as_code()] [add_tools_get_documentation()]
#'
#' @family pre_built_prompt_wraps
#' @family llm_tools
#' @family add_tools
add_tools <- function(prompt, tool_functions = list()) {
  prompt <- tidyprompt(prompt)

  # Check if tool_functions is single function, if so, convert to list
  if (length(tool_functions) == 1 && is.function(tool_functions)) {
    name <- deparse(substitute(tool_functions))
    tool_functions <- list(tool_functions)
    names(tool_functions) <- name
  } else {
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
  }

  modify_fn <- function(original_prompt_text) {
    new_prompt <- glue::glue(
      "{original_prompt_text}

    If you need more information, you can call functions to help you.
    To call a function, output a JSON object with the following format:

    {{
      \"function\": \"<function name>\",
      \"arguments\": {{
        \"<argument_name>\": <argument_value>,
        ...
      }}
    }}

    (Note: you cannot call other functions within arguments.)

    The following functions are available:"
    )

    for (tool_fn_name in names(tool_functions)) {
      tool_function <- tool_functions[[tool_fn_name]]
      docs <- add_tools_get_documentation(tool_function, tool_fn_name)

      fn_llm_text <- glue::glue(
        "  function name: {tool_fn_name}", .trim = FALSE
      )
      if (length(docs$description) > 0)
        fn_llm_text <- glue::glue(
          "{fn_llm_text}\n  description: {docs$description}", .trim = FALSE
        )
      if (length(docs$arguments) > 0) {
        fn_llm_text <- glue::glue(
          "{fn_llm_text}\n  arguments:\n",
          paste(
            sapply(names(docs$arguments), function(param_name) {
              param_description <- docs$arguments[[param_name]]
              glue::glue("    - {param_name}: {param_description}", .trim = FALSE)
            }),
            collapse = "\n"
          ),
          .trim = FALSE
        )
      }
      if (length(docs$return_value) > 0)
        fn_llm_text <- glue::glue(
          "{fn_llm_text}\n  return value: {docs$return_value}", .trim = FALSE
        )

      new_prompt <- glue::glue(
        "{new_prompt}\n\n{fn_llm_text}", .trim = FALSE
      )
    }

    new_prompt <- glue::glue(
      "{new_prompt}\n\n",
      "After you call a function, wait until you receive more information.\n",
      "Use the information to decide your next steps or provide a final response.",
      .trim = FALSE
    )

    return(new_prompt)
  }

  extraction_fn <- function(llm_response) {
    jsons <- extraction_fn_json(llm_response)

    fn_results <- lapply(jsons, function(json) {
      if (is.null(json[["function"]]))
        return(NULL)

      if (json[["function"]] %in% names(tool_functions)) {
        tool_function <- tool_functions[[json[["function"]]]]
        arguments <- json[["arguments"]]

        result <- tryCatch({
          do.call(tool_function, arguments)
        }, error = function(e) {
          glue::glue("Error in {e$message}")
        })

        if (length(result) > 0)
          result <- paste(result, collapse = ", ")

        # Create some context around the result
        string_of_named_arguments <-
          paste(names(arguments), arguments, sep = " = ") |>
          paste(collapse = ", ")

        result_string <- glue::glue(
          "function called: {json[[\"function\"]]}
          arguments used: {string_of_named_arguments}
          result: {result}"
        )

        return(result_string)
      } else {
        return(glue::glue("Error: Function '{json[[\"function\"]]}' not found."))
      }
    })
    fn_results <- fn_results[!sapply(fn_results, is.null)]

    if (length(fn_results) == 0)
      return(llm_response)

    return(llm_feedback(
      paste(fn_results, collapse = "\n\n"),
      tool_result = TRUE
    ))
  }

  # Add environment with tool functions as an attribute to the extraction function
  environment_with_tool_functions <- new.env()
  environment_with_tool_functions$tool_functions <- tool_functions
  attr(extraction_fn, "environment") <- environment_with_tool_functions

  prompt_wrap(prompt, modify_fn, extraction_fn, type = "tool")
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
add_tools_get_documentation <- function(func, name = NULL) {
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
      all(c("name", "description", "arguments", "return_value") %in% names(docs)),
      is.character(docs$name) & length(docs$name) == 1,
      is.character(docs$description) & length(docs$description) == 1,
      is.list(docs$arguments),
      !is.null(names(docs$arguments)),
      is.character(docs$return_value) & length(docs$return_value) == 1
    )
  } else {
    docs <- add_tools_get_documentation_from_helpfile(name)
  }

  for (name in names(docs)) {
    if (length(docs[[name]]) == 0) {
      docs[[name]] <- NULL
    }
  }

  docs
}

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
#' @param description A description of the function
#' @param arguments A named list of arguments (arguments) with descriptions
#' @param return_value A description of the return value
#' @param name The name of the function (optional). If not provided, the function
#' name will be extracted from the function object
#'
#' @return The function object with the documentation added as an attribute
#'
#' @export
#'
#' @example inst/examples/add_tools.R
add_tools_add_documentation <- function(
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
    !is.null(names(arguments)),
    is.character(return_value) & length(return_value) == 1
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



#' Extract documentation from a function's help file
#'
#' This function extracts documentation from a function's help file. It is used
#' as a fallback when the function does not contain documentation
#' as added by [add_tools_add_documentation()].
#'
#' @param name (string) A name of a function (e.g., 'list.files')
#'
#' @return A list with the following elements:
#' - name: The name of the function
#' - description: A description of the function
#' - arguments: A named list of arguments with descriptions
#' - return_value: A description of the return value
#'
#' @noRd
#' @keywords internal
add_tools_get_documentation_from_helpfile <- function(name) {
  # Get the package name if the function is from a package
  if (grepl("::", name)) {
    # Extract function name and package name
    func_name <- gsub(".*::", "", name)
    package_name <- gsub("::.*", "", name)
  } else {
    # Get the package name from the environment of the function
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
      arguments = args,
      return_value = character(0)
    ))
  }

  # Extract the text from the help file
  help_text <- tools::Rd2txt(get_help_file(as.character(help_file))) |>
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
    arguments = parsed_help$Arguments,
    return_value = parsed_help$Value
  ))
}



#' Extract help file function
#'
#' @param file String containing name of file
#'
#' @noRd
#' @keywords internal
#'
#' @source Function adapted from the CRAN package
#' \href{https://cran.r-project.org/web/packages/devtoolbox/index.html}{'devtoolbox'},
#' which based this function on the 'utils:::.getHelpFile' function
get_help_file <- function(file){
  path <- dirname(file)
  dirpath <- dirname(path)

  if (!file.exists(dirpath))
    stop(
      gettextf("invalid %s argument", sQuote("file")),
      domain = NA
    )

  pkgname <- basename(dirpath)
  RdDB <- file.path(path, pkgname)

  if (!file.exists(paste0(RdDB, ".rdx")))
    stop(
      gettextf(
        "package %s exists but was not installed under R >= 2.10.0 so help cannot be accessed",
        sQuote(pkgname)
      ),
      domain = NA
    )

  # internal function from tools
  fetchRdDB <- function(filebase, key = NULL) {
    fun <- function(db) {
      vals <- db$vals
      vars <- db$vars
      datafile <- db$datafile
      compressed <- db$compressed
      envhook <- db$envhook

      fetch <- function(key){
        lazyLoadDBfetch(vals[key][[1L]], datafile, compressed, envhook)
      }

      if (length(key)) {
        if (!key %in% vars)
          stop(
            gettextf(
              "No help on %s found in RdDB %s", sQuote(key), sQuote(filebase)
            ),
            domain = NA
          )

        fetch(key)
      }
      else {
        res <- lapply(vars, fetch)
        names(res) <- vars
        res
      }
    }

    res <- lazyLoadDBexec(filebase, fun)

    if (length(key))
      res
    else invisible(res)
  }

  fetchRdDB(RdDB, basename(file))
}
