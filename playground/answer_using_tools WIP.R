#' Enable R function calling for prompt evaluation by a LLM
#'
#' This function adds the ability for the a LLM to call R functions.
#' Users can specify a list of functions that the LLM can call, and the
#' prompt will be modified to include information, as well as an
#' accompanying extraction function to call the functions (handled by
#' [send_prompt()]). Documentation for the functions is extracted from
#' the help file (if available), or from documentation added by
#' [tools_add_docs()]
#'
#' @details Note that this method of function calling is purely text-based.
#' This makes it suitable for any LLM and any LLM provider. However,
#' 'native' function calling (where the LLM model provider restricts the
#' model to special tokens that can be used to call functions) may perform
#' better in terms of accuracy and efficiency. 'tidyprompt' may support
#' 'native' function calling in the future
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param tools An R function or a list of R functions that the LLM can call.
#' If the function has been documented in a help file (e.g., because it is part of a
#' package), the documentation will be parsed from the help file. If it is a custom
#' function, documentation should be added with [tools_add_docs()]
#'
#' @return A [tidyprompt()] with an added [prompt_wrap()] which
#' will allow the LLM to call R functions
#'
#' @export
#'
#' @example inst/examples/add_tools.R
#'
#' @seealso [answer_as_code()] [tools_get_docs()]
#'
#' @family pre_built_prompt_wraps
#' @family llm_tools
#' @family add_tools
answer_using_tools <- function(
  prompt,
  tools = list(),
  type = c("auto", "openai", "text-based") # TODO: implement Ollama based on openai
) {
  prompt <- tidyprompt(prompt)
  type <- match.arg(type)

  # Check if tools is single function, if so, convert to list
  if (length(tools) == 1 && is.function(tools)) {
    name <- deparse(substitute(tools))
    tools <- list(tools)
    names(tools) <- name
  } else {
    stopifnot(
      is.list(tools),
      length(tools) > 0,
      all(sapply(tools, is.function))
    )
    # Convert tools to named list
    tool_names <- sapply(substitute(tools)[-1], deparse)
    # Ensure the names of tools match the captured names
    names(tools) <- tool_names
  }

  determine_type <- function(llm_provider = NULL) {
    if (type != "auto") return(type)
    if (isTRUE(llm_provider$api_type == "openai")) return("openai")
    return("text-based")
  }

  parameter_fn <- function(llm_provider) {
    type <- determine_type(llm_provider)

    if (type == "openai") {
      tools_openai <- list()

      for (tool_name in names(tools)) {
        tool_openai <- list()
        tool <- tools[[tool_name]]
        docs <- tools_get_docs(tool, tool_name)

        tool_openai$type <- "function"
        tool_openai[["function"]]$name <- tool_name
        if (!is.null(docs$description)) {
          tool_openai[["function"]]$description <- docs$description
        }
        tool_openai[["function"]]$parameters <-
          tools_docs_to_r_json_schema(docs)
        tool_openai[["function"]]$strict <- TRUE

        tools_openai[[length(tools_openai) + 1]] <- tool_openai
      }

      return(list(tools = tools_openai))
    }

    return(list())
  }

  handler_fn <- function(response, llm_provider) {
    type <- determine_type(llm_provider)
    if (type != "openai") return(response)

    while (TRUE) {
      body <- response$http$response$body

      if ("tool_calls" %in% names(body)) {
        tool_calls <- body$tool_calls
      } else {
        body <- tryCatch(
          body |> rawToChar() |> jsonlite::fromJSON(),
          error = function(e) {
            NULL
          }
        )

        if (is.null(body)) break
        if (length(body$choices$message$tool_calls) == 0) break

        tool_calls <- list()
        for (tool_call in body$choices$message$tool_calls) {
          tool_calls[[length(tool_calls) + 1]] <- list(
            id = tool_call$id,
            type = tool_call$type,
            "function" = as.list(tool_call[["function"]])
          )
        }
      }

      if (length(tool_calls) == 0) break

      chat_history <- response$completed
      messages = lapply(seq_len(nrow(chat_history)), function(i) {
        list(role = chat_history$role[i], content = chat_history$content[i])
      })
      messages[[length(messages) + 1]] <- list(
        role = "assistant",
        tool_calls = tool_calls
      )

      for (tool_call in tool_calls) {
        tool_name <- tool_call[["function"]]$name
        tool <- tools[[tool_name]]
        arguments <- tool_call[["function"]]$arguments |> jsonlite::fromJSON()

        chat_history <- chat_history |>
          dplyr::bind_rows(
            data.frame(
              role = "assistant",
              content = paste0(
                "Calling function '",
                tool_name,
                "' with arguments:\n",
                jsonlite::toJSON(arguments, auto_unbox = TRUE, pretty = TRUE)
              )
            )
          )

        result <- tryCatch(
          {
            do.call(tool, arguments)
          },
          error = function(e) {
            glue::glue("Error: {e$message}")
          }
        )

        if (length(result) > 0) result <- paste(result, collapse = ", ")

        result <- as.character(result)

        messages[[length(messages) + 1]] <- list(
          role = "tool",
          content = result,
          tool_call_id = tool_call$id
        )
      }

      new_request <- response$http$request
      new_request$body$data$messages <- messages

      response <- request_llm_provider(
        chat_history,
        new_request,
        llm_provider$parameters$stream,
        api_type = llm_provider$api_type
      )
    }

    return(response)
  }

  modify_fn <- function(original_prompt_text, llm_provider) {
    type <- determine_type(llm_provider)
    if (type == "openai") return(original_prompt_text)

    new_prompt <- glue::glue(
      "{original_prompt_text}\n\n",
      "If you need more information, you can call functions to help you."
    )

    new_prompt <- glue::glue(
      "{new_prompt}\n\n",
      "To call a function, output a JSON object with the following format:\n",
      "  {{\n",
      "    \"function\": \"<function name>\",\n",
      "    \"arguments\": {{\n",
      "      \"<argument_name>\": <argument_value>,\n",
      "      # ...\n",
      "    }}\n",
      "  }}\n",
      "  (Note: you may not provide function calls as function arguments.)"
    )

    new_prompt <- glue::glue(
      "{new_prompt}\n\n",
      "The following functions are available:"
    )

    for (tool_name in names(tools)) {
      tool <- tools[[tool_name]]
      docs <- tools_get_docs(tool, tool_name)

      with_arguments <- TRUE
      if (type == "openai") with_arguments <- FALSE

      tool_llm_text <- tools_docs_to_text(docs, with_arguments)

      new_prompt <- glue::glue(
        "{new_prompt}\n\n{tool_llm_text}",
        .trim = FALSE
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

  extraction_fn <- function(llm_response, llm_provider) {
    type <- determine_type(llm_provider)
    if (type == "openai") return(llm_response)

    jsons <- extraction_fn_json(llm_response)

    fn_results <- lapply(jsons, function(json) {
      if (is.null(json[["function"]])) return(NULL)

      if (json[["function"]] %in% names(tools)) {
        tool_function <- tools[[json[["function"]]]]
        arguments <- json[["arguments"]]

        result <- tryCatch(
          {
            do.call(tool_function, arguments)
          },
          error = function(e) {
            glue::glue("Error in {e$message}")
          }
        )

        if (length(result) > 0) result <- paste(result, collapse = ", ")

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
        return(
          glue::glue("Error: Function '{json[[\"function\"]]}' not found.")
        )
      }
    })
    fn_results <- fn_results[!sapply(fn_results, is.null)]

    if (length(fn_results) == 0) return(llm_response)

    return(
      llm_feedback(
        paste(fn_results, collapse = "\n\n"),
        tool_result = TRUE
      )
    )
  }

  # Add environment with tool functions as an attribute to the extraction function
  environment_with_tools <- new.env()
  environment_with_tools$tools <- tools
  attr(extraction_fn, "environment") <- environment_with_tools
  attr(handler_fn, "environment") <- environment_with_tools

  prompt_wrap(
    prompt,
    modify_fn,
    extraction_fn,
    NULL,
    handler_fn,
    parameter_fn,
    type = "tool",
    name = "answer_using_tools"
  )
}

#' Add tidyprompt function documentation to a function
#'
#' @description This function adds documentation to a custom function. This documentation
#' is used to extract information about the function's name, description, arguments,
#' and return value. This information is used to provide an LLM with information
#' about the functions, so that the LLM can call R functions. The intended
#' use of this function is to add documentation to custom functions that do not
#' have help files; [tools_get_docs()] may generate documentation from a
#' help file when the function is part of base R or a package.
#'
#' If a function already has documentation, the documentation added by this
#' function may overwrite it. If you wish to modify existing documentation,
#' you may make a call to [tools_get_docs()] to extract the existing documentation,
#' modify it, and then call [tools_add_docs()] to add the modified documentation.
#'
#' @param func A function object
#' @param docs A list with the following elements:
#' \itemize{
#' \item 'name': (optional) The name of the function. If not provided, the function
#' name will be extracted from the function object. Use this parameter to override
#' the function name if necessary
#' \item 'description': A description of the function and its purpose
#' \item 'arguments': A named list of arguments with descriptions. Each argument is a list
#' which may contain: \itemize{
#' \item 'description': A description of the argument and its purpose. Not
#' required or used for native function calling (e.g., with OpenAI), but recommended
#' for text-based function calling
#' \item 'type': The type of the argument. This should be one of:
#' 'integer', 'numeric', 'logical', 'string', 'match.arg',
#' 'vector integer', 'vector numeric', 'vector logical', 'vector string'.
#'  For arguments which are named lists, 'type' should be a named list
#'  which contains the types of the elements. For type 'match.arg', the
#'  possible values should be passed as a vector under 'default_value'.
#'  'type' is required for native function calling (with, e.g., OpenAI) but may
#'  also be useful to provide for text-based function calling, in which it will
#'  be added to the prompt introducing the function
#' \item 'default_value': The default value of the argument. This is only required
#' when 'type' is set to 'match.arg'. It should then be a vector of possible values
#' for the argument. In other cases, it is not required; for native function calling,
#' it is not used in other cases; for text-based function calling, it may be useful
#' to provide the default value, which will be added to the prompt introducing the
#' function
#' }
#' \item 'return': A list with the following elements:
#' \itemize{
#' \item 'description': A description of the return value or the side effects of the function
#' }
#' }
#' @return The function object with the documentation added as an attribute
#' ('tidyprompt_tool_docs')
#'
#' @export
#'
#' @example inst/examples/add_tools.R
tools_add_docs <- function(
  func,
  docs
) {
  stopifnot(
    is.function(func),
    is.list(docs),
    length(docs) > 0,
    !is.null(names(docs)),
    (is.null(docs$name) | is.character(docs$name) & length(docs$name) == 1),
    is.character(docs$description) & length(docs$description) == 1,
    is.list(docs$arguments),
    length(docs$arguments) == 0 | !is.null(names(docs$arguments)),
    is.null(docs$return$description) |
      is.character(docs$return$description) &
        length(docs$return$description) == 1
  )

  if (is.null(docs$name)) docs$name <- deparse(substitute(func))

  attr(func, "tidyprompt_tool_docs") <- docs

  return(func)
}

#' Extract documentation from a function
#'
#' This function extracts documentation from a help file (if available,
#' i.e., when the function is part of a package) or from documentation added
#' by [tools_add_docs()]. The extracted documentation includes
#' the function's name, description, arguments, and return value.
#' This information is used to provide an LLM with information about the functions,
#' so that the LLM can call R functions.
#'
#' @details This function will prioritize documentation added by
#' [tools_add_docs()] over documentation from a help file.
#' Thus, it is possible to override the help file documentation by adding
#' custom documentation
#'
#' @param func A function object. The function should belong to a package
#' and have documentation available in a help file, or it should
#' have documentation added by [tools_add_docs()]
#' @param name The name of the function if already known (optional).
#' If not provided it will be extracted from the documentation or the
#' function object's name
#'
#' @return A list with documentation for the function. See [tools_add_docs()]
#' for more information on the contents
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
  if (is.null(name)) name <- deparse(substitute(func))

  if (!is.null(attr(func, "tidyprompt_tool_docs"))) {
    docs <- attr(func, "tidyprompt_tool_docs")

    stopifnot(
      is.list(docs),
      !is.null(names(docs)),
      all(c("name", "description", "arguments") %in% names(docs)),
      is.character(docs$name) & length(docs$name) == 1,
      is.character(docs$description) & length(docs$description) == 1,
      is.list(docs$arguments),
      !is.null(names(docs$arguments)),
      is.null(docs$return$description) ||
        (
          is.character(docs$return$description) &
            length(docs$return$description) == 1
        )
    )
  } else {
    docs <- tools_generate_docs(name)
  }

  # Check that all formal arguments have documentation
  if (!all(names(formals(func)) %in% names(docs$arguments))) {
    # If not, modify args with what is known from formals
    args <- docs$arguments
    true_args <- gd_get_args_defaults_types(func)

    # Add true_args to args only where not present
    for (arg_name in names(true_args)) {
      if (!arg_name %in% names(args)) {
        args[[arg_name]] <- true_args[[arg_name]]
      }
    }

    # Add type, default value where not present
    for (arg_name in names(args)) {
      if (!"type" %in% names(args[[arg_name]])) {
        args[[arg_name]]$type <- true_args[[arg_name]]$type
      }
      if (!"default_value" %in% names(args[[arg_name]])) {
        args[[arg_name]]$default_value <- true_args[[arg_name]]$default_value
      }
    }

    docs$arguments <- args
  }

  # Remove args not present in formals
  for (arg_name in names(docs$arguments)) {
    if (!arg_name %in% names(formals(func))) {
      docs$arguments[[arg_name]] <- NULL
      warning(
        paste0(
          "Argument '",
          arg_name,
          "' not found in function formals. Removing from documentation"
        )
      )
    }
  }

  docs
}

#' Generate function documentation from formals and help file
#'
#' This function generates documentation for a function based on its formals
#' and help file (if available). The documentation includes the function's
#' name, description, arguments, and return value. This function is called internally
#' when there is no documentation.#'
#'
#' @param name (string) A name of a function (e.g., 'list.files')
#'
#' @return A list with documentation for the function
#'
#' @noRd
#' @keywords internal
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

  # Get arguments, defaults, and likely types based on formals
  args <- gd_get_args_defaults_types(func)

  if (package_name == "R_GlobalEnv") {
    help_file <- NULL
  } else {
    # Get the function's help file
    help_file <- utils::help(name, package = as.character(package_name))
  }
  if (length(help_file) == 0) {
    # No help file found
    # Return without function description, arg descriptions, or return description
    return(
      list(
        name = name,
        arguments = args
      )
    )
  }

  # Extract the text from the help file
  help_text <- tools::Rd2txt(utils:::.getHelpFile(help_file[[1]])) |>
    utils::capture.output()

  # Parse the help text
  parsed_help <- gd_parse_help_text(help_text)

  # Add descriptions to arguments
  for (arg_name in names(args)) {
    description <- parsed_help[["Arguments"]][[arg_name]]
    if (!is.null(description)) {
      args[[arg_name]]$description <- description
    }
  }

  # Return list with function name, description, arguments, and return value
  return(
    list(
      name = name,
      description = paste0(parsed_help$Title, ": ", parsed_help$Description),
      arguments = args,
      return = list(description = parsed_help$Value)
    )
  )
}

#' Get argument names, default values, and types from a function's formals
#'
#' This function extracts the argument names, default values, and types from
#' a function's formals. The types are inferred from the default values.
#' Is called internally to generate function documentation when there is none.
#'
#' @param func A function object
#'
#' @return A named list of arguments, where each argument is a list containing:
#' - 'default_value': The default value of the argument. This is not
#'  included if there is no default value. Note that when the default value
#'  is included and is NULL, the default value is NULL (and not missing)
#'  - 'type': see 'gd_infer_type_from_default()'. If there is no default value,
#'  the type is set to 'unknown'
#'  @noRd
#'  @keywords internal
gd_get_args_defaults_types <- function(func) {
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
      type = gd_infer_type_from_default(default_value)
    )
  }

  return(arguments)
}

#' Infer the type of an argument from its default value
#'
#' This function infers the type of an argument from its default value.
#'
#' @param default_value The default value of the argument
#'
#' @return A string indicating the type of the argument. The possible types are:
#' - 'integer': The argument is an integer value
#' - 'numeric': The argument is a numeric value
#' - 'logical': The argument is a logical value
#' - 'string': The argument is a character value
#' - 'match.arg': The argument is a match.arg-type argument
#' - 'vector [integer/numeric/logical/string/unknown] ': The argument is a vector
#'  of the specified type
#' - 'list': The argument is a list
#' - 'call': The argument is a call to a function
#' - 'unknown': The type of the argument is unknown
#'
#' @noRd
#' @keywords internal
gd_infer_type_from_default <- function(default_value) {
  is_whole_number <- function(x)
    tryCatch(
      x %% 1 == 0,
      error = function(e) FALSE
    )

  infer_list_types <- function(lst) {
    # Recursively infer types for each element in a named list
    sapply(lst, gd_infer_type_from_default, simplify = FALSE)
  }

  if (is.null(default_value)) {
    return("unknown")
  }
  if (is.numeric(default_value) & length(default_value) == 1) {
    if (is_whole_number(default_value)) {
      return("integer")
    }
    return("numeric")
  } else if (is.logical(default_value) & length(default_value) == 1) {
    return("logical")
  } else if (is.character(default_value) & length(default_value) == 1) {
    return("string")
  } else if (is.call(default_value)) {
    func_name <- as.character(default_value[[1]])
    if (
      func_name == "c" ||
        (func_name == "list" && is.null(names(default_value[-1])))
    ) {
      # Treat list like c only if it has no names
      values <- default_value[-1]

      if (length(values) == 0) return("vector unknown")

      if (length(values) > 1 && all(sapply(values, is.character))) {
        # Assuming it's a match.arg-type vector
        return("match.arg")
      } else {
        if (all(sapply(values, is.numeric))) {
          if (all(sapply(values, is_whole_number))) {
            return("vector integer")
          }
          return("vector numeric")
        } else if (all(sapply(values, is.logical))) {
          return("vector logical")
        } else if (all(sapply(values, is.character))) {
          return("vector string")
        }

        return("vector unknown")
      }
    } else if (func_name == "list") {
      # Handle named lists
      if (!is.null(names(default_value[-1]))) {
        return(infer_list_types(as.list(default_value[-1])))
      } else {
        return("list")
      }
    } else {
      return("call")
    }
  } else if (is.list(default_value)) {
    if (!is.null(names(default_value))) {
      # Named list
      return(infer_list_types(default_value))
    } else {
      # Unnamed list
      return("list")
    }
  } else {
    return("unknown")
  }
}

#' Function to parse help text from a function's help file
#'
#' @param help_text The text extracted from a function's help file
#'
#' @return A list with the following elements:
#' - 'Title': The title of the help file
#' - 'Description': The description of the function
#' - 'Arguments': A named list of arguments with descriptions
#' - 'Value': The return value or side effects of the function
#'
#' @noRd
#' @keywords internal
gd_parse_help_text <- function(help_text) {
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
    next_start <- ifelse(
      i < length(header_indices),
      header_indices[i + 1] - 1,
      length(clean_text)
    )

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

#' Convert function documentation to a JSON schema
#'
#' This function converts function documentation to an R list that represents
#' a JSON schema. The JSON schema can be used as input LLM providers
#' which support native function calling and require a JSON schema to
#' describe function and its arguments.
#'
#' @param docs Function documentation as returned by [tools_get_docs()]
#'
#' @return A list (R object) representing a JSON schema for the function
#' @export
#'
#' @examples inst/examples/add_tools.R
tools_docs_to_r_json_schema <- function(
  docs,
  all_required = TRUE,
  additional_properties = FALSE
) {
  # Helper function to process each argument recursively
  process_argument <- function(arg) {
    prop <- list()
    r_type <- arg$type

    if (is.list(r_type)) {
      # Handle named lists (objects)
      prop$type <- "object"
      prop$additionalProperties <- FALSE # Set additionalProperties to FALSE
      prop$properties <- list()
      for (name in names(r_type)) {
        sub_arg <- list(
          type = r_type[[name]],
          default_value = if (!is.null(arg$default_value[[name]]))
            arg$default_value[[name]] else NULL
        )
        prop$properties[[name]] <- process_argument(sub_arg)
      }
    } else if (is.null(r_type) || r_type == "unknown") {
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
      # Check if default_value is a call, e.g. c("Val1", "Val2", ...)
      if (
        is.call(arg$default_value) &&
          identical(arg$default_value[[1]], as.name("c"))
      ) {
        # Evaluate the call to get a standard R vector
        prop$enum <- eval(arg$default_value)
      } else {
        prop$enum <- arg$default_value
      }
    } else if (grepl("^vector ", r_type)) {
      # Handle vector types
      item_type <- sub("^vector ", "", r_type)
      prop$type <- "array"
      if (item_type == "integer") {
        prop$items <- list(type = "integer")
      } else if (item_type == "numeric") {
        prop$items <- list(type = "number")
      } else if (item_type %in% c("string", "character")) {
        prop$items <- list(type = "string")
      } else if (item_type == "logical") {
        prop$items <- list(type = "boolean")
      } else {
        prop$items <- list()
      }
    } else if (r_type == "list") {
      # Handle unnamed lists
      default_value <- arg$default_value
      if (is.null(default_value)) {
        prop$type <- "array"
        prop$items <- list()
      } else if (is.list(default_value)) {
        if (is.null(names(default_value))) {
          # Unnamed list (array)
          prop$type <- "array"
          if (length(default_value) > 0) {
            # Assume homogeneous items
            sub_arg <- list(
              type = arg$type,
              default_value = default_value[[1]]
            )
            prop$items <- process_argument(sub_arg)
          } else {
            prop$items <- list()
          }
        } else {
          # Named list (object)
          prop$type <- "object"
          prop$additionalProperties <- FALSE # Set additionalProperties to FALSE
          prop$properties <- list()
          for (name in names(default_value)) {
            sub_arg <- list(
              type = arg$type[[name]],
              default_value = default_value[[name]]
            )
            prop$properties[[name]] <- process_argument(sub_arg)
          }
        }
      } else {
        prop$type <- "array"
        prop$items <- list()
      }
    } else if (r_type == "call") {
      warning(
        "Function calls are not supported in JSON schema; defaulting to 'string'"
      )
      prop$type <- "string"
    } else {
      warning("Unknown type; defaulting to 'string' in JSON schema")
      prop$type <- "string"
    }
    return(prop)
  }

  # Process arguments
  args <- docs$arguments
  properties <- list()
  required_args <- character(0)

  for (arg_name in names(args)) {
    arg <- args[[arg_name]]
    prop <- process_argument(arg)

    # Add to required arguments if there's no default value
    if (!"default_value" %in% names(arg)) {
      required_args <- c(required_args, arg_name)
    }

    # Add property to properties list
    properties[[arg_name]] <- prop
  }

  if (all_required) required_args <- names(args)

  list(
    type = "object",
    properties = properties,
    required = required_args,
    additionalProperties = additional_properties
  )
}

#' Create arguments text from docs
#'
#' @param docs
#' @param line_prefix
#'
#' @return A string with the arguments formatted as text
#'
#' @noRd
#' @keywords internal
tools_docs_to_text <- function(docs, with_arguments = TRUE) {
  # Internal helper function to process arguments
  tools_docs_to_text_arguments <- function(docs, line_prefix = "  ") {
    process_argument <- function(arg_name, arg_info, prefix = "  ") {
      # Warning for unknown type
      if (arg_info$type == "unknown") {
        warning(
          glue::glue(
            "Argument '{prefix}{arg_name}' has an unknown type. Defaulting to 'string'."
          )
        )
        arg_info$type <- "string"
      }

      # Start argument text
      arg_text <- glue::glue("{prefix}- {arg_name}:", .trim = FALSE)

      # Add description if available
      if (!is.null(arg_info$description)) {
        arg_text <- glue::glue(
          "{arg_text} {arg_info$description}",
          .trim = FALSE
        )
      }

      # Handle match.arg type
      if (arg_info$type == "match.arg") {
        arg_options <- paste(
          paste0('"', arg_info$default_value, '"'),
          collapse = ", "
        )
        arg_text <- glue::glue(
          "{arg_text} [Type: string (one of: {arg_options})]",
          .trim = FALSE
        )
      } else {
        arg_text <- glue::glue(
          "{arg_text} [Type: {arg_info$type}]",
          .trim = FALSE
        )
      }

      return(arg_text)
    }

    process_nested_list <- function(arg_list, prefix = "") {
      nested_text <- ""
      for (arg_name in names(arg_list)) {
        arg_info <- arg_list[[arg_name]]

        # If arg_info is not a list, assume it represents the type
        if (!is.list(arg_info)) {
          arg_info <- list(type = arg_info)
        }

        # Check if we have a nested structure
        if (is.list(arg_info$type) || is.null(arg_info$type)) {
          # Include description if available
          if (!is.null(arg_info$description)) {
            nested_line <- glue::glue(
              "{prefix}- {arg_name}: {arg_info$description} [Type: named list]",
              .trim = FALSE
            )
          } else {
            nested_line <- glue::glue(
              "{prefix}- {arg_name}: [Type: named list]",
              .trim = FALSE
            )
          }

          # Use arg_info$type if it's a list, else use arg_info
          next_level <- if (is.list(arg_info$type)) arg_info$type else arg_info
          nested_subtext <- process_nested_list(
            next_level,
            paste0(prefix, "  ")
          )
          nested_text <- paste(
            nested_text,
            nested_line,
            nested_subtext,
            sep = "\n"
          )
        } else {
          # For simple arguments, handle normally
          arg_line <- process_argument(arg_name, arg_info, prefix)
          nested_text <- paste(
            nested_text,
            arg_line,
            sep = "\n"
          )
        }
      }
      return(nested_text)
    }

    tool_llm_text <- process_nested_list(docs$arguments)

    remove_empty_lines <- function(text) {
      # Split into individual lines
      lines <- unlist(strsplit(text, "\n", fixed = TRUE))

      # Keep only lines that are not empty (after trimming whitespace)
      non_empty_lines <- lines[!grepl("^\\s*$", lines)]

      # Rejoin the cleaned lines into a single string
      cleaned_text <- paste(non_empty_lines, collapse = "\n")

      return(cleaned_text)
    }

    # Apply line prefix to each line after cleaning
    cleaned_result <- remove_empty_lines(tool_llm_text)
    if (nzchar(line_prefix)) {
      lines <- strsplit(cleaned_result, "\n")[[1]]
      lines <- paste0(line_prefix, lines)
      cleaned_result <- paste(lines, collapse = "\n")
    }

    cleaned_result
  }

  # Define text
  tool_llm_text <- glue::glue(
    "  function name: {docs$name}",
    .trim = FALSE
  )
  if (length(docs$description) > 0)
    tool_llm_text <- glue::glue(
      "{tool_llm_text}\n  description: {docs$description}",
      .trim = FALSE
    )
  if (length(docs$arguments) > 0 & with_arguments) {
    tool_llm_text <- glue::glue(
      "{tool_llm_text}\n  arguments:",
      "\n",
      tools_docs_to_text_arguments(docs, line_prefix = "    "),
      .trim = FALSE
    )
  }
  if (length(docs$return$description) > 0)
    tool_llm_text <- glue::glue(
      "{tool_llm_text}\n  return value: {docs$return$description}",
      .trim = FALSE
    )

  return(tool_llm_text)
}
