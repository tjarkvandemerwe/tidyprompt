# Example function to pass to a LLM as a tool
temperature_in_location <- function(
    location = c("Amsterdam", "Utrecht", "Enschede"),
    unit = c("Celcius", "Fahrenheit")
) {
  #' @name temperature_in_location
  #'
  #' @description Get the temperature in a location
  #'
  #' @param location Location, must be one of: "Amsterdam", "Utrecht", "Enschede"
  #' @param unit Unit, must be one of: "Celcius", "Fahrenheit"
  #'
  #' @return The temperature in the specified location and unit
  #'
  #' @example
  #' temperature_in_location("Amsterdam", "Fahrenheit")
  location <- match.arg(location)
  unit <- match.arg(unit)

  temperature_celcius <- switch(
    location,
    "Amsterdam" = 20,
    "Utrecht" = 21,
    "Enschede" = 22
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
  if (section_keyword == "@param") {
    param_lines <- doc_lines[section_starts]

    # Clean up the lines
    param_lines <- sapply(param_lines, clean_line)

    # Remove section keyword from the start of each line; also remove leading "'# ' if it's there
    param_lines <- gsub(paste0("^", section_keyword, "\\s*"), "", param_lines)
    param_lines <- param_lines |> setNames(NULL)

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
  next_section <- grep("^\\s*#'\\s*@", doc_lines)
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
#' with the tags: 'name', 'description', 'param', 'return', and 'example'.
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
  func_text <- capture.output(print(func))

  # Find documentation lines
  doc_lines <- grep("^\\s*#'", func_text, value = TRUE)

  # Extract parameters, return values, and examples using the generic function
  name <- extract_doc_section(doc_lines, "@name")
  description <- extract_doc_section(doc_lines, "@description")
  params <- extract_doc_section(doc_lines, "@param")
  return_value <- extract_doc_section(doc_lines, "@return")
  example <- extract_doc_section(doc_lines, "@example")

  # Convert example to how LLM should call it
  converted_example <- sub("^(\\w+)\\((.*)\\)$", "FUNCTION[\\1](\\2)", example)
  converted_example <- gsub("\"", "", converted_example)

  # Return the results as a list
  return(list(
    name = name,
    description = description,
    parameters = params,
    return_value = return_value,
    example = converted_example
  ))
}

# Example usage:
docs <- extract_function_docs(temperature_in_location)
print(docs)
