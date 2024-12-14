#' Generate an example object from a JSON schema
#'
#' This function generates an example JSON object from a JSON schema.
#' This is called when providing a schema to 'answer_as_json()'
#' with 'schema_in_prompt_as = "example"' and type = 'text-based'/'ollama'.
#'
#' @param schema A list (R object) representing a JSON schema
#'
#' @return A list (R object) which matches the JSON schema definition
#'
#' @export
#'
#' @example inst/examples/answer_as_json.R
#' @family json
r_json_schema_to_example <- function(schema) {
  if ("schema" %in% names(schema)) {
    schema <- schema$schema
  }

  if (is.null(schema$type)) {
    return(NULL)
  }
  switch(
    schema$type,
    object = {
      # Generate an object with properties
      result <- list()
      if (!is.null(schema$properties)) {
        for (name in names(schema$properties)) {
          result[[name]] <- r_json_schema_to_example(schema$properties[[name]])
        }
      }
      return(result)
    },
    array = {
      # Generate an array with one example item
      example_item <- r_json_schema_to_example(schema$items)
      return(list(example_item))
    },
    string = {
      return(ifelse(!is.null(schema$example), schema$example, "..."))
    },
    number = {
      return(ifelse(!is.null(schema$example), schema$example, 123.45))
    },
    integer = {
      return(ifelse(!is.null(schema$example), schema$example, 123))
    },
    boolean = {
      return(ifelse(!is.null(schema$example), schema$example, TRUE))
    },
    NULL
  ) # For unsupported or missing types, return NULL
}



#' Generate a JSON schema from an example object
#'
#' This function generates a JSON schema from an example R object.
#'
#' @param example A list (R object) that represents an example object
#'
#' @return A list (R object) representing a JSON schema matching
#' the example object
#' @export
#'
#' @example inst/examples/answer_as_json.R
#'
#' @family answer_as_json
#' @family json
r_json_schema_from_example <- function(example) {
  if (!requireNamespace("tidyjson", quietly = TRUE))
    stop(paste0(
      "The 'tidyjson' package is required to generate a JSON schema",
      " from an example object"
    ))

  # Convert the list to JSON
  json_data <- jsonlite::toJSON(example, auto_unbox = TRUE) |> as.character()

  # Generate the JSON schema
  schema <- json_data |> tidyjson::json_schema()

  return(schema)
}
