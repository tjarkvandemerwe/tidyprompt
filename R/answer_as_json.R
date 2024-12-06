#' @title Make LLM answer as JSON (with optional schema)
#'
#' @description This functions wraps a prompt with settings that ensure the LLM response
#' is a valid JSON object, optionally matching a given JSON schema.
#'
#' The function can work with all models and providers through text-based
#' handling, but also supports native settings for the OpenAI-type APIs
#' (llm_provider$api_type == "openai") and the Ollama API (llm_provider$api_type == "ollama").
#' The 'llm_provider$api_type' is taken from the provider used in [send_prompt()];
#' based on the provider, the function will automatically determine the appropriate
#' way to reach the desired JSON outcome.
#'
#' This means that it is possible
#' to easily switch between providers with different levels of JSON support,
#' while ensuring the results will be in the correct format.
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param schema (optional) A list (R object) which represents
#' a JSON schema that the response should match
#' \itemize{
#'  \item When not using the OpenAI API, the schema will be
#'  added to the original prompt (see argument: 'schema_in_prompt_as'),
#'  and the response will be validated against the schema with
#'  [jsonvalidate::json_validate()].
#'  \item When using an OpenAI API, the schema will be added to the API request parameters
#'  and the API will ensure the response matches the schema.
#' }
#' See example and/or the OpenAI API documentation for more information on defining JSON schemas
#' @param schema_strict If TRUE, the provided schema will be strictly enforced.
#' This option is passed the the API when using an OpenAI API and otherwise to
#' [jsonvalidate::json_validate()]
#' @param schema_in_prompt_as (optional) If providing a schema and when not using
#' an OpenAI API, this argument specifies how the schema should be included in the prompt.
#' \itemize{
#'  \item "example" (default): The schema will be included as an example JSON object.
#'  \item "schema": The schema will be included as a JSON schema.
#' }
#' @return A [tidyprompt()] with an added [prompt_wrap()] which will ensure
#' that the LLM response is a valid JSON object
#'
#' @export
#'
#' @example inst/examples/answer_as_json.R
#'
#' @family pre_built_prompt_wraps
#' @family answer_as_prompt_wraps
#' @family answer_as_json
#' @family text_helpers
answer_as_json <- function(
    prompt,
    schema = NULL,
    schema_strict = FALSE,
    schema_in_prompt_as = c("example", "schema")
) {
  prompt <- tidyprompt(prompt)
  schema_in_prompt_as <- match.arg(schema_in_prompt_as)

  parameter_fn <- function(llm_provider) {
    if (llm_provider$api_type == "ollama") {
      return(list(format = "json"))
    }

    if (llm_provider$api_type == "openai") {
      if (is.null(schema)) {
        return(list(response_format = list(type = "json_object")))
      } else {
        schema$strict <- schema_strict

        return(list(
          response_format = list(
            type = "json_schema",
            json_schema = schema
          )
        ))
      }
    }

    return(NULL)
  }

  modify_fn <- function(prompt_text, llm_provider) {
    if (
      isTRUE(llm_provider$api_type == "openai") & !is.null(schema)
    )
      return(prompt_text)

    prompt_text <- glue::glue(
      "{prompt_text}\n\n",
      "Your must format your response as a JSON object."
    )

    if (
      !is.null(schema)
    ) {
      if (!requireNamespace("jsonvalidate", quietly = TRUE)) {
        warning(paste0(
          "When using an llm_provider$api_type other than 'openai' and providing a schema,",
          " the 'jsonvalidate' package must be installed to validate the response",
          " against the schema. Currently, the 'jsonvalidate' package is not installed",
          " and the LLM response will not be validated against the schema"
        ))
      }

      schema_instruction <- NULL
      if (schema_in_prompt_as == "example") {
        schema_instruction <- paste0(
          "Your JSON object should match this example JSON object:\n",
          jsonlite::toJSON(r_json_schema_to_example(schema), auto_unbox = TRUE, pretty = TRUE)
        )
      } else if (schema_in_prompt_as == "schema") {
        schema_instruction <- paste0(
          "Your JSON object should match this JSON schema:\n",
          jsonlite::toJSON(schema, auto_unbox = TRUE, pretty = TRUE)
        )
      }

      schema_instruction <<- schema_instruction
      prompt_text <- paste0(prompt_text, "\n\n", schema_instruction)
    }

    return(prompt_text)
  }

  extraction_fn <- function(llm_response, llm_provider) {
    jsons <- extraction_fn_json(llm_response)

    if (length(jsons) == 0)
      return(llm_feedback(
        "You must respond as a valid JSON object."
      ))

    if (length(jsons) == 1)
      jsons <- jsons[[1]]

    if (
      !is.null(schema)
      & !isTRUE(llm_provider$api_type %in% c("openai"))
      & requireNamespace("jsonvalidate", quietly = TRUE)
    ) {
      # Convert to JSON
      answer_json <- jsonlite::toJSON(jsons, auto_unbox = TRUE, pretty = TRUE)
      schema_json <- jsonlite::toJSON(schema, auto_unbox = TRUE, pretty = TRUE)

      # Validate JSON with verbose error reporting
      validation_result <- jsonvalidate::json_validate(
        answer_json, schema_json, strict = schema_strict, verbose = TRUE
      )

      if (!validation_result) {
        # Extract error details
        error_details <- attr(validation_result, "errors")

        return(llm_feedback(paste0(
          "Your response did not match the expected JSON schema.\n\n",
          df_to_string(error_details), "\n\n",
          schema_instruction
        )))
      }
    }

    jsons
  }

  prompt_wrap(
    prompt,
    modify_fn, extraction_fn, NULL, NULL, parameter_fn,
    name = "answer_as_json"
  )
}



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
#' @family answer_as_json
#' @family text_helpers
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
#' @family json_schema
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
