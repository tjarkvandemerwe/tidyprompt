#' @title Make LLM answer as JSON
#'
#' @description This functions wraps a prompt with settings that ensure the LLM response
#' is a valid JSON object, optionally matching a given JSON schema.
#'
#' The function can work with all models and providers when using type "text-based",
#' but also supports native settings for the OpenAI-type APIs and the Ollama API.
#'
#' When providing a schema, for type "text-based" and "ollama", the JSON schema will be validated
#' within the extraction function using the [jsonvalidate](https://cran.r-project.org/web/packages/jsonvalidate/index.html) package.
#' For type "openai", the schema will be added to the API request parameters and the API will
#' then ensure the response matches the schema.
#'
#' @param prompt A single string or a [tidyprompt()] object
#' @param type The way in which the JSON response will be enforced. Must be
#' one of "text-based", "ollama", or "openai".
#' \itemize{
#'  \item "text-based": The JSON response will be extracted from the LLM response
#'  and validated within an extraction function.
#'  \item "openai": Relevant parameters will be added to the prompt through which
#'  the API will enforce that the response is a valid JSON object. When providing
#'  a schema, the 'response_format' parameter will be set to 'json_schema' and the
#'  schema will also be added (enforcing a JSON response according to the schema).
#'  When not providing a schema, the 'response_format' parameter will be set to 'json_object'
#'  which enforces a JSON object response (but not any specific schema). For the latter,
#'  a request for a JSON object is added to the prompt as is required by the OpenAI API.
#'  \item "ollama": The parameter 'format' will be set to 'json' in the API request
#'  (enforcing a JSON response). Besides that, the handling is the same as "text-based".
#'  As recommended by Ollama, a request for a JSON object is added to the prompt.
#' }
#' "text-based" is more generally applicable, while "openai" may be more efficient but
#' specific to the conditions of the API (note that APIs besides the OpenAI API
#' may follow the same structure, in which case "openai" may also be used for those APIs)
#' @param schema (optional) A list representing a JSON schema object that the response must match.
#' \itemize{
#'  \item When using type "text-based" or "ollama", the schema will be
#'  added to the original prompt (see argument: 'schema_in_prompt_as'),
#'  and the response will be validated against the schema with
#'  [jsonvalidate::json_validate()].
#'  \item When using "openai", the schema will be added to the API request parameters
#'  and the API will ensure the response matches the schema.
#' }
#' See example and/or the OpenAI API documentation for more information on defining JSON schemas
#' @param schema_strict If TRUE, the provided schema will be strictly enforced.
#' This option is passed the the API when using type "openai" and to the [jsonvalidate::json_validate()]
#' function when using "text-based" or "ollama". Default is FALSE (as it
#' is in the OpenAI API)
#' @param schema_in_prompt_as (optional) If providing a schema, and when using type
#' "text-based" or "ollama", this argument specifies how the schema should be included in the prompt.
#' \itemize{
#'  \item "example" (default): The schema will be included as an example JSON object.
#'  \item "schema": The schema will be included as a JSON schema.
#' }
#' "example" appears to work better in practice
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
answer_as_json <- function(
    prompt,
    type = c("text-based", "ollama", "openai"),
    schema = NULL,
    schema_strict = FALSE,
    schema_in_prompt_as = c("example", "schema")
) {
  prompt <- tidyprompt(prompt)
  type <- match.arg(type)
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

  if (type == "openai") {
    if (is.null(schema)) {
      prompt$parameters$response_format <- list(type = "json_object")
    } else {
      schema$strict <- schema_strict

      prompt$parameters$response_format <- list(
        type = "json_schema",
        json_schema = schema
      )
    }
  }

  if (type == "ollama")
    prompt$parameters$format <- "json"

  schema_instruction <- NULL
  if (type %in% c("text-based", "ollama") & !is.null(schema)) {
    if (!requireNamespace("jsonvalidate", quietly = TRUE)) {
      warning(paste0(
        "When using type = 'text-based'/'ollama' and providing a schema,",
        " the 'jsonvalidate' package must be installed to validate the response",
        " against the schema. Currently, the 'jsonvalidate' package is not installed",
        " and the LLM response will not be validated against the schema"
      ))
    }

    if (schema_in_prompt_as == "example") {
      schema_instruction <- paste0(
        "Your JSON object should match this example JSON object:\n",
        jsonlite::toJSON(generate_json_example_from_schema(schema), auto_unbox = TRUE, pretty = TRUE)
      )
    } else if (schema_in_prompt_as == "schema") {
      schema_instruction <- paste0(
        "Your JSON object should match this JSON schema:\n",
        jsonlite::toJSON(schema, auto_unbox = TRUE, pretty = TRUE)
      )
    }
  }

  modify_fn <- function(prompt_text) {
    if (type == "openai" & !is.null(schema))
      return(prompt_text)

    prompt_text <- glue::glue(
      "{prompt_text}\n\n",
      "Your must format your response as a JSON object."
    )

    if (
      (type %in% c("text-based", "ollama"))
      & !is.null(schema_instruction)
    )
      prompt_text <- paste0(prompt_text, "\n\n", schema_instruction)

    return(prompt_text)
  }

  extraction_fn <- function(llm_response) {
    jsons <- extraction_fn_json(llm_response)

    if (length(jsons) == 0)
      return(llm_feedback(
        "You must respond as a valid JSON object."
      ))

    if (length(jsons) == 1)
      jsons <- jsons[[1]]

    if (
      !is.null(schema)
      & type %in% c("text-based", "ollama")
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

  prompt_wrap(prompt, modify_fn, extraction_fn)
}



#' Generate an example object from a JSON schema
#'
#' This function generates an example JSON object from a JSON schema.
#' This is called when providing a schema to 'answer_as_json()'
#' with 'schema_in_prompt_as = "example"' and type = 'text-based'/'ollama'.
#'
#' @param schema JSON schema
#'
#' @return A list representing an example object that matches the schema.
#' If the schema is not valid or does not contain a 'type' field, NULL is returned.
#' Convert the result to true JSON with 'jsonlite::toJSON()'
#'
#' @export
#'
#' @example inst/examples/answer_as_json.R
#' @family answer_as_json
generate_json_example_from_schema <- function(schema) {
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
          result[[name]] <- generate_json_example_from_schema(schema$properties[[name]])
        }
      }
      return(result)
    },
    array = {
      # Generate an array with one example item
      example_item <- generate_json_example_from_schema(schema$items)
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
