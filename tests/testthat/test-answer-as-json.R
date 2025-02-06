test_that("answer_as_json (text-based) works without schema", {
  skip_test_if_no_ollama()

  response <- "Create a very short persona" |>
    answer_as_json(type = "text-based") |>
    send_prompt(llm_provider_ollama())

  expect_true(is.list(response), info = "Response should be a list")
})

test_that("answer_as_json (text-based) works with json schema", {
  skip_test_if_no_ollama()

  schema <- list(
    "$schema" = "http://json-schema.org/draft-04/schema#",
    title = "Persona",
    type = "object",
    properties = list(
      name = list(type = "string", description = "The persona's name"),
      age = list(
        type = "integer",
        minimum = 0,
        description = "The persona's age"
      ),
      gender = list(
        type = "string",
        enum = c("Male", "Female", "Non-binary", "Other"),
        description = "The persona's gender"
      ),
      hobbies = list(
        type = "array",
        items = list(type = "string"),
        description = "List of hobbies"
      ),
      pet = list(
        type = "object",
        description = "Information about the persona's pet",
        properties = list(
          name = list(type = "string", description = "The pet's name"),
          age = list(
            type = "integer",
            minimum = 0,
            description = "The pet's age"
          ),
          species = list(
            type = "string",
            enum = c("Dog", "Cat", "Fish", "Bird", "Other"),
            description = "The pet's species"
          )
        ),
        required = c("name", "age", "species")
      )
    ),
    required = c("name", "age", "gender", "hobbies", "pet"),
    additionalProperties = FALSE
  )

  r_json_schema_to_example(schema)

  response <- "Create a persona" |>
    answer_as_json(schema, schema_strict = TRUE, type = "text-based") |>
    send_prompt(llm_provider_ollama())

  expect_true(is.list(response), info = "Response should be a list")

  # Validate top-level properties
  expect_true(
    all(
      c(
        "name",
        "age",
        "gender",
        "hobbies",
        "pet"
      ) %in%
        names(response)
    )
  )

  # Validate `name`
  expect_true(is.character(response$name), info = "`name` should be a string")
  expect_true(nchar(response$name) > 0, info = "`name` should not be empty")

  # Validate `age`
  expect_true(is.numeric(response$age), info = "`age` should be numeric")
  expect_true(response$age >= 0, info = "`age` should be non-negative")

  # Validate `ge  nder`
  expect_true(
    is.character(response$gender),
    info = "`gender` should be a string"
  )
  expect_true(
    response$gender %in% c("Male", "Female", "Non-binary", "Other"),
    info = "`gender` should be one of the allowed values"
  )

  # Validate `hobbies`
  expect_true(
    is.list(response$hobbies) || is.character(response$hobbies),
    info = "`hobbies` should be a list or a character vector"
  )
  expect_true(
    all(sapply(response$hobbies, is.character)),
    info = "All items in `hobbies` should be strings"
  )

  # Validate `pet` object
  expect_true(is.list(response$pet), info = "`pet` should be a list")
  expect_true(
    is.character(response$pet$name),
    info = "`pet$name` should be a string"
  )
  expect_true(
    nchar(response$pet$name) > 0,
    info = "`pet$name` should not be empty"
  )
  expect_true(
    is.numeric(response$pet$age),
    info = "`pet$age` should be numeric"
  )
  expect_true(response$pet$age >= 0, info = "`pet$age` should be non-negative")
  expect_true(
    is.character(response$pet$species),
    info = "`pet$species` should be a string"
  )
  expect_true(
    response$pet$species %in% c("Dog", "Cat", "Fish", "Bird", "Other"),
    info = "`pet$species` should be one of the allowed values"
  )
})

test_that("answer_as_json (openai via auto) works", {
  skip_test_if_no_openai()

  schema <- list(
    "$schema" = "http://json-schema.org/draft-04/schema#",
    title = "Persona",
    type = "object",
    properties = list(
      name = list(type = "string", description = "The persona's name"),
      age = list(type = "integer", description = "The persona's age"),
      gender = list(
        type = "string",
        enum = c("Male", "Female", "Non-binary", "Other"),
        description = "The persona's gender"
      ),
      hobbies = list(
        type = "array",
        items = list(type = "string"),
        description = "List of hobbies"
      )
    ),
    required = c("name", "age", "gender", "hobbies"),
    additionalProperties = FALSE
  )

  expect_no_error(
    "Create a persona" |>
      answer_as_json(schema, type = "auto") |>
      send_prompt(llm_provider_openai())
  )

  expect_no_error(
    "Create a very short persona" |>
      answer_as_json(type = "auto") |>
      send_prompt(llm_provider_openai())
  )
})

test_that("answer_as_json (ollama via auto) works", {
  skip_test_if_no_ollama()

  schema <- list(
    "$schema" = "http://json-schema.org/draft-04/schema#",
    title = "Persona",
    type = "object",
    properties = list(
      name = list(type = "string", description = "The persona's name"),
      age = list(type = "integer", description = "The persona's age"),
      gender = list(
        type = "string",
        enum = c("Male", "Female", "Non-binary", "Other"),
        description = "The persona's gender"
      ),
      hobbies = list(
        type = "array",
        items = list(type = "string"),
        description = "List of hobbies"
      )
    ),
    required = c("name", "age", "gender", "hobbies"),
    additionalProperties = FALSE
  )

  expect_no_error(
    "Create a persona" |>
      answer_as_json(schema, type = "auto") |>
      send_prompt(llm_provider_ollama())
  )

  expect_no_error(
    "Create a very short persona" |>
      answer_as_json(type = "auto") |>
      send_prompt(llm_provider_ollama())
  )
})
