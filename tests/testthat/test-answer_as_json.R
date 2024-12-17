test_that("answer_as_json works correctly", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a JSON object with name and age." |>
    answer_as_json() |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.list(response))
  expect_true("name" %in% names(response))
  expect_true("age" %in% names(response))
})

test_that("answer_as_json handles schema validation", {
  fake_llm <- llm_provider_fake()

  schema <- list(
    type = "object",
    properties = list(
      name = list(type = "string"),
      age = list(type = "integer")
    ),
    required = c("name", "age")
  )

  response <- "Provide a JSON object with name and age." |>
    answer_as_json(schema = schema) |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.list(response))
  expect_true("name" %in% names(response))
  expect_true("age" %in% names(response))
  expect_true(is.character(response$name))
  expect_true(is.numeric(response$age))
})

test_that("answer_as_json handles invalid JSON responses", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a JSON object with name and age." |>
    answer_as_json() |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.list(response))
  expect_true("name" %in% names(response))
  expect_true("age" %in% names(response))
})
