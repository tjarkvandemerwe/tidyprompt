test_that("answer_as_named_list works correctly", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a named list with name and age." |>
    answer_as_named_list(item_names = c("name", "age")) |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.list(response))
  expect_true("name" %in% names(response))
  expect_true("age" %in% names(response))
})

test_that("answer_as_named_list handles item_instructions", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a named list with name and age." |>
    answer_as_named_list(
      item_names = c("name", "age"),
      item_instructions = list(
        name = "The name of the person",
        age = "The age of the person"
      )
    ) |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.list(response))
  expect_true("name" %in% names(response))
  expect_true("age" %in% names(response))
})

test_that("answer_as_named_list handles item_validations", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a named list with name and age." |>
    answer_as_named_list(
      item_names = c("name", "age"),
      item_validations = list(
        age = function(x) {
          if (x < 0) {
            return(llm_feedback("Age must be a non-negative number."))
          }
          return(TRUE)
        }
      )
    ) |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.list(response))
  expect_true("name" %in% names(response))
  expect_true("age" %in% names(response))
  expect_true(response$age >= 0)
})

test_that("answer_as_named_list handles missing item names", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a named list with name and age." |>
    answer_as_named_list(item_names = c("name", "age")) |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.list(response))
  expect_true("name" %in% names(response))
  expect_true("age" %in% names(response))
})
