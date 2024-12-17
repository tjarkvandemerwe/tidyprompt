test_that("answer_as_key_value works correctly", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a list of key-value pairs." |>
    answer_as_key_value() |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.list(response))
  expect_true(length(response) > 0)
})

test_that("answer_as_key_value handles key_name and value_name", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a list of key-value pairs." |>
    answer_as_key_value(key_name = "attribute", value_name = "description") |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.list(response))
  expect_true(length(response) > 0)
})

test_that("answer_as_key_value handles pair_explanation", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a list of key-value pairs." |>
    answer_as_key_value(pair_explanation = "Each pair should represent a fruit and its color.") |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.list(response))
  expect_true(length(response) > 0)
})

test_that("answer_as_key_value handles n_unique_items", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a list of key-value pairs." |>
    answer_as_key_value(n_unique_items = 3) |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.list(response))
  expect_equal(length(response), 3)
})

test_that("answer_as_key_value handles list_mode", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a list of key-value pairs." |>
    answer_as_key_value(list_mode = "comma") |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.list(response))
  expect_true(length(response) > 0)
})
