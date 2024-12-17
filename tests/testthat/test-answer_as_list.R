test_that("answer_as_list works correctly", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a list of items." |>
    answer_as_list() |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.list(response))
  expect_true(length(response) > 0)
})

test_that("answer_as_list handles item_name", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a list of items." |>
    answer_as_list(item_name = "fruit") |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.list(response))
  expect_true(length(response) > 0)
})

test_that("answer_as_list handles item_explanation", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a list of items." |>
    answer_as_list(item_explanation = "Each item should represent a fruit.") |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.list(response))
  expect_true(length(response) > 0)
})

test_that("answer_as_list handles n_unique_items", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a list of items." |>
    answer_as_list(n_unique_items = 3) |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.list(response))
  expect_equal(length(response), 3)
})

test_that("answer_as_list handles list_mode", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a list of items." |>
    answer_as_list(list_mode = "comma") |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.list(response))
  expect_true(length(response) > 0)
})
