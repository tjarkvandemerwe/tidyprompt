test_that("answer_as_boolean works correctly", {
  fake_llm <- llm_provider_fake()

  response <- "Is the sky blue?" |>
    answer_as_boolean() |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.logical(response))
  expect_true(response)
})

test_that("answer_as_boolean handles true_definition and false_definition", {
  fake_llm <- llm_provider_fake()

  response <- "Is water wet?" |>
    answer_as_boolean(
      true_definition = "Water is wet",
      false_definition = "Water is not wet"
    ) |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.logical(response))
  expect_true(response)
})

test_that("answer_as_boolean handles invalid responses", {
  fake_llm <- llm_provider_fake()

  response <- "Is fire cold?" |>
    answer_as_boolean() |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.logical(response))
  expect_false(response)
})
