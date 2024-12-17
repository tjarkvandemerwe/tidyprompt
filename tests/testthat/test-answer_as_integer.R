test_that("answer_as_integer works correctly", {
  fake_llm <- llm_provider_fake()

  response <- "What is 5 + 5?" |>
    answer_as_integer() |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.numeric(response))
  expect_equal(response, 10)
})

test_that("answer_as_integer handles min and max constraints", {
  fake_llm <- llm_provider_fake()

  response <- "Give me a number between 1 and 10" |>
    answer_as_integer(min = 1, max = 10) |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.numeric(response))
  expect_true(response >= 1 && response <= 10)
})

test_that("answer_as_integer handles invalid responses", {
  fake_llm <- llm_provider_fake()

  response <- "What is the square root of -1?" |>
    answer_as_integer() |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.na(response))
})
