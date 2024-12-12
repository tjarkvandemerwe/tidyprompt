testthat::test_that("send message with fake llm provider", {
  ollama <- llm_provider_fake()
  result <- ollama$complete_chat("Hi!")

  # Check length of result
  testthat::expect_length(result$completed, 3)

  # Check that 'role' and 'response' are in the result
  testthat::expect_true(all(c("role", "content") %in% names(result$completed)))
})

testthat::test_that("send basic prompt", {
  result <- "Hi" |>
    send_prompt(llm_provider_fake())

  # Check that the result is a single character string
  testthat::expect_true(is.character(result))
  testthat::expect_length(result, 1)
})

testthat::test_that("send prompt with text added", {
  result <- "Hi" |>
    add_text("How are you?") |>
    send_prompt(llm_provider_fake())

  testthat::expect_true(is.character(result))
  testthat::expect_length(result, 1)
})

testthat::test_that("send prompt with validation & extraction added", {
  result <- "Hi" |>
    add_text("What is 2 + 2?") |>
    answer_as_integer() |>
    send_prompt(llm_provider_fake())

  is_whole_number <- function(x) {
    is.numeric(x) && x == floor(x)
  }

  testthat::expect_true(is_whole_number(result))
  testthat::expect_length(result, 1)
})

testthat::test_that("send prompt with mode added", {
  result <- "What is 2 + 2?" |>
    answer_by_chain_of_thought() |>
    answer_as_integer() |>
    send_prompt(llm_provider_fake())

  is_whole_number <- function(x) {
    is.numeric(x) && x == floor(x)
  }

  testthat::expect_true(is_whole_number(result))
  testthat::expect_length(result, 1)
})
