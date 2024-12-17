test_that("answer_as_text works correctly", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a short text response." |>
    answer_as_text() |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.character(response))
  expect_true(nchar(response) > 0)
})

test_that("answer_as_text handles max_words constraint", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a short text response." |>
    answer_as_text(max_words = 5) |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.character(response))
  expect_true(length(strsplit(response, "\\s+")[[1]]) <= 5)
})

test_that("answer_as_text handles max_characters constraint", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a short text response." |>
    answer_as_text(max_characters = 50) |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.character(response))
  expect_true(nchar(response) <= 50)
})

test_that("answer_as_text handles invalid responses", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a short text response." |>
    answer_as_text(max_words = 1) |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.character(response))
  expect_true(length(strsplit(response, "\\s+")[[1]]) <= 1)
})
