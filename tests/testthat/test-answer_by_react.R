test_that("answer_by_react works correctly", {
  fake_llm <- llm_provider_fake()

  response <- "Explain the process of photosynthesis." |>
    answer_by_react() |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.character(response))
  expect_true(nchar(response) > 0)
})

test_that("answer_by_react handles extract_from_finish_brackets", {
  fake_llm <- llm_provider_fake()

  response <- "Explain the process of photosynthesis." |>
    answer_by_react(extract_from_finish_brackets = TRUE) |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.character(response))
  expect_true(nchar(response) > 0)
})

test_that("answer_by_react handles extraction_lenience", {
  fake_llm <- llm_provider_fake()

  response <- "Explain the process of photosynthesis." |>
    answer_by_react(extraction_lenience = TRUE) |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.character(response))
  expect_true(nchar(response) > 0)
})
