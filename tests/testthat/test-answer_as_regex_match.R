test_that("answer_as_regex_match works correctly with full_match mode", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a valid email address." |>
    answer_as_regex_match(regex = "^[\\w\\.-]+@[\\w\\.-]+\\.\\w+$", mode = "full_match") |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.character(response))
  expect_match(response, "^[\\w\\.-]+@[\\w\\.-]+\\.\\w+$")
})

test_that("answer_as_regex_match works correctly with extract_matches mode", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a text containing multiple email addresses." |>
    answer_as_regex_match(regex = "[\\w\\.-]+@[\\w\\.-]+\\.\\w+", mode = "extract_matches") |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.character(response))
  expect_true(length(response) > 0)
  expect_true(all(grepl("[\\w\\.-]+@[\\w\\.-]+\\.\\w+", response)))
})

test_that("answer_as_regex_match handles invalid responses", {
  fake_llm <- llm_provider_fake()

  response <- "Provide a valid email address." |>
    answer_as_regex_match(regex = "^[\\w\\.-]+@[\\w\\.-]+\\.\\w+$", mode = "full_match") |>
    send_prompt(fake_llm, verbose = TRUE)

  expect_true(is.character(response))
  expect_false(grepl("^[\\w\\.-]+@[\\w\\.-]+\\.\\w+$", response))
})
