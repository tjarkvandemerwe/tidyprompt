test_that("answer_as_regex_match returns matches the regex", {
  skip_test_if_no_ollama()

  ollama <- llm_provider_ollama()

  response <- "Extract all numbers from this text: 123, 456, and 789." |>
    answer_as_regex_match("\\d+", mode = "extract_matches") |>
    send_prompt(ollama, verbose = FALSE)

  expect_true(is.character(response))
  expect_true(all(grepl("\\d+", response)))
})
