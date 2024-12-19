test_that("answer_as_text returns constrained text", {
  skip_test_if_no_ollama()

  ollama <- llm_provider_ollama()

  response <- "Provide a short description of the sky." |>
    answer_as_text(max_words = 10, max_characters = 50) |>
    send_prompt(ollama, verbose = FALSE)

  expect_true(is.character(response))
  expect_true(length(strsplit(response, "\\s+")[[1]]) <= 10)
  expect_true(nchar(response) <= 50)
})
