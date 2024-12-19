test_that("answer_as_named_list returns named list", {
  skip_test_if_no_ollama()

  ollama <- llm_provider_ollama()

  response <- "Provide a named list with name and age." |>
    answer_as_named_list(item_names = c("name", "age")) |>
    send_prompt(ollama, verbose = FALSE)

  expect_true(is.list(response))
  expect_true(all(c("name", "age") %in% names(response)))
})
