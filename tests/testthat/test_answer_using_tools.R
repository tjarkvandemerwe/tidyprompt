test_that("LLM can use R function without error", {
  skip_test_if_no_ollama()

  prompt <- "What files are in my current directory?" |>
    answer_using_tools(dir)

  result <- prompt |>
    send_prompt(llm_provider_ollama())

  expect_true(!is.null(result))
})
