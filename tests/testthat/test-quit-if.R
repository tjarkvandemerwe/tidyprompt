test_that("LLM can exit evaluation via quit_if", {
  skip_test_if_no_ollama()

  response <- "what is the favorite number of the dog of my step father who moved to the north pole?" |>
    set_system_prompt("you are an assistant who does not guess things") |>
    answer_as_integer() |>
    quit_if() |>
    send_prompt(llm_provider_ollama(), verbose = FALSE)

  expect_true(is.null(response))
})
