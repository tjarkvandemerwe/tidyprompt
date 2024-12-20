test_that("create tidyprompt", {
  prompt <- "Hi" |> tidyprompt()
  expect_s3_class(prompt, "Tidyprompt")
})

test_that("construct simple prompt text", {
  prompt <- "Hi" |> tidyprompt()
  expect_equal(construct_prompt_text(prompt), "Hi")
})

test_that("construct more complex prompt text", {
  prompt <- "Hi" |> add_text("How are you?", sep = "\n\n", position = "after")
  expect_equal(
    construct_prompt_text(prompt), "Hi\n\nHow are you?"
  )
})

test_that("construct prompt text based on llm_provider", {
  fake <- llm_provider_fake()

  prompt <- "Hi" |>
    prompt_wrap(
      modify_fn = function(x) {
        if (!is.null(llm_provider)) {
          if (!inherits(llm_provider, "LlmProvider")) {
            stop("llm_provider must be an LlmProvider object")
          }
        }

        if (isTRUE(llm_provider$api_type == "fake")) {
          return("text for fake llm provider")
        }

        return("text for other llm provider")
      }
    )

  text_fake <- construct_prompt_text(prompt, llm_provider = fake)
  text_other <- construct_prompt_text(prompt)

  expect_identical(text_fake, "text for fake llm provider")
  expect_identical(text_other, "text for other llm provider")
})

test_that("create tidyprompt from chat_history object", {
  prompt <- "Hi" |> add_text("How are you?", sep = "\n\n", position = "after")

  chat_history <- prompt$get_chat_history()

  # Expected tidyprompt behavior: construct prompt from chat history
  prompt2 <- tidyprompt(chat_history)

  # Check class
  expect_s3_class(prompt2, "Tidyprompt")

  # Check text
  expect_equal(
    construct_prompt_text(prompt2), "Hi\n\nHow are you?"
  )
})

test_that("setting and getting chat history returns consistent results", {
  # Example chat history
  chat_history <- dplyr::tibble(
    role = c("user", "assistant", "user"),
    content = c("What is 5 + 5?", "10", "And what is 5 + 6?"),
    tool_result = c(FALSE, FALSE, NA)
  ) |> chat_history()

  # Create Tidyprompt object and set chat history
  # Initial prompt should not show up.
  prompt <- tidyprompt("Initial prompt")
  prompt$set_chat_history(chat_history)

  # Get chat history and compare
  retrieved_chat_history <- prompt$get_chat_history()
  expect_equal(retrieved_chat_history, chat_history)
})

test_that("can create tidyprompt from tidyprompt", {
  prompt <- tidyprompt("hi") |>
    answer_as_integer() |>
    quit_if()
  prompt2 <- tidyprompt(prompt)

  expect_identical(prompt, prompt2)
})
