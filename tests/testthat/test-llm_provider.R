# Test: Initialization of llm_provider with basic parameters
test_that("llm_provider initializes with parameters", {
  parameters <- list(model = "my-llm-model", api_key = "my-api-key")
  provider <- `llm_provider-class`$new(complete_chat_function = function(chat_history) list(role = "assistant", content = "Hello"), parameters = parameters)

  expect_s3_class(provider, "Llm_provider")
  expect_equal(provider$parameters, parameters)
})

# Test: Setting and updating parameters
test_that("llm_provider updates parameters correctly", {
  parameters <- list(model = "my-llm-model", api_key = "my-api-key")
  provider <- `llm_provider-class`$new(complete_chat_function = function(chat_history) list(role = "assistant", content = "Hello"), parameters = parameters)

  # Update parameters
  new_parameters <- list(api_key = "new-api-key", timeout = 10)
  provider$set_parameters(new_parameters)

  updated_params <- provider$parameters
  expect_equal(updated_params$model, "my-llm-model")
  expect_equal(updated_params$api_key, "new-api-key")
  expect_equal(updated_params$timeout, 10)
})

# Test: complete_chat function with verbose on
test_that("llm_provider complete_chat prints message when verbose is TRUE", {
  test_chat_function <- function(chat_history) {
    return(list(completed = data.frame(
      role = "assistant",
      content = "Hello!"
    )))
  }

  provider <- `llm_provider-class`$new(complete_chat_function = test_chat_function, verbose = TRUE)

  # Test interaction with chat history
  chat_history <- data.frame(role = "user", content = "Hello")
  expect_message(provider$complete_chat(list(chat_history = chat_history)), "--- Receiving response from LLM provider: ---")
})

# Test: Fake LLM provider responses
test_that("llm_provider_fake returns expected response for known prompt", {
  provider_fake <- llm_provider_fake(verbose = FALSE)

  chat_history <- data.frame(role = "user", content = "Hi there!")
  result <- provider_fake$complete_chat(list(chat_history = chat_history))
  response <- result$completed |> utils::tail(1)

  expect_equal(response$role, "assistant")
  expect_match(response$content, "nice to meet you")
})

# Test: Invalid parameters handling
test_that("llm_provider errors on invalid parameters", {
  expect_error(`llm_provider-class`$new(complete_chat_function = function(chat_history) list(role = "assistant", content = "Hello"), parameters = list("unnamed")), "parameters must be a named list")
})
