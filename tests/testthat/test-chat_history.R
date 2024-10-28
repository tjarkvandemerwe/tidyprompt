test_that("chat_history.character creates a valid chat_history data frame", {
  # Test with a single string input
  result <- chat_history("Hello, this is a test message.")
  expect_s3_class(result, "chat_history")
  expect_equal(ncol(result), 2)
  expect_equal(result$role, "user")
  expect_equal(result$content, "Hello, this is a test message.")
})

test_that("chat_history.character errors on non-single string inputs", {
  # Test with multiple strings
  expect_error(chat_history(c("Hello", "This should fail")),
               "A single character string is expected for chat history input.")
})

test_that("chat_history.data.frame accepts valid chat history data frame", {
  # Valid data frame input
  valid_df <- data.frame(
    role = c("user", "assistant", "system"),
    content = c("User message", "Assistant response", "System message"),
    stringsAsFactors = FALSE
  )
  result <- chat_history(valid_df)
  expect_s3_class(result, "chat_history")
  expect_equal(ncol(result), 2)
  expect_equal(result$role, c("user", "assistant", "system"))
  expect_equal(result$content, c("User message", "Assistant response", "System message"))
})

test_that("chat_history.data.frame errors on missing columns", {
  # Data frame missing 'role' or 'content' column
  invalid_df <- data.frame(
    role = c("user", "assistant"),
    text = c("Message 1", "Message 2"),
    stringsAsFactors = FALSE
  )
  expect_error(chat_history(invalid_df),
               "The data frame must contain exactly two columns: 'role' and 'content'.")
})

test_that("chat_history.data.frame errors on invalid role values", {
  # Data frame with invalid role value
  invalid_df <- data.frame(
    role = c("user", "bot"),
    content = c("User message", "Invalid role"),
    stringsAsFactors = FALSE
  )
  expect_error(chat_history(invalid_df),
               "The 'role' column must contain only 'user', 'assistant', or 'system'.")
})

test_that("chat_history.data.frame errors on non-character content column", {
  # Data frame with non-character content column
  invalid_df <- data.frame(
    role = c("user", "assistant"),
    content = c(1, 2),
    stringsAsFactors = FALSE
  )
  expect_error(chat_history(invalid_df),
               "The 'content' column must be of type character.")
})

test_that("chat_history.default errors on invalid input types", {
  # Test with numeric input
  expect_error(chat_history(42),
               "The input must be either a data frame with 'role' and 'content' columns, or a single character string.")
})
