test_that("df_to_string correctly formats a simple data frame in wide format", {
  df <- data.frame(Name = c("Alice", "Bob"), Age = c(25, 30))
  expected_output <- "Name, Age\nAlice, 25\nBob, 30"

  expect_equal(df_to_string(df, how = "wide"), expected_output)
})

test_that("df_to_string correctly formats a simple data frame in long format", {
  df <- data.frame(Name = c("Alice", "Bob"), Age = c(25, 30))
  expected_output <- "Name: Alice\nAge: 25\n\n\nName: Bob\nAge: 30\n\n\n"

  expect_equal(df_to_string(df, how = "long"), expected_output)
})

test_that("df_to_string returns empty string for empty data frame", {
  df <- data.frame()
  expect_equal(df_to_string(df, how = "wide"), "")
  expect_equal(df_to_string(df, how = "long"), "")
})

test_that("df_to_string handles single-column data frame in wide and long formats", {
  df <- data.frame(Age = c(25, 30))

  expected_output_wide <- "Age\n25\n30"
  expect_equal(df_to_string(df, how = "wide"), expected_output_wide)

  expected_output_long <- "Age: 25\n\n\nAge: 30\n\n\n"
  expect_equal(df_to_string(df, how = "long"), expected_output_long)
})

test_that("df_to_string throws error if input is not a data frame", {
  expect_error(df_to_string("not_a_data_frame", how = "wide"), "Input must be a data frame")
  expect_error(df_to_string(list(Name = c("Alice", "Bob")), how = "wide"), "Input must be a data frame")
})

test_that("df_to_string handles data frame with NA values in wide and long formats", {
  df <- data.frame(Name = c("Alice", NA), Age = c(25, 30))

  expected_output_wide <- "Name, Age\nAlice, 25\nNA, 30"
  expect_equal(df_to_string(df, how = "wide"), expected_output_wide)

  expected_output_long <- "Name: Alice\nAge: 25\n\n\nName: NA\nAge: 30\n\n\n"
  expect_equal(df_to_string(df, how = "long"), expected_output_long)
})

test_that("df_to_string defaults to 'wide' format if 'how' parameter is omitted", {
  df <- data.frame(Name = c("Alice", "Bob"), Age = c(25, 30))
  expected_output <- "Name, Age\nAlice, 25\nBob, 30"

  expect_equal(df_to_string(df), expected_output)
})
