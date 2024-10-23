testthat::test_that("send message with ollama", {
  ollama <- create_ollama_llm_provider()
  result <- ollama$complete_chat("Hi!")

  # Check length of result
  testthat::expect_length(result, 2)

  # Check that 'role' and 'response' are in the result
  testthat::expect_true(all(c("role", "content") %in% names(result)))

  # Check that 'role' is single character string
  testthat::expect_true(is.character(result$role))
  testthat::expect_length(result$role, 1)

  # Check that 'content' is single character string
  testthat::expect_true(is.character(result$content))
  testthat::expect_length(result$content, 1)
})

testthat::test_that("send basic prompt", {
  result <- "Hi" |>
    send_prompt(llm_provider = create_ollama_llm_provider())

  # Check that the result is a single character string
  testthat::expect_true(is.character(result))
  testthat::expect_length(result, 1)
})

testthat::test_that("send prompt with text added", {
  result <- "Hi" |>
    add_text("How are you?") |>
    send_prompt(llm_provider = create_ollama_llm_provider())

  testthat::expect_true(is.character(result))
  testthat::expect_length(result, 1)
})

testthat::test_that("send prompt with validation & extraction added", {
  result <- "Hi" |>
    add_text("What is 5+5?") |>
    answer_as_integer() |>
    send_prompt(llm_provider = create_ollama_llm_provider())

  testthat::expect_true(is.integer(result))
  testthat::expect_length(result, 1)
})

testthat::test_that("send prompt with mode added", {
  result <- "Hi" |>
    answer_by_chain_of_thought() |>
    send_prompt(llm_provider = create_ollama_llm_provider())

  testthat::expect_true(is.character(result))
  testthat::expect_length(result, 1)
})

testthat::test_that("send prompt with tool added", {
  # Define tool function
  temperature_in_location <- function(
    location = c("Amsterdam", "Utrecht", "Enschede"),
    unit = c("Celcius", "Fahrenheit")
  ) {
    #' llm_tool::name temperature_in_location
    #'
    #' llm_tool::description Get the temperature in a location
    #'
    #' llm_tool::param location Location, must be one of: "Amsterdam", "Utrecht", "Enschede"
    #' llm_tool::param unit Unit, must be one of: "Celcius", "Fahrenheit"
    #'
    #' llm_tool::return The temperature in the specified location and unit
    #'
    #' llm_tool::example
    #' temperature_in_location("Amsterdam", "Fahrenheit")
    location <- match.arg(location)
    unit <- match.arg(unit)

    temperature_celcius <- switch(
      location,
      "Amsterdam" = 32.5,
      "Utrecht" = 19.8,
      "Enschede" = 22.7
    )

    if (unit == "Celcius") {
      return(temperature_celcius)
    } else {
      return(temperature_celcius * 9/5 + 32)
    }
  }

  result <- "Hi, what is the weather in Enschede? Give me Celcius degrees" |>
    add_tools(tool_functions = list(temperature_in_location)) |>
    send_prompt(llm_provider = create_ollama_llm_provider())

  testthat::expect_true(is.character(result))
  testthat::expect_length(result, 1)
})
