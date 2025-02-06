test_that("text-based function calling works", {
  skip_test_if_no_ollama()

  prompt <- "What files are in my current directory?" |>
    answer_using_tools(dir, type = "text-based")

  result <- prompt |>
    send_prompt(llm_provider_ollama())

  expect_true(!is.null(result))
})

test_that("text-based function calling works with custom function", {
  skip_test_if_no_ollama()

  # Example fake weather function to add to the prompt:
  temperature_in_location <- function(
    location = c("Amsterdam", "Utrecht", "Enschede"),
    unit = c("Celcius", "Fahrenheit")
  ) {
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
      return(temperature_celcius * 9 / 5 + 32)
    }
  }

  # Generate documentation for a function
  #   (based on formals, & help file if available)
  docs <- tools_get_docs(temperature_in_location)
  docs$description <- "Get the temperature in a location"
  docs$arguments$unit$description <- "Unit in which to return the temperature"
  docs$arguments$location$description <- "Location for which to return the temperature"
  docs$
  return$description <- "The temperature in the specified location and unit"
  temperature_in_location <- tools_add_docs(temperature_in_location, docs)

  prompt <- "Hi, what is the weather in Enschede? Give me Celcius degrees" |>
    answer_using_tools(temperature_in_location, type = "text-based")

  result <- prompt |>
    send_prompt(llm_provider_ollama())

  expect_true(!is.null(result))
})

test_that("native ollama function calling works", {
  skip_test_if_no_ollama()

  prompt <- "What files are in my current directory?" |>
    answer_using_tools(dir, type = "ollama")

  result <- prompt |>
    send_prompt(llm_provider_ollama())

  expect_true(!is.null(result))
})

test_that("native openai function calling works", {
  skip_test_if_no_openai()

  prompt <- "What files are in my current directory?" |>
    answer_using_tools(dir, type = "openai")

  result <- prompt |>
    send_prompt(llm_provider_openai())

  expect_true(!is.null(result))
})
