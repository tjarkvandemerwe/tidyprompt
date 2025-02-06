# Example fake weather function to add to the prompt:
temperature_in_location_nl <- function(
  location = c("Amsterdam", "Utrecht", "Enschede"),
  unit = c("Celcius", "Fahrenheit")
) {
  #' llm_tool::name temperature_in_location_nl
  #'
  #' llm_tool::description Get the temperature in a Dutch city
  #'
  #' llm_tool::param location Location, must be one of: "Amsterdam", "Utrecht", "Enschede"
  #' llm_tool::param unit Unit, must be one of: "Celcius", "Fahrenheit"
  #'
  #' llm_tool::return The temperature in the specified location and unit

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

prompt <- "hi current weather in enschede??" |>
  add_tools(
    tool_functions = list(
      temperature_in_location_nl
    )
  )
prompt
send_prompt(prompt, llm_provider_openai())

prompt <- "What are the files in my current directory?" |>
  add_tools(
    tool_functions = list(
      list.files
    )
  )
send_prompt(prompt, llm_provider_openai())
