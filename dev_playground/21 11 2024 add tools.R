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
    return(temperature_celcius * 9/5 + 32)
  }
}


# Example fake weather function to add to the prompt:
temperature_in_location_germany <- function(
    location = c("Berlin", "Munich", "Hamburg"),
    unit = c("Celcius", "Fahrenheit")
) {
  #' llm_tool::name temperature_in_location_germany
  #'
  #' llm_tool::description Get the temperature in a German city
  #'
  #' llm_tool::param location Location, must be one of: "Berlin", "Munich", "Hamburg"
  #' llm_tool::param unit Unit, must be one of: "Celcius", "Fahrenheit"
  #'
  #' llm_tool::return The temperature in the specified location and unit
  #'
  #' llm_tool::example
  #' temperature_in_location_germany("Munich", "Celcius")

  location <- match.arg(location)
  unit <- match.arg(unit)

  temperature_celcius <- switch(
    location,
    "Berlin" = 32.5,
    "Munich" = 19.8,
    "Hamburg" = 22.7
  )

  if (unit == "Celcius") {
    return(temperature_celcius)
  } else {
    return(temperature_celcius * 9/5 + 32)
  }
}


# Attempt to extract documentation as it is extracted by add_tools():
add_tools_extract_documentation(temperature_in_location_germany)

"Hi, what is the weather in New York? Give me Celcius degrees" |>
  add_tools(tool_functions = list(
    temperature_in_location_nl,
    temperature_in_location_germany
  )) |>
  answer_as_integer() |>
  quit_if() |>
  send_prompt(llm_provider_openai())

prompt <- "hi current weather in enschede??" |>
  add_tools(tool_functions = list(
    temperature_in_location_nl
  ))
prompt
send_prompt(prompt, llm_provider_openai())




