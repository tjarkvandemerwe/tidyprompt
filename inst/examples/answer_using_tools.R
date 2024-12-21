\dontrun{
  # When using functions from base R or R packages,
  #   documentation is automatically extracted from help files:
  "What are the files in my current directory?" |>
    answer_using_tools(dir) |> # 'dir' function is from base R
    send_prompt()
}

# Custom functions may also be provided;
#   in this case, some documentation is extracted from the function's formals;
#   descriptions may be added manually. See below

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
    return(temperature_celcius * 9/5 + 32)
  }
}

# Generate documentation for a function
#   (based on formals, & help file if available)
docs <- tools_get_docs(temperature_in_location)

# The types get inferred from the function's formals
# However, descriptions are still missing as the function is not from a package
# We can modify the documentation object to add descriptions:
docs$description <- "Get the temperature in a location"
docs$arguments$unit$description <- "Unit in which to return the temperature"
docs$arguments$location$description <- "Location for which to return the temperature"
docs$return$description <- "The temperature in the specified location and unit"
# (See `?tools_add_docs` for more details on the structure of the documentation)

# When we are satisfied with the documentation, we can add it to the function:
temperature_in_location <- tools_add_docs(temperature_in_location, docs)

\dontrun{
  # Now the LLM can use the function:
  "Hi, what is the weather in Enschede? Give me Celcius degrees" |>
    answer_using_tools(temperature_in_location) |>
    send_prompt()
}
