# devtools::load_all()

# Setup LLM-provider
if (FALSE) {

  ollama <- create_ollama_llm_provider()
  openai <- create_openai_llm_provider()

  # Create your own llm_provider with create_llm_provider;
  # to see an example, see source code of create_openai_llm_provider and create_ollama_llm_provider
}


# Basic prompt
if (FALSE) {

  "Hi!" |>
    send_prompt(llm_provider = create_ollama_llm_provider())

  # Add some text with a wrapper
  "Hi!" |>
    add_text("Can you please calculate what is 5+5?") |>
    send_prompt(llm_provider = create_ollama_llm_provider())

  # Construct prompt without sending it
  "Hi!" |>
    add_text("Can you please calculate what is 5+5?") |>
    construct_prompt_text() |>
    cat()

}

# Add output validation
if (FALSE) {

  prompt <- "Can you please calculate what is 5+5?" |>
    answer_as_integer()

  # This will instruction to the prompt, and also extraction + validation
  #   functions for when sending the prompt the LLM
  prompt |>
    construct_prompt_text() |>
    cat()

  prompt |>
    send_prompt(llm_provider = create_ollama_llm_provider())

  # Upon failing validation, the LLM will be sent feedback so that it can retry
  prompt_which_will_initially_fail <- "Can you please calculate what is 5+5?" |>
    add_text("Write out your answer in words, do not use numbers!") |>
    answer_as_integer() |>
    send_prompt(llm_provider = create_ollama_llm_provider())

}

# Add a mode of answering
if (FALSE) {

  prompt <- "Can you please calculate what is 5+5?" |>
    set_mode_chainofthought() |>
    send_prompt(llm_provider = create_ollama_llm_provider())

  # Mode will wrap around the prompt, and may also add extraction + validation
  # E.g., chain-of-thought has the option to request final answer to be
  # within 'Finish[]' and will extract from within those brackets

}

# Add tools (giving LLM the ability to call R-functions)
if (FALSE) {

  # Create function with inline documentation, using llm_function:: tags; example:
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

  # Add tool to prompt
  prompt <- "Hi, what is the weather in Enschede?" |>
    add_tools(temperature_in_location) |>
    send_prompt(llm_provider = create_ollama_llm_provider())

}

