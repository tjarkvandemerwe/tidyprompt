#### Configuration ####

options(tidyprompt.verbose = TRUE)


####  Code to test extractions/validations ####
if (FALSE) {

  ollama <- create_ollama_llm_provider()

  prompt <- "What is 2 + 2?" |>
    answer_by_chain_of_thought() |>
    answer_as_integer(min = 0) |>
    add_text("hiii", position = "before")

  x <- environment(prompt$prompt_wraps[[2]]$modify_fn)

  prompt |>
    send_prompt(ollama, verbose = TRUE)

  "Hi!" |>
    add_text("Can you please calculate what is 5+5? Write the answer out as a word") |>
    answer_as_integer(add_instruction_to_prompt = FALSE) |>
    send_prompt(ollama)

  prompt <- "Hi!" |>
    add_text("Can you please calculate what is 5+5? Write the answer out as a word") |>
    answer_as_integer(add_instruction_to_prompt = FALSE) |>
    set_llm_provider(create_ollama_llm_provider()) |>
    answer_by_chain_of_thought()
  prompt |> construct_prompt_text()
  prompt |> send_prompt()

  tool_functions <- list(temperature_in_location)


  "Hi!" |>
    send_prompt(
      llm_provider = create_ollama_llm_provider(),
      system_prompt = "You are an assistant who always answers in poems. You are also very angry."
    )

}



#### Test prompt wrap ####

if (FALSE) {

  #' Add a mode to a prompt (example function)
  #'
  #' @param prompt_wrap_or_list A single string, a prompt_wrap object, or a list
  #' of prompt_wrap objects.
  #'
  #' @return A prompt list with an added mode prompt wrapper object.
  #' @export
  add_example_mode <- function(prompt_wrap_or_list) {
    prompt_list <- validate_prompt_list(prompt_wrap_or_list)

    new_wrap <- create_prompt_wrap(
      type = "mode",
      modify_fn = function(original_prompt_text, modify_fn_args) {
        return(glue::glue(
          "{original_prompt_text}

        Provide your answer in a way that a 5-year old would understand."
        ))
      },
      modify_fn_args = list()
    )

    # (May also want to add extractions for a mode, etc.)

    return(c(prompt_list, list(new_wrap)))
  }

  # Create prompt with extra text
  pw <- tidyprompt(
    prompt_text = "Enter a number between 1 and 10",
    validation_functions = list(
      function(x) {
        if (x < 1 || x > 10) {
          return("The number should be between 1 and 10")
        }
        return(TRUE)
      }
    ),
    llm_provider = create_ollama_llm_provider()
  ) |>
    add_text("Some text added to the end of the prompt.")

  # Construct prompt text from list of prompt wrappers
  construct_prompt_text(pw) |>
    cat()

  # Example; starting from string, adding mode in the middle but placed at the end
  "Hi there!" |>
    add_text("Some text added to the end of the prompt.") |>
    add_example_mode() |> # This will be placed at the end, even though we add it in the middle
    add_text("More text to add (to be placed before the mode)") |>
    construct_prompt_text() |>
    cat()

  # Example passing to query_llm
  "Hi, how are you?" |>
    add_text("Maybe write a poem to express yourself?") |>
    add_example_mode() |>
    set_llm_provider(create_ollama_llm_provider()) |>
    send_prompt()

}



#### Code to test extractions/validations ####

if (FALSE) {

  "Hi!" |>
    add_text("Can you please calculate what is 5+5? Write the answer out as a word") |>
    answer_as_integer(add_instruction_to_prompt = FALSE) |>
    set_llm_provider(create_ollama_llm_provider()) |>
    answer_by_chain_of_thought(extract_from_finish_brackets = FALSE) |>
    send_prompt()

  "Hi!" |>
    add_text("Can you please calculate what is 5+5? Write the answer out as a word") |>
    answer_as_integer(add_instruction_to_prompt = FALSE) |>
    set_llm_provider(create_ollama_llm_provider()) |>
    answer_by_chain_of_thought(extract_from_finish_brackets = FALSE) |>
    send_prompt()

  "Hi!" |>
    send_prompt(
      llm_provider = create_ollama_llm_provider(),
      system_prompt = "You are an assistant who always answers in poems. You are also very angry."
    )

  "Hi" |>
    send_prompt(llm_provider = create_ollama_llm_provider())

}



#### Tool testing ####

if (FALSE) {

  #' Example function to pass to LLM
  #'
  #' @param location ...
  #' @param unit ...
  #'
  #' @return ...
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


  "Hi, what is the weather in Enschede? Give me Celcius degrees" |>
    add_tools(tool_functions = list(temperature_in_location)) |>
    send_prompt(llm_provider = create_ollama_llm_provider())

  # For test:
  # tool_functions <- list(temperature_in_location)
  # # Convert tool_functions to named list, taking the name from the function documentation
  # tool_functions <- setNames(tool_functions, sapply(tool_functions, function(f) {
  #   docs <- extract_function_docs(f)
  #   return(docs$name)
  # }))
  #
  # llm_response <- 'Let me call it with Enschede as the location and Celsius as the unit...
  # FUNCTION[temperature_in_location]("Enschede", "Celcius")'
  #
  # tool_extraction(llm_response, tool_functions)

}
