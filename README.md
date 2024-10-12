
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyprompt

<!-- badges: start -->
<!-- badges: end -->

`tidyprompt` is an R package to prompt and empower your large language
models (LLMs), the tidy way.

Key features of `tidyprompt` are:

- **tidy prompting**: Quickly and elegantly construct prompts for LLMs,
  using piping syntax (inspired by the `tidyverse`). Wrap a base prompt
  in prompt wrappers to influence how the LLM handles the prompt. A
  library of pre-built prompt wrappers is included, but you can also
  write your own.

- **structured output**: Extract structured output from the LLM’s
  response, and validate it. Automatic retries with feedback to the LLM,
  if the output is not as expected.

- **reasoning modes**: Make your LLM answer in a specific mode, such as
  chain-of-thought or ReAct (Reasoning and Acting) modes.

- **function calling**: Give your LLM the ability to autonomously call R
  functions (‘tools’). With this, the LLM can retrieve information or
  take other actions.

- **compatible with all LLM providers**: Usable with any LLM provider
  that supports chat completion. Use included LLM providers such as
  Ollama (on your local PC or your own server) or the OpenAI API. Or
  easily write a hook for any other LLM provider.

## Installation

You can install the development version of tidyprompt from
[GitHub](https://github.com/tjarkvandemerwe/tidyprompt) with:

``` r
# install.packages("remotes")
remotes::install_github("tjarkvandemerwe/tidyprompt")
```

## Example usage

``` r
library(tidyprompt)
```

### Setup an LLM provider

`tidyprompt` can be used with any LLM provider capable of completing a
chat.

At the moment, `tidyprompt` includes pre-built functions to connect with
Ollama and the OpenAI API.

With the `create_llm_provider` function, you can easily write a hook for
any other LLM provider. You could make API calls using the `httr`
package or use another R package that already has a hook for the LLM
provider you want to use.

``` r
# Ollama running on local PC
ollama <- create_ollama_llm_provider(
  parameters = list(model = "llama3.1:8b", url = "http://localhost:11434/api/chat")
)

# OpenAI API
openai <- create_openai_llm_provider(
  parameters = list(model = "gpt-4o-mini", api_key = Sys.getenv("OPENAI_API_KEY"))
)

# Create your own LLM provider hook using create_llm_provider(); 
#   see ?create_llm_provider for more information, and take a look at
#   the source code of create_ollama_llm_provider() and create_openai_llm_provider()
```

### Basic prompting

A simple string serves as the base for a prompt.

By adding prompt wrappers, you can influence various aspects of how the
LLM handles the prompt, while verifying that the output is structured
and valid (including retries with feedback to the LLM if it is not).

``` r
  "Hi there!" |>
    send_prompt(ollama)
#> [1] "It's nice to meet you. Is there something I can help you with or would you like to chat?"
```

`add_text` is a simple example of a prompt wrapper. It simply adds some
text at the end of the base prompt.

``` r
  "Hi there!" |>
    add_text("What is a large language model? Explain in 10 words.") |>
    send_prompt(ollama)
#> [1] "Complex computer program trained on vast texts to generate human-like responses."
```

You can also construct the final prompt text, without sending it to an
LLM provider.

``` r
  "Hi there!" |>
    add_text("What is a large language model? Explain in 10 words.") |>
    construct_prompt_text() |>
    cat()
#> Hi there!
#> 
#> What is a large language model? Explain in 10 words.
```

### Retrieving output in a specific format

Using prompt wrappers, you can force the LLM to return the output in a
specific format. You can also extract the output to turn it from a
character into another data type.

For instance, the `answer_as_integer` prompt wrapper will force the LLM
to return an integer.

To achieve this, the prompt wrapper will add some text to the base
prompt, asking the LLM to reply with an integer. However, the prompt
wrapper does more: it also will attempt to extract and validate the
integer from the LLM’s response. If extraction or validation fails,
feedback is sent back to the LLM, after which the LLM can retry
answering the prompt.

``` r
  "What is 2 + 2?" |>
    answer_as_integer(add_instruction_to_prompt = TRUE) |>
    send_prompt(ollama, verbose = TRUE)
#> --- Sending message to LLM-provider: ---
#> What is 2 + 2?
#> 
#> You must answer with only an integer (use no other characters).
#> --- Received response from LLM-provider: ---
#> 4
#> [1] 4
```

Below is an example of a prompt which will initially fail, but will
succeed after a retry.

``` r
  "What is 2 + 2?" |>
    add_text("Please write out your reply in words, use no numbers.") |>
    answer_as_integer(add_instruction_to_prompt = FALSE) |>
    send_prompt(ollama, verbose = TRUE)
#> --- Sending message to LLM-provider: ---
#> What is 2 + 2?
#> 
#> Please write out your reply in words, use no numbers.
#> --- Received response from LLM-provider: ---
#> Four.
#> --- Sending message to LLM-provider: ---
#> You must answer with only an integer (use no other characters).
#> --- Received response from LLM-provider: ---
#> 4
#> [1] 4
```

### Adding a reasoning mode to the LLM

Prompt wrappers may also be used to add a reasoning mode to the LLM. It
is hypothesized that this could improve the LLM’s performance on more
complex tasks.

For instance, function `set_mode_chainofthought` will add chain of
thought reasoning mode to the LLM. This wraps the base prompt within a
request for the LLM to reason step by step, asking it to provide the
final answer within ‘FINISH\[<final answer here>\]’. An extraction
function then ensures only the final answer is returned.

``` r
  "What is 2 + 2?" |>
    set_mode_chainofthought() |>
    answer_as_integer() |>
    send_prompt(ollama, verbose = TRUE)
#> --- Sending message to LLM-provider: ---
#> You are given a user's prompt.
#> To answer the user's prompt, you need to think step by step to arrive at a final answer.
#> 
#> ----- START OF USER'S PROMPT -----
#> What is 2 + 2?
#> ----- END OF USER'S PROMPT -----
#> 
#> What are the steps you would take to answer the user's prompt?
#> Describe your thought process in the following format:
#>   >> step 1: <step 1 description>
#>   >> step 2: <step 2 description>
#>   (etc.)
#> 
#> When you are done, you must type:
#> FINISH[<put here your final answer to the user's prompt>]
#> 
#> Make sure your final answer follows the logical conclusion of your thought process.
#> --- Received response from LLM-provider: ---
#> >> step 1: Identify the mathematical operation in the prompt, which is a simple addition problem.
#> 
#> >> step 2: Recall the basic arithmetic fact that 2 + 2 equals a specific numerical value.
#> 
#> >> step 3: Apply this knowledge to determine the result of the addition problem, using the known facts about numbers and their operations.
#> 
#> >> step 4: Conclude that based on this mathematical understanding, the solution to the prompt "What is 2 + 2?" is a fixed numerical quantity.
#> 
#> FINISH[4]
#> [1] 4
```

### Giving tools to the LLM (autonomous function-calling)

With `tidyprompt`, you can define R functions and give the LLM the
ability to call them. This enables the LLM to retrieve additional
information or take other actions.

``` r
  # Define a function that returns fake data about the temperature in a location
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
    
    # As shown above, one can use docstring-like text to document the function.
    #   This will provide the LLM information on what the function does,
    #   and how it should be used.
    
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

  # Ask the LLM a question which can be answered with the function
  "Hi, what is the weather temperature in Enschede?" |>
    add_text("I want to know the Celcius degrees.") |>
    answer_as_integer() |>
    add_tools(temperature_in_location) |>
    send_prompt(ollama, verbose = TRUE)
#> --- Sending message to LLM-provider: ---
#> Hi, what is the weather temperature in Enschede?
#> 
#> I want to know the Celcius degrees.
#> 
#> If you need more information, you can call functions to help you.
#> To call a function, type:
#>   FUNCTION[<function name here>](<argument 1>, <argument 2>, etc...)
#> 
#> The following functions are available:
#> 
#> function name: temperature_in_location
#> description: Get the temperature in a location
#> arguments:
#>     - location: Location, must be one of: "Amsterdam", "Utrecht", "Enschede"
#>     - unit: Unit, must be one of: "Celcius", "Fahrenheit"
#> return value: The temperature in the specified location and unit
#> example usage: FUNCTION[temperature_in_location]("Amsterdam", "Fahrenheit")
#> 
#> After you call a function, wait until you receive more information.
#> --- Received response from LLM-provider: ---
#> I'll use the provided function to get the current temperature in Enschede.
#> 
#> FUNCTION[temperature_in_location]("Enschede", "Celcius")
#> --- Sending message to LLM-provider: ---
#> function called: temperature_in_location
#> arguments used: Enschede, Celcius
#> result: 22.7
#> --- Received response from LLM-provider: ---
#> So the current temperature in Enschede is 22.7 degrees Celsius.
#> --- Sending message to LLM-provider: ---
#> You must answer with only an integer (use no other characters).
#> --- Received response from LLM-provider: ---
#> 22
#> [1] 22
```

### Creating your own prompt wrappers

Under the hood, prompts are just lists of a base prompt (a string) and a
series of prompt wrappers.

You can thus create a function which takes a prompt and appends a new
prompt wrapper to it.

Take a look at the source code for function `add_text`:

``` r
add_text <- function(prompt_wrap_or_list, text, sep = "\n\n") {
  prompt_list <- validate_prompt_list(prompt_wrap_or_list)

  new_wrap <- create_prompt_wrap(
    modify_fn = function(original_prompt_text, modify_fn_args) {
      text <- modify_fn_args$text
      sep <- modify_fn_args$sep
      return(paste(original_prompt_text, text, sep = sep))
    },
    modify_fn_args = list(text = text, sep = sep)
  )

  return(c(prompt_list, list(new_wrap)))
}
```

More complex prompt wrappers may also add extraction and validation
functions. Take a look at the source code for function
`answer_as_integer`:

``` r
answer_as_integer <- function(
    prompt_wrap_or_list, min = NULL, max = NULL, add_instruction_to_prompt = FALSE
) {
  prompt_list <- validate_prompt_list(prompt_wrap_or_list)

  new_wrap <- create_prompt_wrap(
    modify_fn = function(original_prompt_text, modify_fn_args) {
      min <- modify_fn_args$min
      max <- modify_fn_args$max

      new_prompt_text <- original_prompt_text

      if (add_instruction_to_prompt) {
        new_prompt_text <- glue::glue(
          "{new_prompt_text}

          You must answer with only an integer (use no other characters)."
        )

        if (!is.null(min) && !is.null(max)) {
          new_prompt_text <- glue::glue(
            "{new_prompt_text}
            Enter an integer between {min} and {max}."
          )
        } else if (!is.null(min)) {
          new_prompt_text <- glue::glue(
            "{new_prompt_text}
            Enter an integer greater than or equal to {min}."
          )
        } else if (!is.null(max)) {
          new_prompt_text <- glue::glue(
            "{new_prompt_text}
            Enter an integer less than or equal to {max}."
          )
        }
      }

      return(new_prompt_text)
    },
    
    extraction_functions = list(
      function(x) {
        extracted <- suppressWarnings(as.integer(x))
        if (is.na(extracted)) {
          return(create_llm_feedback("You must answer with only an integer (use no other characters)."))
        }
        return(extracted)
      }
    ),
    
    validation_functions = list(
      function(x) {
        if (!is.null(min) && x < min) {
          return(create_llm_feedback(glue::glue(
            "The number should be greater than or equal to {min}."
          )))
        }
        if (!is.null(max) && x > max) {
          return(create_llm_feedback(glue::glue(
            "The number should be less than or equal to {max}."
          )))
        }

        return(TRUE)
      }
    )
  )

  return(c(prompt_list, list(new_wrap)))
}
```

They key difference between an extraction and validation function is
that an extraction function alters the LLM’s response and passes on the
altered response to next extraction and/or validation functions, and
eventually to the return statement of send_prompt (if extractions and
validations are succesful). A validation function, on the other hand,
only checks if the LLM’s response passes a logical test. Both extraction
and validation functions can return feedback to the LLM.

For more information, on what you can do with prompt wrappers, see the
documentation of the `prompt_wrap` class creator function:
`create_prompt_wrap`. For examples of prompt wrapper functions, see, for
instance the documentation and source code of `add_text`,
`answer_as_integer`, `set_mode_chainofthought`, and `add_tools`.

## More information and contributing

`tidyprompt` is under active development by Luka Koning
(<l.koning@kennispunttwente.nl>) and Tjark van de Merwe
(<t.vandemerwe@kennispunttwente.nl>). Note that in this stage, the
package is not yet fully stable and its architecture is subject to
change.

If you encounter issues, please open an issue in the GitHub repository.
You are welcome to contribute to the package by opening a pull request.
If you have any questions or suggestions, you can also reach us via
e-mail.
