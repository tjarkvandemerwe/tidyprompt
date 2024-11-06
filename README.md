
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyprompt

<!-- badges: start -->

[![R-CMD-check](https://github.com/tjarkvandemerwe/tidyprompt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tjarkvandemerwe/tidyprompt/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`tidyprompt` is an R package to prompt and empower your large language
models (LLMs), the tidy way.

Key features of `tidyprompt` are:

- **tidy prompting**: Quickly and elegantly construct prompts for LLMs,
  using piping syntax (inspired by the `tidyverse`). Wrap a base prompt
  in prompt wraps to influence how the LLM handles the prompt. A library
  of pre-built prompt wraps is included, but you can also write your
  own.

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
  Ollama (local PC/on your own server), OpenAI, OpenRouter (offering
  various providers), Mistral, Groq, XAI (Grok), or Google Gemini. Or
  easily write your own hook for any other LLM provider.

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
various LLM providers, such as Ollama, OpenAI, OpenRouter, Mistral,
Groq, XAI (Grok), and Google Gemini.

With the `llm_provider` function, you can easily write a hook for any
other LLM provider. You could make API calls using the `httr` package or
use another R package that already has a hook for the LLM provider you
want to use. If your API of choice follows the structure of the OpenAI
API, you can very easily call the `llm_provider_openai` function and
change the relevant parameters (like the url and the API key).

``` r
# Ollama running on local PC
ollama <- llm_provider_ollama(
  parameters = list(model = "llama3.1:8b"),
)

# OpenAI API
openai <- llm_provider_openai(
  parameters = list(model = "gpt-4o-mini")
)

# Various providers via OpenRouter (e.g., Anthropic)
openrouter <- llm_provider_openrouter(
  parameters = list(model = "anthropic/claude-3.5-sonnet")
)

# ... functions also included for Mistral, Groq, XAI (Grok), and Google Gemini

# ... or easily create your own hook for any other LLM provider;
#   see ?llm_provider for more information; also take a look at the source code of
#   llm_provider_ollama() and llm_provider_openai(). For APIs that follow the structure
#   of the OpenAI API for chat completion, you can use llm_provider_openai() and change
#   the relevant parameters (like the url and the API key).
```

### Basic prompting

A simple string serves as the base for a prompt.

By adding prompt wraps, you can influence various aspects of how the LLM
handles the prompt, while verifying that the output is structured and
valid (including retries with feedback to the LLM if it is not).

``` r
  "Hi there!" |>
    send_prompt(ollama)
#> --- Sending request to LLM provider (llama3.1:8b): ---
#> Hi there!
#> --- Receiving response from LLM provider: ---
#> It's nice to meet you. Is there something I can help you with or would you like to chat?
#> [1] "It's nice to meet you. Is there something I can help you with or would you like to chat?"
```

`add_text` is a simple example of a prompt wrap. It simply adds some
text at the end of the base prompt.

``` r
  "Hi there!" |>
    add_text("What is a large language model? Explain in 10 words.") |>
    send_prompt(ollama)
#> --- Sending request to LLM provider (llama3.1:8b): ---
#> Hi there!
#> 
#> What is a large language model? Explain in 10 words.
#> --- Receiving response from LLM provider: ---
#> Advanced computer program that understands and generates human-like written text.
#> [1] "Advanced computer program that understands and generates human-like written text."
```

You can also construct the final prompt text, without sending it to an
LLM provider.

``` r
  "Hi there!" |>
    add_text("What is a large language model? Explain in 10 words.")
#> <tidyprompt>
#> The base prompt is modified by a wrapper function, resulting in:
#> > Hi there!
#> > 
#> > What is a large language model? Explain in 10 words. 
#> Use '<tidyprompt>$prompt_wraps' to show the wrapper functions.
#> Use '<tidyprompt>$base_prompt' to show the base prompt text.
#> Use '<tidyprompt> |> construct_prompt_text()' to show the full prompt text.
```

### Retrieving output in a specific format

Using prompt wraps, you can force the LLM to return the output in a
specific format. You can also extract the output to turn it from a
character into another data type.

For instance, the `answer_as_integer` prompt wrap will force the LLM to
return an integer.

To achieve this, the prompt wrap will add some text to the base prompt,
asking the LLM to reply with an integer. However, the prompt wrap does
more: it also will attempt to extract and validate the integer from the
LLM’s response. If extraction or validation fails, feedback is sent back
to the LLM, after which the LLM can retry answering the prompt.

``` r
  "What is 2 + 2?" |>
    answer_as_integer() |>
    send_prompt(ollama)
#> --- Sending request to LLM provider (llama3.1:8b): ---
#> What is 2 + 2?
#> 
#> You must answer with only an integer (use no other characters).
#> --- Receiving response from LLM provider: ---
#> 4
#> [1] 4
```

Below is an example of a prompt which will initially fail, but will
succeed after a retry.

``` r
  "What is 2 + 2?" |>
    add_text("Please write out your reply in words, use no numbers.") |>
    answer_as_integer(add_instruction_to_prompt = FALSE) |>
    send_prompt(ollama)
#> --- Sending request to LLM provider (llama3.1:8b): ---
#> What is 2 + 2?
#> 
#> Please write out your reply in words, use no numbers.
#> --- Receiving response from LLM provider: ---
#> Two plus two equals four.
#> --- Sending request to LLM provider (llama3.1:8b): ---
#> You must answer with only an integer (use no other characters).
#> --- Receiving response from LLM provider: ---
#> 4
#> [1] 4
```

### Adding a reasoning mode to the LLM

Prompt wraps may also be used to add a reasoning mode to the LLM. It is
hypothesized that this could improve the LLM’s performance on more
complex tasks.

For instance, function `answer_by_chain_of_thought` will add chain of
thought reasoning mode to the LLM. This wraps the base prompt within a
request for the LLM to reason step by step, asking it to provide the
final answer within ‘FINISH\[<final answer here>\]’. An extraction
function then ensures only the final answer is returned.

``` r
  "What is 2 + 2?" |>
    answer_by_chain_of_thought() |>
    answer_as_integer() |>
    send_prompt(ollama)
#> --- Sending request to LLM provider (llama3.1:8b): ---
#> You are given a user's prompt.
#> To answer the user's prompt, you need to think step by step to arrive at a final answer.
#> 
#> ----- START OF USER'S PROMPT -----
#> What is 2 + 2?
#> 
#> You must answer with only an integer (use no other characters).
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
#> --- Receiving response from LLM provider: ---
#> >> step 1: Identify the mathematical operation requested in the prompt, which is addition.
#> The prompt asks for the sum of 2 and 2.
#> 
#> >> step 2: Recall the basic arithmetic fact that 2 + 2 equals a specific number.
#> This is a fundamental math concept that can be recalled from memory or learned through experience.
#> 
#> >> step 3: Apply this knowledge to determine that the sum of 2 and 2 is indeed 4.
#> The numerical value of 4 is derived directly from knowing that 2 added to itself results in a total count of four items, quantities, or values.
#> 
#> FINISH4
#> --- Sending request to LLM provider (llama3.1:8b): ---
#> Error, could not parse your final answer.
#> Please type: 'FINISH[<put here your final answer to the original prompt>]'
#> --- Receiving response from LLM provider: ---
#> FINISH[4]
#> [1] 4
```

### Giving tools to the LLM (autonomous function-calling)

With `tidyprompt` and the `add_tools` prompt wrap, you can define your
own R functions and give the LLM the ability to call them. This enables
the LLM to retrieve additional information or take other actions.

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
    send_prompt(ollama)
#> --- Sending request to LLM provider (llama3.1:8b): ---
#> Hi, what is the weather temperature in Enschede?
#> 
#> I want to know the Celcius degrees.
#> 
#> You must answer with only an integer (use no other characters).
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
#> --- Receiving response from LLM provider: ---
#> I'll call the `temperature_in_location` function with the necessary arguments.
#> 
#> FUNCTION[temperature_in_location]("Enschede", "Celcius")
#> --- Sending request to LLM provider (llama3.1:8b): ---
#> function called: temperature_in_location
#> arguments used: location = Enschede, unit = Celcius
#> result: 22.7
#> --- Receiving response from LLM provider: ---
#> The current temperature in Enschede is 22.7°C.
#> --- Sending request to LLM provider (llama3.1:8b): ---
#> You must answer with only an integer (use no other characters).
#> --- Receiving response from LLM provider: ---
#> 22
#> [1] 22
```

### Code generation and evaluation

`answer_as_code` is a more advanced prompt wrap, which has various
options to enable LLM code generation. R code can be extracted, parsed
for validity, and optionally be evaluated in a dedicated R session
(using the `callr` package). The prompt wrap can also be set to ‘tool
mode’ (with `output_as_tool = TRUE`), where the output of R code is
returned to the LLM, so that it can be used to formulate a final answer.

``` r
# From prompt to ggplot
plot <- paste0(
  "Create a scatter plot of miles per gallon (mpg) versus",
  " horsepower (hp) for the cars in the mtcars dataset.",
  " Use different colors to represent the number of cylinders (cyl).",
  " Make the plot nice and readable,",
  " but also be creative, a little crazy, and have humour!"
) |>
  answer_as_code(
    pkgs_to_use = c("ggplot2"),
    evaluate_code = TRUE,
    return_mode = "object"
  ) |>
  send_prompt(openai)
plot
```

![](figure/answer_as_code1-1.png)

### Creating your own prompt wraps

Under the hood, prompts are just lists of a base prompt (a string) and a
series of prompt wraps.

You can thus create a function which takes a prompt and appends a new
prompt wrap to it.

Take a look at the source code for function `add_text`:

``` r
add_text <- function(
    prompt,
    text, position = c("after", "before"), sep = "\n\n"
) {
  position <- match.arg(position)

  modify_fn <- function(original_prompt_text) {
    if (position == "after") {
      paste(original_prompt_text, text, sep = sep)
    } else {
      paste(text, original_prompt_text, sep = sep)
    }
  }

  prompt_wrap(prompt, modify_fn)
}
```

More complex prompt wraps may also add extraction and validation
functions. Take a look at the source code for function
`answer_as_integer`:

``` r
answer_as_integer <- function(
    prompt,
    min = NULL,
    max = NULL,
    add_instruction_to_prompt = TRUE
) {
  instruction <- "You must answer with only an integer (use no other characters)."

  if (!is.null(min) && !is.null(max)) {
    instruction <- paste(instruction, glue::glue("Enter an integer between {min} and {max}."))
  } else if (!is.null(min)) {
    instruction <- paste(instruction, glue::glue("Enter an integer greater than or equal to {min}."))
  } else if (!is.null(max)) {
    instruction <- paste(instruction, glue::glue("Enter an integer less than or equal to {max}."))
  }


  # Define modification/extraction/validation functions:
  modify_fn <- function(original_prompt_text) {
    if (!add_instruction_to_prompt) {
      return(original_prompt_text)
    }

    glue::glue("{original_prompt_text}\n\n{instruction}")
  }

  extraction_fn <- function(x) {
    extracted <- suppressWarnings(as.integer(x))
    if (is.na(extracted)) {
      return(create_llm_feedback(instruction))
    }
    return(extracted)
  }

  validation_fn <- function(x) {
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

  prompt_wrap(prompt, modify_fn, extraction_fn, validation_fn)
}
```

They key difference between an extraction and validation function is
that an extraction function alters the LLM’s response and passes on the
altered response to next extraction and/or validation functions, and
eventually to the return statement of send_prompt (if extractions and
validations are succesful). A validation function, on the other hand,
only checks if the LLM’s response passes a logical test. Both extraction
and validation functions can return feedback to the LLM.

For more information, on what you can do with prompt wraps, see the
documentation of the `prompt_wrap` class creator function:
`create_prompt_wrap`. For examples of prompt wrap functions, see, for
instance the documentation and source code of `add_text`,
`answer_as_integer`, `answer_by_chain_of_thought`, and `add_tools`.

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
