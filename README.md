
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyprompt

<!-- badges: start -->
<!-- badges: end -->

The goal of tidyprompt is to create llm prompting pipelines in a
structured and tidy manner.

## Installation

You can install the development version of tidyprompt from
[GitHub](https://github.com/tjarkvandemerwe/tidyprompt) with:

``` r
# install.packages("devtools")
devtools::install_github("tjarkvandemerwe/tidyprompt")
```

## Example

An basic example of prompting, defining the output structure, validating
the output, wrapping the prompt inside other prompts, retry with
feedback to the llm:

``` r
library(tidyprompt)

# holiday_suggestions <- 
#   prompt("Can you give me five possible holiday locations") |>
#   reply_as_list(name = "locations",
#                 element_name = "location") |>
#   validate(are_cities) |> 
#   add_prepromt("You are a helpful assistant") |> 
#   reason_method("chain of thought") |> 
#   send_prompt(retry = 5)
```
