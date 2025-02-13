
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyprompt

<!-- badges: start -->

[![R-CMD-check](https://github.com/tjarkvandemerwe/tidyprompt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tjarkvandemerwe/tidyprompt/actions/workflows/R-CMD-check.yaml)
[![](https://www.r-pkg.org/badges/version/tidyprompt)](https://cran.r-project.org/package=tidyprompt)
[![](https://img.shields.io/badge/devel%20version-0.0.1.9000-blue.svg)](https://github.com/tjarkvandemerwe/tidyprompt)
<!-- badges: end -->

‘tidyprompt’ is an R package to easily construct prompts and associated
logic for interacting with large language models (‘LLMs’).

Think of ‘tidyprompt’ as the ‘ggplot2’ package for creating prompts and
handling LLM interactions. ‘tidyprompt’ introduces the concept of prompt
wraps, which are building blocks that you can use to quickly turn a
simple prompt into an advanced one. Prompt wraps do not just modify the
prompt text, but also add extraction and validation functions that will
be applied to the response of a LLM. Moreover, these functions can send
feedback to the LLM.

With ‘tidyprompt’ and prompt wraps, you can add various features to your
prompts and define how they are evaluated by LLMs. For example:

- **structured output**: Obtain structured output from a LLM, adhering
  to a specific type and/or format. Use pre-built prompt wraps or your
  own R code to validate.

- **feedback & retries**: Automatically provide feedback to a LLM when
  the output is not as expected, allowing the LLM to retry their answer.

- **reasoning modes**: Make a LLM answer a prompt in a specific mode,
  such as chain-of-thought or ReAct (Reasoning and Acting).

- **function calling**: Give a LLM the ability to autonomously call R
  functions (‘tools’). With this, the LLM can retrieve information or
  take other actions. ‘tidyprompt’ also supports R code generation and
  evaluation, allowing LLMs to run R code.

## Installation

Install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("tjarkvandemerwe/tidyprompt")
```

Or install from CRAN (0.0.1):

``` r
install.packages("tidyprompt")
```

## Getting started

See the [‘Getting
started’](https://tjarkvandemerwe.github.io/tidyprompt/articles/getting_started.html)
vignette for a detailed introduction to using ‘tidyprompt’.

## Examples

Here are some quick examples of what you can do with ‘tidyprompt’:

``` r
"What is 5+5?" |>
  answer_as_integer() |>
  send_prompt(llm_provider_ollama())
#> [1] 10
```

``` r
"Are you a large language model?" |>
  answer_as_boolean() |>
  send_prompt(llm_provider_ollama())
#> [1] TRUE
```

``` r
"What animal is the biggest?" |>
  answer_as_regex_match("^(cat|dog|elephant)$") |>
  send_prompt(llm_provider_ollama())
#> [1] "elephant"
```

``` r
# Make LLM use a function from an R package to search Wikipedia for the answer
"What is something fun that happened in November 2024?" |>
  answer_as_text(max_words = 25) |>
  answer_using_tools(getwiki::search_wiki) |>
  send_prompt(llm_provider_ollama())
#> [1] "The 2024 ARIA Music Awards ceremony, a vibrant celebration of Australian music,
#> took place on November 20, 2024."
```

``` r
# From prompt to linear model object in R
model <- paste0(
  "Using my data, create a statistical model",
  " investigating the relationship between two variables."
) |>
  answer_using_r(
    objects_to_use = list(data = cars),
    evaluate_code = TRUE,
    return_mode = "object"
  ) |>
  prompt_wrap(
    validation_fn = function(x) {
      if (!inherits(x, "lm"))
        return(llm_feedback("The output should be a linear model object."))
      return(TRUE)
    }
  ) |>
  send_prompt(llm_provider_ollama())
summary(model)
#> Call:
#> lm(formula = speed ~ dist, data = data)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -7.5293 -2.1550  0.3615  2.4377  6.4179 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  8.28391    0.87438   9.474 1.44e-12 ***
#> dist         0.16557    0.01749   9.464 1.49e-12 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 3.156 on 48 degrees of freedom
#> Multiple R-squared:  0.6511, Adjusted R-squared:  0.6438 
#> F-statistic: 89.57 on 1 and 48 DF,  p-value: 1.49e-12
```

``` r
# Escape validation on questions that cannot be answered
"How many years old is my neighbour's dog?" |>
  answer_as_integer() |>
  quit_if() |>
  send_prompt(llm_provider_ollama())
#> NULL
```

``` r
# LLM in the loop; 
#   LLM verifies answer of LLM and can provide feedback
"What is the capital of France?" |>
  llm_verify() |>
  send_prompt(llm_provider_ollama())
#> ...
  
# Human in the loop; 
#   user verifies answer of LLM and can provide feedback
"What is the capital of France?" |>
  user_verify() |>
  send_prompt(llm_provider_ollama())
#> ...
```

## More information and contributing

‘tidyprompt’ is developed by Luka Koning
(<l.koning@kennispunttwente.nl>) and Tjark van de Merwe
(<t.vandemerwe@kennispunttwente.nl>).

If you encounter issues, have questions, or have suggestions, please
open an issue in the GitHub repository. You are also welcome to
contribute to the package by opening a pull request.

### Why ‘tidyprompt’?

We designed ‘tidyprompt’ because we found ourselves writing code
repeatedly to both construct prompts and handle the associated output of
LLMs; these tasks were intertwined. Often times, we also wanted to add
features to our prompts, or take them away, which required us to rewrite
a lot of code. Thus, we wanted to have building blocks with which we
could easily construct prompts and simultaneously add code to handle the
output of LLMs. This led us to a design inspired by piping syntax, as
popularized by the ‘tidyverse’ and familiar to many R users.

‘tidyprompt’ should be seen as a tool which can be used to enhance the
functionality of LLMs beyond what APIs natively offer. It is designed to
be flexible and provider-agnostic, so that its features can be used with
a wide range of LLM providers and models.

‘tidyprompt’ is primarily focused on ‘text-based’ handling of LLMs,
where textual output is parsed to achieve structured output and other
functionalities. Several LLM providers and models also offers forms of
‘native’ handling, where the LLM is directly controlled by the LLM
provider to provide output in a certain manner.

Where appropriate, ‘tidyprompt’ may also support native configuration of
specific APIs. Currently, `answer_as_json()` and `answer_using_tools()`
offer native support for adhering to JSON schemas and calling functions.
Native handling may be powerful in some cases, but restrictive in other
cases. It is good to test what works best for your use case. Note also
that prompt wraps may extend what is enforced by native handling, such
as adding additional validation or feedback.

The philosophy behind ‘tidyprompt’ is furthermore that it aims to be
flexible enough that users can implement advanced features, potentially
specific to certain LLM providers, within the options of their custom
prompt wraps. This way, ‘tidyprompt’ can be a powerful tool for a wide
range of use cases, without focusing on maintaining provider-specific
features.

#### ‘tidyprompt’ versus ‘ellmer’ & ‘tidyllm’

In line with the above, ‘tidyprompt’ is less focused on interfacing with
the APIs of various LLM providers, like R packages ‘ellmer’ and
‘tidyllm’ do. Instead, ‘tidyprompt’ is primarily focused on offering a
framework for constructing prompts and associated logic for interactions
with LLMs.

We aim to design ‘tidyprompt’ in such a way that it may be compatible
with ‘ellmer’, ‘tidyllm’, and any other packages offering an interface
to LLM APIs. We are open to feedback on our design and may include
compatibility with specific features from these packages in the future.
