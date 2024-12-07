
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyprompt

<!-- badges: start -->

[![R-CMD-check](https://github.com/tjarkvandemerwe/tidyprompt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tjarkvandemerwe/tidyprompt/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

‘tidyprompt’ is an R package to easily prompt large language models
(‘LLMs’) and enhance their functionality.

Key features of ‘tidyprompt’ are:

- **tidy prompting**: Easily construct prompts for LLMs, using piping
  syntax (inspired by the ‘tidyverse’). Prompt wraps are building blocks
  with which you can quickly create advanced prompts, simultaneously
  adding extraction and validation functions for processing the LLM
  output. A library of pre-built prompt wraps is included, but you can
  also write custom prompt wraps.

- **structured output**: Obtain structured output from a LLM, adhering
  to a specific type and/or format.

- **feedback & retries**: Automatically provide feedback to the LLM when
  the output is not as expected, so that it can retry.

- **reasoning modes**: Make your LLM answer a prompt in a specific mode,
  such as chain-of-thought or ReAct (Reasoning and Acting) modes.

- **function calling**: Give your LLM the ability to autonomously call R
  functions (‘tools’). With this, the LLM can retrieve information or
  take other actions. ‘tidyprompt’ also supports R code generation and
  evaluation, allowing LLMs to run R code.

- **compatible with all LLM providers**: All features of ‘tidyprompt’
  are designed to be provider-agnostic, meaning that they can be used
  with any LLM provider that supports chat completion. ‘tidyprompt’
  includes various default LLM providers, including Ollama, OpenAI,
  OpenRouter (offering various providers, including Anthropic), Mistral,
  Groq, XAI (Grok), and Google Gemini. You can also write a hook for any
  other LLM provider.

## Installation

You can install the development version of tidyprompt from
[GitHub](https://github.com/tjarkvandemerwe/tidyprompt) with:

``` r
# install.packages("remotes")
remotes::install_github("tjarkvandemerwe/tidyprompt")
```

## Getting started

See the [‘Getting
started’](https://tjarkvandemerwe.github.io/tidyprompt/articles/getting_started.html)
vignette for an introduction to ‘tidyprompt’.

This vignette shows example usage and guides you through the basic
functionalities of ‘tidyprompt’, demonstrating how to use and create
prompt wraps and send prompts to LLMs.

## More information and contributing

‘tidyprompt’ is under active development by Luka Koning
(<l.koning@kennispunttwente.nl>) and Tjark van de Merwe
(<t.vandemerwe@kennispunttwente.nl>). Note that in this stage, the
package is not yet fully stable and its architecture is subject to
change.

If you encounter issues, please open an issue in the GitHub repository.
You are welcome to contribute to the package by opening a pull request.
If you have any questions or suggestions, you can also reach us via
e-mail.

### Philosophy of ‘tidyprompt’: text-based versus native handling of LLMs

‘tidyprompt’ is primarily focused on ‘text-based’ handling of LLMs,
where textual output is parsed to achieve structured output and other
functionalities. Several LLM providers and models also offers forms of
‘native’ handling, where the LLM is directly controlled by the LLM
provider to provide output in a certain manner.

In the first place, ‘tidyprompt’ aims to be suitable for all LLM
providers and models that support chat completion. This means that
prompt wraps can also enable advanced functionalities like structured
output and function calling for LLMs that do not natively support it.
Where appropriate, ‘tidyprompt’ may also support native configuration of
specific APIs.

Currently, `answer_as_json()` and `answer_using_tools()` offer native
support for adhering to JSON schemas and calling functions. Native
handling may be powerful in some cases, but restrictive in other cases.
It is good to test what works best for your use case. When combining
multiple prompt wraps, ‘text-based’ handling appears to be generally
more flexible and robust.

The philosophy behind ‘tidyprompt’ is furthermore that it aims to be
flexible enough that users can implement advanced features, potentially
specific to certain LLM providers, within the options of their custom
prompt wraps. This way, ‘tidyprompt’ can be a powerful tool for a wide
range of use cases, without focusing too much on maintaining a large
number of provider-specific features.
