
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyprompt

<!-- badges: start -->

[![R-CMD-check](https://github.com/tjarkvandemerwe/tidyprompt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tjarkvandemerwe/tidyprompt/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

‘tidyprompt’ is an R package to prompt and empower your large language
models (LLMs), the tidy way.

Key features of ‘tidyprompt’ are:

- **tidy prompting**: Quickly and elegantly construct prompts for LLMs,
  using piping syntax (inspired by the ‘tidyverse’). Wrap a base prompt
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
  take other actions. ‘tidyprompt’ also supports R code generation and
  evaluation, allowing LLMs to run R code.

- **compatible with all LLM providers**: Usable with any LLM provider
  that supports chat completion. Use included LLM providers such as
  Ollama (local PC/on your own server), OpenAI, OpenRouter (offering
  various providers, including Anthropic), Mistral, Groq, XAI (Grok), or
  Google Gemini. Or easily write your own hook for any other LLM
  provider.

‘tidyprompt’ is primarily based on [‘text-based’
(vs. ‘native’)](#a-note-on-text-based-versus-native-handling-of-llms)
handling of LLMs. This offers a more general approach compared to
‘native’ handling, the latter being more specific to certain LLM
providers, models, and API structures. Features to support ‘native’
handling may nonetheless be added in the future.

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

### A note on ‘text-based’ versus ‘native’ handling of LLMs

‘tidyprompt’ is primarily focused on ‘text-based’ handling of LLMs,
where textual output is parsed to achieve structured output and other
functionalities. This is in contrast to ‘native’ handling, where the LLM
is directly controlled by the LLM provider to provide output in a
certain manner (for instance, the available tokens are restricted to
match a specific output format). While the latter may be more efficient,
it tends to be more specific to LLM providers, certain models, and API
structures. With ‘text-based’ processing, ‘tidyprompt’ aims to be more
general and suitable for a wider range of LLM providers and models.
Nonetheless, ‘tidyprompt’ may also include support for ‘native’
processing in the future.
