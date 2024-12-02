
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyprompt

<!-- badges: start -->

[![R-CMD-check](https://github.com/tjarkvandemerwe/tidyprompt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tjarkvandemerwe/tidyprompt/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

‘tidyprompt’ is an R package to easily prompt large language models
(‘LLMs’) and enhance their functionality.

Key features of ‘tidyprompt’ are:

- **tidy prompting**: Efficiently construct prompts for LLMs, using
  piping syntax (inspired by the ‘tidyverse’). Wrap a base prompt in
  prompt wraps to influence how the LLM handles the prompt. A library of
  pre-built prompt wraps is included, but you can also write your own
  prompt wraps.

- **structured output**: Obtain structured output from a LLM, adhering
  to a specific type and/or format. Configure automatic retries with
  feedback to the LLM when the output is not as expected.

- **reasoning modes**: Make your LLM answer a prompt in a specific mode,
  such as chain-of-thought or ReAct (Reasoning and Acting) modes.

- **function calling**: Give your LLM the ability to autonomously call R
  functions (‘tools’). With this, the LLM can retrieve information or
  take other actions. ‘tidyprompt’ also supports R code generation and
  evaluation, allowing LLMs to run R code.

- **compatible with all LLM providers**: Usable with any LLM provider
  that supports chat completion. Use included LLM providers such as
  Ollama (local PC/on your own server), OpenAI, OpenRouter (offering
  various providers, including Anthropic), Mistral, Groq, XAI (Grok), or
  Google Gemini. Or write a hook for any other LLM provider.

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
structures.

In the first place, ‘tidyprompt’ aims to be suitable for all LLM
providers and models that support chat completion. This means that
prompt wraps can also enable advanced functionalities like prompt
wrapping and structured output for LLMs that do not natively support it.
Where appropriate, ‘tidyprompt’ may also support native processing;
currently, `answer_as_json()` also offers support for the respective
native options of the Ollama and OpenAI-type APIs (including supplying
JSON schemas). Users that use a provider whose API does not have this
functions, can still use `answer_as_json()` with text-based handling
(and it is easy to switch between providers with different levels of
JSON support). For `add_tools()`, ‘tidyprompt’ currently supports
text-based handling, but will soon also support native processing for
key API types (Ollama, OpenAI).

The philosophy behind ‘tidyprompt’ is furthermore that it aims to be
flexible enough that users can implement advanced features, potentially
specific to certain LLM providers, within the options of their custom
prompt wraps.
