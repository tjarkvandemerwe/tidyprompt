% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/llm_provider.R
\name{create_fake_llm_provider}
\alias{create_fake_llm_provider}
\title{Create a fake LLM provider (for development and testing purposes)}
\usage{
create_fake_llm_provider()
}
\value{
A new llm_provider object for use of the fake LLM provider
}
\description{
This function creates a fake LLM provider that can be used for development
and testing purposes. It is hardcoded to send back specific responses to
specific prompts that are used in vignettes, tests, and examples.
This is useful for running tests and builds in environments in which an
actual LLM provider is not available.
}
