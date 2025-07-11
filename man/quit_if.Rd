% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_quit_if.R
\name{quit_if}
\alias{quit_if}
\title{Make evaluation of a prompt stop if LLM gives a specific response}
\usage{
quit_if(
  prompt,
  quit_detect_regex = "NO ANSWER",
  instruction =
    paste0("If you think that you cannot provide a valid answer, you must type:\\n",
    "'NO ANSWER' (use no other characters)"),
  success = TRUE,
  response_result = c("null", "llm_response", "regex_match")
)
}
\arguments{
\item{prompt}{A single string or a \code{\link[=tidyprompt]{tidyprompt()}} object}

\item{quit_detect_regex}{A regular expression to detect in the LLM's
response which will cause the evaluation to stop. The default
will detect the string "NO ANSWER" in the response}

\item{instruction}{A string to be added to the prompt to instruct the LLM
how to respond if it cannot answer the prompt. The default is
"If you think that you cannot provide a valid answer, you must type: 'NO ANSWER' (use no other characters)".
This parameter can be set to \code{NULL} if no instruction is needed in the prompt}

\item{success}{A logical indicating whether the \code{\link[=send_prompt]{send_prompt()}} loop break
should nonetheless be considered as a successful completion of the
extraction and validation process. If \code{FALSE}, the \code{object_to_return} must
will always be set to NULL and thus parameter 'response_result' must also
be set to 'null'; if \code{FALSE}, \code{\link[=send_prompt]{send_prompt()}} will also print a warning
about the unsuccessful evaluation. If \code{TRUE}, the \code{object_to_return} will be
returned as the response result of \code{\link[=send_prompt]{send_prompt()}} (and \code{\link[=send_prompt]{send_prompt()}}
will print no warning about unsuccessful evaluation); parameter 'response_result'
will then determine what is returned as the response result of \code{\link[=send_prompt]{send_prompt()}}.}

\item{response_result}{A character string indicating what should be returned
when the quit_detect_regex is detected in the LLM's response. The default is
'null', which will return NULL as the response result o f \code{\link[=send_prompt]{send_prompt()}}.
Under 'llm_response', the full LLM response will be returned as the
response result of \code{\link[=send_prompt]{send_prompt()}}.
Under 'regex_match', the part of the LLM response that matches the
quit_detect_regex will be returned as the response result of \code{\link[=send_prompt]{send_prompt()}}}
}
\value{
A \code{\link[=tidyprompt]{tidyprompt()}} with an added \code{\link[=prompt_wrap]{prompt_wrap()}} which will ensure
that the evaluation will stop upon detection of the quit_detect_regex in the
LLM's response
}
\description{
This function is used to wrap a \code{\link[=tidyprompt]{tidyprompt()}} object and ensure that the
evaluation will stop if the LLM says it cannot answer the prompt. This is
useful in scenarios where it is determined the LLM is unable to provide a
response to a prompt.
}
\examples{
\dontrun{
  "What the favourite food of my cat on Thursday mornings?" |>
    quit_if() |>
    send_prompt(llm_provider_ollama())
  # --- Sending request to LLM provider (llama3.1:8b): ---
  #   What the favourite food of my cat on Thursday mornings?
  #
  #   If you think that you cannot provide a valid answer, you must type:
  #   'NO ANSWER' (use no other characters)
  # --- Receiving response from LLM provider: ---
  #   NO ANSWER
  # NULL
}
}
\seealso{
Other pre_built_prompt_wraps: 
\code{\link{add_text}()},
\code{\link{answer_as_boolean}()},
\code{\link{answer_as_category}()},
\code{\link{answer_as_integer}()},
\code{\link{answer_as_json}()},
\code{\link{answer_as_list}()},
\code{\link{answer_as_multi_category}()},
\code{\link{answer_as_named_list}()},
\code{\link{answer_as_regex_match}()},
\code{\link{answer_as_text}()},
\code{\link{answer_by_chain_of_thought}()},
\code{\link{answer_by_react}()},
\code{\link{answer_using_r}()},
\code{\link{answer_using_sql}()},
\code{\link{answer_using_tools}()},
\code{\link{prompt_wrap}()},
\code{\link{set_system_prompt}()}

Other miscellaneous_prompt_wraps: 
\code{\link{add_text}()},
\code{\link{set_system_prompt}()}
}
\concept{miscellaneous_prompt_wraps}
\concept{pre_built_prompt_wraps}
