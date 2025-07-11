% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidyprompt.R
\name{set_system_prompt}
\alias{set_system_prompt}
\title{Set system prompt of a \linkS4class{tidyprompt} object}
\usage{
set_system_prompt(prompt, system_prompt)
}
\arguments{
\item{prompt}{A single string or a \code{\link[=tidyprompt]{tidyprompt()}} object}

\item{system_prompt}{A single character string representing the system prompt}
}
\value{
A \code{\link[=tidyprompt]{tidyprompt()}} with the system prompt set
}
\description{
Set the system prompt for a prompt. The system prompt will be added
as a message with role 'system' at the start of the chat history when
this prompt is evaluated by \code{\link[=send_prompt]{send_prompt()}}.
}
\details{
The system prompt will be stored in the \code{\link[=tidyprompt]{tidyprompt()}} object
as '$system_prompt'.
}
\examples{
prompt <- "Hi there!" |>
  set_system_prompt("You are an assistant who always answers in very short poems.")
prompt$system_prompt

\dontrun{
  prompt |>
    send_prompt(llm_provider_ollama())
  # --- Sending request to LLM provider (llama3.1:8b): ---
  #   Hi there!
  # --- Receiving response from LLM provider: ---
  #   Hello to you, I say,
  #   Welcome here, come what may!
  #   How can I assist today?
  # [1] "Hello to you, I say,\nWelcome here, come what may!\nHow can I assist today?"
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
\code{\link{quit_if}()}

Other miscellaneous_prompt_wraps: 
\code{\link{add_text}()},
\code{\link{quit_if}()}
}
\concept{miscellaneous_prompt_wraps}
\concept{pre_built_prompt_wraps}
