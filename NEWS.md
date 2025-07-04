# tidyprompt (development version)

* Added new prompt wraps: `answer_as_category()` & `answer_as_multi_category()`
* Added `llm_break_soft()`, which can be used to interrupt prompt evaluation
without causing an error
* Safer handling of chat history objects ('NA' rows could cause errors in specific cases)
* Fixed default Google Gemini LLM provider
* More flexible lenient extraction for final answer extraction in chain-of-thought
* Use `message()` instead of `cat()` for printing LLM responses
* Added 'num_ctx' parameter for Ollama LLM provider to more easily control
the context window size
* Added `set_option()` & ``set_options()` to Ollama LLM provider to 
more easily set options
* Added a more detailed error message in case a LLM provider cannot be reached

# tidyprompt 0.0.1

* Initial CRAN release

# tidyprompt 0.0.0.9000

* Initial development version available on GitHub
