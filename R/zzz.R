# Suppress warning about undefined variables which are passed between environments
#   and are not actually undefined
utils::globalVariables(c("self", "schema_instruction", ".data"))

# Print welcome message
cli::cli_h3(
  paste0(
    "Thank you for using '",
    cli::style_bold("tidyprompt"),
    "'!"
  )
)
cli::cli_text("Note that 'tidyprompt' is still under active development.")
cli::cli_alert_info(
  "You may download the latest version from {.href [GitHub](https://github.com/tjarkvandemerwe/tidyprompt)}, using:"
)
cli::cli_text(
  "{cli::symbol$arrow_right} {.run [remotes::install_github()](remotes::install_github(\"tjarkvandemerwe/tidyprompt\"))}"
)
cli::cli_alert_info("Bugs, suggestions, questions, or contributions?")
cli::cli_text(
  paste0(
    "{cli::symbol$arrow_right}  Open an",
    " {.href [issue](https://github.com/tjarkvandemerwe/tidyprompt/issues)}",
    " or {.href [pull request](https://github.com/tjarkvandemerwe/tidyprompt/pulls)}"
  )
)
cat("\n")
