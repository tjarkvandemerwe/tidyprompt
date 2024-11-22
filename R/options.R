options(tidyprompt.verbose = TRUE)

print_welcome <- function() {
  if (!requireNamespace("cli", quietly = TRUE)) {
    cat("--- Thank you for using 'tidyprompt'!\n")
    cat("Note that 'tidyprompt' is still under active development.\n")
    cat("You may download the latest version from github.com/tjarkvandemerwe/tidyprompt, using:\n")
    cat("  remotes::install_github(\"tjarkvandemerwe/tidyprompt\")\n")
    cat("Bugs, suggestions, questions, or contributions?\n")
    cat("  Open an issue or pull request on GitHub.\n")
  } else {
    cli::cli_h3(paste0(
      "Thank you for using '", cli::style_bold("tidyprompt"), "'!"
    ))
    cli::cli_text("Note that 'tidyprompt' is still under active development.")
    cli::cli_alert_info("You may download the latest version from {.href [GitHub](https://github.com/tjarkvandemerwe/tidyprompt)}, using:")
    cli::cli_text("{cli::symbol$arrow_right} {.run [remotes::install_github()](remotes::install_github(\"tjarkvandemerwe/tidyprompt\"))}")
    cli::cli_alert_info("Bugs, suggestions, questions, or contributions?")
    cli::cli_text(paste0(
      "{cli::symbol$arrow_right}  Open an",
      " {.href [issue](https://github.com/tjarkvandemerwe/tidyprompt/issues)}",
      " or {.href [pull request](https://github.com/tjarkvandemerwe/tidyprompt/pulls)}"
    ))
  }
  cat("\n")
}
print_welcome()
