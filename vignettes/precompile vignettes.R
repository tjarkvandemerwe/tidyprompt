here::i_am("vignettes/precompile vignettes.R")

precompile_vignettes <- function() {
  knitr::knit(
    here::here("vignettes", "getting_started.Rmd.orig"),
    output = here::here("vignettes", "getting_started.Rmd"),
    envir = new.env()
  )

  knitr::knit(
    here::here("vignettes", "sentiment_analysis.Rmd.orig"),
    output = here::here("vignettes", "sentiment_analysis.Rmd"),
    envir = new.env()
  )
}

precompile_vignettes()

# Include below answer_as_code chunk *after* compilation (in .Rmd):
# ```{r echo=FALSE, out.width=504, fig.alt="Plot of sentiment scores for each sentence"}
# if (file.exists(here::here("man", "figures", "answer_as_code1-1.png"))) {
#   knitr::include_graphics(here::here("man", "figures", "answer_as_code1-1.png"))
# } else if (file.exists("./man/figures/answer_as_code1-1.png")) {
#   knitr::include_graphics("./man/figures/answer_as_code1-1.png")
# } else if (file.exists("../man/figures/answer_as_code1-1.png")) {
#   knitr::include_graphics("../man/figures/answer_as_code1-1.png")
# }
# ```
