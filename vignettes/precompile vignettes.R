here::i_am("vignettes/precompile vignettes.R")

precompile_vignettes <- function() {
  knitr::knit(
    here::here("vignettes", "getting_started.Rmd.orig"),
    output = here::here("vignettes", "getting_started.Rmd")
  )
}
