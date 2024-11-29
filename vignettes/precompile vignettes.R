here::i_am("vignettes/precompile vignettes.R")

precompile_vignettes <- function() {
  knitr::knit(
    here::here("vignettes", "getting_started.Rmd.orig"),
    output = here::here("vignettes", "getting_started.Rmd"),
    envir = new.env()
  )
}

precompile_vignettes()

# Include below answer_as_code chunk *after* compilation (in .Rmd):
# ```{r echo=FALSE, out.width=504}
# if (file.exists(here::here("figure", "answer_as_code1-1.png"))) {
#   knitr::include_graphics(here::here("figure", "answer_as_code1-1.png"))
# } else if (file.exists("./figure/answer_as_code1-1.jpg")) {
#   knitr::include_graphics("./figure/answer_as_code1-1.jpg")
# } else if (file.exists("../figure/answer_as_code1-1.jpeg")) {
#   knitr::include_graphics("../figure/answer_as_code1-1.jpeg")
# }
# ```
