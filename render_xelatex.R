
render_xelatex <- function(file = "st1gombintex") {
  rmarkdown::render(paste0(file, ".Rmd"))
  system(paste0("xelatex --interaction=nonstopmode ", file, ".tex"))
  system(paste0("biber ", file))
  system(paste0("xelatex --interaction=nonstopmode ", file, ".tex"))
  system(paste0("xelatex --interaction=nonstopmode ", file, ".tex"))
}