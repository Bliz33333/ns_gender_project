library(pacman)
p_load(tidyverse, readxl)

start_year <- 2008
end_year <- 2023
num_years <- end_year - start_year +1




journals <- read_excel("journals.xlsx")

saveit <- function(..., string, file) {
  x <- list(...)
  names(x) <- string
  save(list=names(x), file=file, envir=list2env(x))
}
