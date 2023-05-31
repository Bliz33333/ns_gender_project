library(pacman)
p_load(tidyverse, readxl)

start_year <- 2008
end_year <- 2023
num_years <- end_year - start_year +1

if(!dir.exists("./data"))
{
  dir.create("./data")
}
if(!dir.exists("./data/yearly"))
{
  dir.create("./data/yearly")
}

journals <- read_excel("journals.xlsx")

saveit <- function(..., string, file) {
  x <- list(...)
  names(x) <- string
  save(list=names(x), file=file, envir=list2env(x))
}


# TODO: 
# two affiliations contain interrobangs
# resolve first and last author selection in the case of collective names first or last