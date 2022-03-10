# build file
library(here)
library(devtools)
pkg_path <- here("R","Spells")

document(pkg_path)
check(pkg_path)


devtools::install_local(here::here("R","Spells"), force = TRUE)
load_all(here::here("R","Spells"))
