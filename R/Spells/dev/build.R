# build file
library(here)
library(devtools)
pkg_path <- here("Spells","R","Spells")

document(pkg_path)
check(pkg_path)


devtools::install_local(here::here("Spells","R","Spells"), force = TRUE)

