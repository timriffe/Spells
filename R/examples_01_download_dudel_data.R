# download Dudel transition probabilities:
library(here)

url <- "https://static-content.springer.com/esm/art%3A10.1007%2Fs13524-017-0619-6/MediaObjects/13524_2017_619_MOESM2_ESM.zip"

download.file(url = url, destfile = here("Data","Dudel.zip"))
unzip(zipfile = here("Data","Dudel.zip"), 
      exdir = here("Data","Dudel"))
file.remove(here("Data","Dudel.zip"))




