# Tests fertility
library(here)
library(tidyverse)
data_path <- here("Spells","Data","Castro","cas_wom_1000seqs.RData")

# df:         original datasets (variables are described in object vars)
# dsets:      list of original files
# m_births:   women x age matrix with 1 for births, 0 for no births and NA for unobserved
# m_orders:   women x age matrix indicating birth orders, NA for unobserved
# m_parity:   women x age matrix indicating parity (i.e., children even born), NA for unobs
# m_unions:   women x age matrix for marital status
# 
# vars:       description of variables in df
load(data_path)


cumsum_na <- function(x,na.rm = TRUE){
  na_ind      <- is.na(x)
  x[na_ind]   <- 0
  xcs         <- cumsum(x)
  xcs[na_ind] <- NA
  xcs
}

# m_parity derived from / relates to m_births
apply(m_parity,1,cumsum_na,na.rm=TRUE) %>% 
  t() %>% 
  rowMeans(na.rm=TRUE) %>% 
  plot(x = 10:50,.)
lines(10:50,colMeans(m_parity,na.rm = TRUE))
# aka, doesn't account for left truncation?

#m_orders # birth events marked w parity, but ages w no births are 0s.




# so we continue w m_parity or m_unions.

# m_parity
# m_unions

all(is.na(m_parity) == is.na(m_unions))

# soon go for macro stats. w alignment and counting





