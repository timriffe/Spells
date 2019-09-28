# Tests fertility
library(here)
library(tidyverse)
pkg_path <- here("Spells","R","Spells")
library(devtools)
load_all(pkg_path)

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

# convret numeric data.frame to character matrix
as.character.matrix <- function(X){
  lapply(X, as.character) %>% 
    as.data.frame() %>% 
    as.matrix() 
}
library(rlang)
as.tidy.df <- function(X, value.name = "parity"){

    dimnames(X) <- list(id = 1:nrow(X),
                        age = 10:(9+ncol(X)))
    X$id <- 1:nrow(X)
    
    X %>%
    gather(age, {{value.name}}, everything(), -id, convert = TRUE) 
}
m_parity %>% 
  as.tidy.df %>%
  mutate(parity = as.character(parity)) %>% 
  arrange(id,age) %>% 
  group_by(id) %>% 
  mutate(dur0 = clock(x = parity, state = "0", clock_type = "duration"),
         dur1 = clock(x = parity, state = "1", clock_type = "duration"),
         dur2 = clock(x = parity, state = "2", clock_type = "duration"),
         dur3 = clock(x = parity, state = "3", clock_type = "duration"),
         ) %>% 
  head()

align(x, state = "Inactive", type = "right", spell = "last")

# cumsum treating NAs as 0s, then reimputing NAs
cumsum_na <- function(x,na.rm = TRUE){
  na_ind      <- is.na(x)
  x[na_ind]   <- 0
  xcs         <- cumsum(x)
  xcs[na_ind] <- NA
  xcs
}

# load(data_path)
# # m_parity derived from / relates to m_births
# apply(m_births,1,cumsum_na,na.rm=TRUE) %>% 
#   rowMeans(na.rm=TRUE) %>% 
#   plot(x = 10:50,.)
# lines(10:50,colMeans(m_parity,na.rm = TRUE))
# aka, doesn't account for left truncation?

#m_orders # birth events marked w parity, but ages w no births are 0s.
# so we continue w m_parity or m_unions.

# m_parity
# m_unions

# all(is.na(m_parity) == is.na(m_unions))
# soon go for macro stats. w alignment and counting


# clock.matrix <- function(X, ...){
#   apply(X = X, MARGIN = 1, FUN = clock, ...) %>% t()
# }




# apply(m_parity, 1, clock, state = "0", clock_type = "step", increasing = TRUE) %>% t()

# TODO: would like to be able to clock with conditional statements.
# Should that happen using filtering or some other way?
# 
# clock.matrix(m_parity, state = "1", clock_type = "step", increasing = FALSE) %>% 
#   colMeans(na.rm=TRUE) %>% 
#   plot(10:50,.,type='l')





load(data_path)

  m_parity %>% 
    as.tidy.df(value.name = "parity") %>% 
    str()
    mutate(dur = clock(parity))
