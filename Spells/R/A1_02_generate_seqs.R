# this script takes bootstrapped transition probabilities produced by Angelo
# and it iterates as necessary to produce the three results for application #1

# We just write out the results, and we don't save the intermediate data 
# objects (simulated trajectories)

# these calcs just for one state space and sex.

library(here)
library(tidyverse)
library(markovchain)
library(reshape2)
library(stringr)
#library(Spells)
library(devtools)
# 0) load functions
source(here::here("Spells","R","LorentiFunctions.R"))

load_all(here("Spells","R","Spells"))
TR <- readRDS(here("Spells","Data","Lorenti","boot_females_tp_limitations.rda"))
TR %>% 
  mutate(from =  str_extract(from,"[a-z,A-Z]+"),
         sex = "F") %>% 
  rename(state_from = from) %>% 
  # just pick out extreme quintiles
  filter(InQ %in% c("I","V")) %>% 
  group_by(InQ)
  

# This should be called inside a 'do()'?
get_trajectories <- function(
  X, 
  .InQ = "I", 
  .sex = "F", 
  Ntraj = 50000, 
  case = 1){
  Fi <- getU(X, sex = .sex, InQ = .InQ) 
  Fimc <- closeout(Fi)
  # make sim matrix
  Fsim  <- replicate(Ntraj,
                     rmarkovchain(n = 65, 
                                  object = FVmc, 
                                  t0 = "16::Healthy", 
                                  parallel = TRUE)
                     )
                  
  dimnames(Fsim = list(16:80, 1:Ntraj))
  Fsim           <- gsub(".*:","", Fsim)
  # start pipe
  Fsim <-
    Fsim %>% 
   melt(varnames = c("age","id"), 
        value.name = "state") %>% 
   mutate(InQ = .InQ, sex = .sex) %>% 
   filter(age < 80) %>% # it's closed out so everyone dead at 80...
   group_by(InQ, id) %>% 
   mutate(dead = ifelse(any(state == "Dead"),TRUE,FALSE)) %>% 
   ungroup() %>% 
   filter(state != "Dead") %>% 
   group_by(InQ, id) %>% 
    # say we want average duration of disability
    # * spells starting in age x
    # * spells ending in age x
    # avg duration of nth spells by age.
   mutate(dis_dur = clock(state, 
                          state = "Disabled",
                          clock_type = "duration"),
          dis_order = clock(state, 
                            state = "Disabled",
                            clock_type = "order",
                            increasing = TRUE)) %>% 
   group_by(InQ, id, dis_order) %>% 
   mutate(first = row_number() == 1L,
          last = row_number() == n(),
          first = ifelse(state == "Disabled" & 
                         max(age) == 79, FALSE, first),
          last = ifelse(state == "Disabled" & 
                           max(age) == 79, FALSE, last)) 
  
  # now toggle output depending on application:
  if (case == 1){
    Fsim <- Fsim %>% 
      ungroup() %>% 
      mutate(InQ = case_when(InQ == "I" ~ "lowest 20%",
                             InQ == "V" ~ "highest 20%")) %>% 
      filter(first,
             !is.na(dis_dur)) %>% 
      group_by(sex, InQ, age) %>% 
      summarize(dur_first_mean = mean(dis_dur, na.rm = TRUE)) %>% 
      mutate(dur_first_mean = na_if(dur_first_mean, NaN))
    # ready to plot!
  }
  
  if (case == 2){
    Fsim <- Fsim %>% 
      filter(first,
             !is.na(dis_dur)) %>% 
      group_by(sex, InQ, age) %>% 
      summarize(order_first_mean = mean(dis_order, na.rm = TRUE)) %>% 
      mutate(order_first_mean = na_if(order_first_mean, NaN))
  }
  
  if (case == 3){
    Fsim <- Fsim %>%
      filter(dead) %>% 
      group_by(InQ, id) %>% 
      mutate(ttd = max(age) - age,
             ad5 = max(age) - max(age) %% 5) %>% 
      ungroup() %>% 
      filter(ad5 > 30) %>% 
      group_by(sex, InQ, ttd, ad5) %>% 
      summarize(ttdprev = mean(state == "Disabled"))
  }
  Fsim
}

