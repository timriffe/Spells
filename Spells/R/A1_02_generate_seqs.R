# this script takes bootstrapped transition probabilities produced by Angelo
# and it iterates as necessary to produce the three results for application #1

# We just write out the results, and we don't save the intermediate data 
# objects (simulated trajectories)

# these calcs just for one state space and sex.
# devtools::install_github("tidyverse/multidplyr")
library(here)
library(tidyverse)
library(markovchain)
library(reshape2)
library(stringr)
library(rlang)
#library(Spells)
library(devtools)
#library(data.table) # only single-threaded
#library(multidplyr) # wasn't working with group_map()
library(parallel) # finally used mclapply()
library(tictoc)
# 0) load functions
source(here::here("Spells","R","LorentiFunctions.R"))

load_all(here("Spells","R","Spells"))
TR <- readRDS(here("Spells","Data","Lorenti","boot_females_tp_limitations.rda"))

# declare cluster
use.cores <- max(c(parallel::detectCores()-2,1))
# cluster   <- new_cluster(use.cores)

TRp <- TR %>% 
  # just pick out extreme quintiles
  filter(InQ %in% c("I","V")) %>% 
  mutate(from = as.character(from),
         from =  str_extract(from,"[a-z,A-Z]+"),
         state_to = as.character(state_to),
         sex = "F") %>% 
  rename(state_from = from) %>% 
  group_by(i, InQ, sex) %>% 
  base::split(f=list(.$i,.$InQ),drop = TRUE)

# first example in application 1
tic()
A1.1 <-
  mclapply(TRp, 
         get_trajectories, 
         Ntraj = 50000, 
         case = 1, 
         mc.cores = use.cores) %>% 
  rbind_list()
toc()

save.rds(A1.1, file = here::here("Spells","Data","Lorenti","A1.1.rds"))
rm(A1.1);gc()
 
# second example in application 1
tic()
A1.2 <-
  mclapply(TRp, 
           get_trajectories, 
           Ntraj = 50000, 
           case = 2, 
           mc.cores = use.cores) %>% 
  rbind_list()
toc()

save.rds(A1.2, file = here::here("Spells","Data","Lorenti","A1.2.rds"))
rm(A1.1);gc()

# third example in application 1
tic()
A1.3 <-
  mclapply(TRp, 
           get_trajectories, 
           Ntraj = 50000, 
           case = 3, 
           mc.cores = use.cores) %>% 
  rbind_list()
toc()

save.rds(A1.3, file = here::here("Spells","Data","Lorenti","A1.3.rds"))
rm(A1.3);gc()
 
# p
# a <- TRp %>% 
#   do(get_trajectories(.data, Ntraj = 500, case = 1)) %>% 
#   collect()
# toc()
# TR <- as.data.table(TR)

# Takes a looooong time, single-threaded!
# A1.1 <- TR[, get_trajectories(.SD, Ntraj = 50000, case = 1), by = list(i,InQ,sex)]
# save.rds(A1.1, file = here::here("Spells","Data","Lorenti","A1.1.rds"))
# rm(A1.1);gc()
# 
# A1.2 <- TR[, get_trajectories(.SD, Ntraj = 50000, case = 2), by = list(i,InQ,sex)]
# save.rds(A1.2, file = here::here("Spells","Data","Lorenti","A1.2.rds"))
# rm(A1.2);gc()
# 
# A1.3 <- TR[, get_trajectories(.SD, Ntraj = 50000, case = 3), by = list(i,InQ,sex)]
# save.rds(A1.3, file = here::here("Spells","Data","Lorenti","A1.3.rds"))
# rm(A1.3);gc()
# 
# rm(TR);gc()


