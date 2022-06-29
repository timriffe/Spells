# this script takes bootstrapped transition probabilities produced by Angelo
# and it iterates as necessary to produce the three results for application #1

# We just write out the results, and we don't save the intermediate data 
# objects (simulated trajectories)

# 0) load functions
source(here::here("R","00_load_functions.R"))

TRin <- readRDS(here::here("Data","Application1","boot_tp_limitations.rds"))

# this should automatically choose 40 if on Hydra,
# or 6 if on TR's laptop
clsize <- max(c(min(c(parallel::detectCores()-2,40)),1))
Ntraj <- ifelse(clsize == 40, 50000, 1000)


TRp <-
  TRin %>% 
  # just pick out extreme quintiles
  dplyr::filter(INC_Q %in% c("I","V"),
         sex == "F") %>% 
  mutate(from = as.character(from),
         state_from =  str_extract(from,"[a-z,A-Z]+"),
         to = as.character(to),
         state_to = str_extract(to,"[a-z,A-Z]+"),
         parallel_group = i %% clsize + 1,
         age_from = str_extract(from,"[0-9]+")) %>% 
  select(-from, - to) %>% 
  arrange(parallel_group, i, INC_Q) %>% 
  base::split(f=list(.$parallel_group),drop = TRUE)

inner_fun <- function(X, .case = 1, .Ntraj = 1000){
  X %>% 
    as_tibble() %>% 
    group_by(i, INC_Q) %>% 
    do(get_trajectories(X = .data, Ntraj = .Ntraj, case = .case)) %>% 
    ungroup()
}



tic()
A1.1 <-
  mclapply(TRp,
           inner_fun,
           .Ntraj = Ntraj,
           .case = 1,
           mc.cores = clsize) %>% 
  bind_rows()
toc()

saveRDS(A1.1, file = here::here("Data","Application1","A1.1.rds"))
rm(A1.1);gc()

# second example in application 1
tic()
A1.2 <-
  mclapply(TRp,
           inner_fun,
           .Ntraj = Ntraj,
           .case = 2,
           mc.cores = clsize) %>% 
  bind_rows()
toc()

saveRDS(A1.2, file = here::here("Data","Application1","A1.2.rds"))
rm(A1.2);gc()

# third example in application 1
# second example in application 1
tic()
A1.3 <-
  mclapply(TRp,
           inner_fun,
           .Ntraj = Ntraj,
           .case = 3,
           mc.cores = clsize) %>% 
  bind_rows()
toc()
saveRDS(A1.3, file = here::here("Data","Application1","A1.3.rds"))
rm(A1.3);gc()


# p
# a <- TRp %>% 
#   do(get_trajectories(.data, Ntraj = 500, case = 1)) %>% 
#   collect()
# toc()
# TR <- as.data.table(TR)

# Takes a looooong time, single-threaded!
# A1.1 <- TR[, get_trajectories(.SD, Ntraj = 50000, case = 1), by = list(i,InQ,sex)]
# save.rds(A1.1, file = here::here("Spells","Data","Application1","A1.1.rds"))
# rm(A1.1);gc()
# 
# A1.2 <- TR[, get_trajectories(.SD, Ntraj = 50000, case = 2), by = list(i,InQ,sex)]
# save.rds(A1.2, file = here::here("Spells","Data","Application1","A1.2.rds"))
# rm(A1.2);gc()
# 
# A1.3 <- TR[, get_trajectories(.SD, Ntraj = 50000, case = 3), by = list(i,InQ,sex)]
# save.rds(A1.3, file = here::here("Spells","Data","Application1","A1.3.rds"))
# rm(A1.3);gc()
# 
# rm(TR);gc()


