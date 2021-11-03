# this script takes bootstrapped transition probabilities produced by Angelo
# and it iterates as necessary to produce the three results for application #1

# We just write out the results, and we don't save the intermediate data 
# objects (simulated trajectories)

# 0) load functions
source(here::here("R","00_install_packages.R"))

TR <- readRDS(here("Data","Application1","boot_females_tp_limitations.rda"))


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

# this should automatically choose 40 if on Hydra,
# or 6 if on TR's laptop
clsize <- max(c(min(c(parallel::detectCores()-2,40)),1))
cl     <- makeCluster(clsize)
registerDoParallel(cl)
getDoParWorkers()

trials <- length(TRp)
# should do small sizes on TR's laptop, big for Hydra
Ntraj  <- ifelse(clsize == 6, 1000, 50000)
# first example in application 1
tic()
A1.1 <- foreach(i = icount(trials),
                .combine = 'rbind',
                .packages=c('Spells','tidyverse',"markovchain"),.errorhandling = 'remove') %dopar% {
                  get_trajectories(TRp[[i]], Ntraj = Ntraj, case = 1)
                }
(a1.1.time <- toc())

saveRDS(A1.1, file = here::here("Data","Application1","A1.1.rds"))
rm(A1.1);gc()

# second example in application 1
tic()
A1.2 <- foreach(i = icount(trials),
                .combine = 'rbind',
                .packages=c('Spells','tidyverse',"markovchain"),.errorhandling = 'remove') %dopar% {
                  get_trajectories(TRp[[i]], Ntraj = Ntraj, case = 2)
                }
(a1.2.time <- toc())

saveRDS(A1.2, file = here::here("Data","Application1","A1.2.rds"))
rm(A1.1);gc()

# third example in application 1
tic()
A1.3 <- foreach(i = icount(trials),
                .combine = 'rbind',
                .packages=c('Spells','tidyverse',"markovchain"),.errorhandling = 'remove') %dopar% {
                  get_trajectories(TRp[[i]], Ntraj = Ntraj, case = 3)
                }
(a1.3.time <- toc())
saveRDS(A1.3, file = here::here("Data","Application1","A1.3.rds"))
rm(A1.3);gc()


stopCluster(cl) 
closeAllConnections() 
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


