library(here)
library(tidyverse)
library(markovchain)
library(reshape2)
library(Spells)
# 0) load functions
source(here::here("Spells","R","LorentiFunctions.R"))

# 1) read in ITSILC data:

TR <- readRDS(here("Spells","Data","Lorenti","SILC_tr_prob12_15.RDS"))

# 2) get U
# highest and lowest quintile, Females:
FV <- getU(TR, sex = "F", InQ = "V") 
FI <- getU(TR, sex = "F", InQ = "I") 

# 3) transform to markovchain objects
FVmc <- closeout(FV)
FImc <- closeout(FI)

# 4)
# simulation global parameter
Ntraj <- 30000

# each assuming a start healthy at age 50.
# *If you wanted some fraction to start
# in healthy and some in disability, then
# you'd need to repeat this with round(Ntraj * init[1])
# healthy starting cases, and round(Ntraj * init[2])
# disabled starting cases
FVsim  <- replicate(Ntraj,
                    rmarkovchain(n = 65, 
                                 object = FVmc, 
                                 t0 = "16::Healthy", 
                                 parallel = TRUE)
)

FIsim  <- replicate(Ntraj,
                    rmarkovchain(n = 65, 
                                 object = FImc, 
                                 t0 = "16::Healthy", 
                                 parallel = TRUE)
)

# 5) clean the names
colnames(FVsim) <- 1:Ntraj
colnames(FIsim) <- 1:Ntraj

rownames(FVsim) <- 16:80
rownames(FIsim) <- 16:80

FVsim           <- gsub(".*:","",FVsim)
FIsim           <- gsub(".*:","",FIsim)

# 6) turn into tidy data, bind together for joint computations.
FVlong <- melt(FVsim, 
               varnames = c("age","id"), 
               value.name = "state") %>% 
  mutate(InQ = "V", sex = "F")
FIlong <- melt(FIsim, 
               varnames = c("age","id"), 
               value.name = "state") %>% 
  mutate(InQ = "I", sex = "F")

Dat <- rbind(FVlong, FIlong) %>% 
  mutate(state = as.character(state))

saveRDS(Dat, here::here("Spells","Data","Lorenti","SILCsim.rds"))
