library(here);library(tidyverse)
remove(list=ls())

load(here('Spells', 'Data', 'Castro', 'cas_wom_dhs_raw_colombia.RData'))
Sys.time(); # runs in 1m (Senegal, 100,000 obs)
# move to long format, includes
# only women > age 10, up until (and including) age at survey
# and the variables mage, dob, sex, order, and parity.
# This lacks a union status variable, which could still be calculated
dat <- db %>% 
  select(-starts_with("b0_")) %>% 
  pivot_longer(cols = b3_01:bord_20,
               names_to = c(".value","value"),
               names_sep = "_",
               values_drop_na = TRUE) %>% 
  rename(dob = b3, sex = b4) %>%  
  mutate(mage = floor((dob - v011) / 12),
         maget =  floor((v008 - v011) / 12),
         afu = floor((v509 - v011) / 12)) %>% 
  complete(mage,  nesting(ident, maget)) %>% 
  filter(mage <= maget,
         mage >= 10) %>% 
  group_by(ident) %>% 
  mutate(parity = ifelse(is.na(dob),0,1),
         parity = cumsum(parity),
         bparity = ifelse(!is.na(sex) & sex == 1, 1, 0),
         bparity = cumsum(bparity),
         gparity = ifelse(!is.na(sex) & sex == 2, 1, 0),
         gparity = cumsum(gparity),
         res = v025[which.max(mage)],
         pwt = v005[!is.na(v005)][1]/1000000,
         v008 = v008[which.max(mage)],
         yr = v008 / 12 + 1900,
         waveyr = round(yr / 5) * 5,
         waveyr = ifelse(waveyr == 1985, 1986, waveyr),
         afu = floor((v509-v011) / 12),
         evmar = case_when(afu > mage ~ "NM",
                           mage >= afu ~ "M",
                           TRUE ~ "NM")) %>% 
  ungroup() %>% 
  arrange(ident, mage) 

dat$key<-paste0(dat$ident, '_', dat$mage)
dat<-dat[duplicated(dat$key)==FALSE,]

saveRDS(dat,here("Spells","Data","Castro","cas_wom_tidy_colombia.rds"))
Sys.time()
