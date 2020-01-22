library(here); library(devtools); library(TraMineR); library(tidyverse)
library(reshape2); library(colorspace); library(xtable)

install_github("timriffe/Spells/Spells/R/Spells")
# load_all(here("Spells","R","Spells"))
# load(here("Spells","Data","Castro","cas_wom_seqs.RData"))

db_tidy <- readRDS(here("Spells","Data","Castro","cas_wom_tidy.rds"))

first_sex <- function(x){
  x[!is.na(x)][1]
}

db_to_summarize_and_plot <- db_tidy %>% 
  mutate(parity = as.character(parity),
         bparity = as.character(bparity),
         gparity = as.character(gparity)) %>% 
  group_by(ident) %>% 
  # need to filter on ever-union, if we use it to align.
  mutate(dfb = min(dob, na.rm=T),
         wdb = min(v011, na.rm=T),
         afb = floor((dfb- wdb)/12), afb5 = afb - afb %% 5,
         sexf = first_sex(sex),
         ev_union = any(evmar == "M", na.rm = TRUE),
         ceb = max(as.numeric(parity)),
         ceb = ifelse(ceb > 3, 4, ceb),
         
         # Alingments
         left_union = align(x = ev_union,
                            state = "NM", 
                            type ="left",
                            spell = "first"),
         
         left_par1 = align(x = parity,
                           state = "1", 
                           type ="left",
                           spell = "first"),
         
         # Note: if first birth is a girl, la_fboy just starts at 0
         la_fboy = align(x = bparity, state = "1", type ="left", spell = "first"),
         la_fgir = align(x = gparity, state = "1", type ="left", spell = "first"),
         
         # Clocks
         c_step_down_par1 = clock(x = parity, state = "1",  clock_type  = "step", 
           increasing  = FALSE),
         
         c_step_down_boy1 = clock(x = bparity, state = "1", clock_type  = "step", 
           increasing  = FALSE),
         
         c_step_down_gir1 = clock(x = gparity, state = "1", clock_type  = "step", 
           increasing  = FALSE)
  ) %>% 
  ungroup() %>% 
  filter(ceb >= 2 & afb<40, !is.na(mage))

saveRDS(db_to_summarize_and_plot, here("Spells","Data","Castro","cas_wom_period.rds"))

