source(here::here("R","00_install_packages.R"))

db_tidy <- readRDS(here("Data","Application2","cas_wom_tidy_senegal.rds"))

first_sex <- function(x){
  x[!is.na(x)][1]
}
x <- c(NA, NA, NA, 2, NA, NA,2, NA, NA, 2)

next_sex <- function(x, sex = 1){
  out       <- rep(NA, length(x))
  ind       <- !is.na(x)
  indd      <- which(ind)
  par1      <- indd[1]
  sexind    <- x == sex
  if (!any(sexind, na.rm = TRUE)){
    return(out)
  }
  ind_sex       <-  which(sexind) 
  ind_sex_right <- ind_sex > par1
  if (!any(ind_sex_right, na.rm = TRUE)){
    return(out)
  }
  
  sex_right <- ind_sex[ind_sex_right][1]
  out[par1:(sex_right - 1)] <- "X"
  out
}

clock(next_sex(x,2),state="X",clock_type= "step",increasing = FALSE)

db_to_summarize_and_plot <- db_tidy %>% 
  mutate(parity = as.character(parity),
         bparity = next_sex(sex, 1),
         gparity = next_sex(sex, 2)) %>% 
  group_by(ident) %>% 
  # need to filter on ever-union, if we use it to align.
  mutate(bparity = next_sex(sex, 1),
         gparity = next_sex(sex, 2),
         dfb = min(dob, na.rm=T),
         wdb = min(v011, na.rm=T),
         afb = floor((dfb- wdb)/12), afb5 = afb - afb %% 5,
         sexf = first_sex(sex),
         #ev_union = any(evmar == "M", na.rm = TRUE),
         ceb = max(as.numeric(parity)),
         ceb = ifelse(ceb > 3, 4, ceb),
         
         # Alingments
         # left_union = align(x = ev_union,
         #                    state = "NM", 
         #                    type ="left",
         #                    spell = "first"),
         
         left_par1 = Spells::align(x = parity,
                           state = "1", 
                           type ="left",
                           spell = "first"),
         
         # Note: if first birth is a girl, la_fboy just starts at 0
         #la_fboy = align(x = bparity, state = "X", type ="left", spell = "first"),
         #la_fgir = align(x = gparity, state = "X", type ="left", spell = "first"),
         
         # Clocks
         c_step_down_par1 = clock(x = parity, state = "1",  clock_type  = "step", 
           increasing  = FALSE),
         
         c_step_down_boy1 = clock(x = bparity, state = "X", clock_type  = "step", 
           increasing  = FALSE),
         
         c_step_down_gir1 = clock(x = gparity, state = "X", clock_type  = "step", 
           increasing  = FALSE)
  ) %>% 
  ungroup() %>% 
  filter(ceb >= 2 & afb<40, !is.na(mage))

saveRDS(db_to_summarize_and_plot, here("Data","Application2","cas_wom_senegal.rds"))
remove(list=ls())
Sys.time()
