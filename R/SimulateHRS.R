# simulate from HRS transitions


library("markovchain")
# Data coming from different project
data_home <- "/home/tim/workspace/HLEDecomp/HLEDecomp"

# functions copied-pasted from there, too specific to be packageable...
getcols <- function(ntrans = 3, self = TRUE, dead = "4"){
  if (self){
    return(paste0("m",c(t(outer(1:ntrans,1:ntrans,paste0)))))
  } else {
    cols <- outer(1:ntrans,c(1:ntrans,dead),paste0)
    cols <- sort(cols[lower.tri(cols) | upper.tri(cols)])
    cols <- paste0("m",cols)
    return(cols)
  }
}

get_TR <- function(version = "06",home = getwd(),...){
  path <- file.path(home,"Data","Transitions","DCS",
                    paste0("TR_v",version,".Rdata"))
  Dat           <- local(get(load(path)))
  Dat$educlevel <- NULL
  Dat$spec      <- NULL
  Dat$specnum   <- NULL
  Dat$specgr    <- NULL
  Dat$specnum2  <- NULL
  Dat$specgr    <- NULL
  subset(Dat, ...)
}

pi2u <- function(pivec){
  cbind(rbind(0,diag(pivec)),0)
}

# just transition rates, excluding to death, single sex, edu, year
data_2_U <- function(datself, ntrans = 3){
  datself    <- as.data.frame(datself, stringsAsFactors = FALSE)
  # get the block order
  trans.self <- getcols(ntrans = ntrans, self = TRUE)
  this.order <- as.data.frame(
    matrix(trans.self, ntrans, byrow = FALSE), stringsAsFactors = FALSE)
  
  # take advantage of data.frame columns as list elements
  UL         <- lapply(as.data.frame(datself[, trans.self], stringsAsFactors = FALSE), pi2u)
  names(UL)  <- trans.self
  U          <- do.call("cbind", 
                        lapply(this.order, function(x, UL){
                          do.call("rbind", UL[x])
                        }, UL = UL)
  )
  
  U
}


simulate_HRS <- function(
  version = "06",
  .sex = "f", 
  .edu = "all_edu", 
  .time = 1996,
  N=1e4,
  data_home){
  datself <- get_TR(version = version,
                    subset = sex == .sex & edu == .edu & time == .time, 
                    home = data_home)
  # All edu females
  U <- data_2_U(datself)
  
  # complete the Markov matrix with Dead state:
  U <- cbind(U, 0)
  U <- rbind(U, 1 - colSums(U))
  age_state   <- c(outer(seq(48,110,by=2),paste0("::",c("H","ADL1","ADL+")),paste0),"Dead")
  dimnames(U) <- list(to=age_state, from=age_state)
  
  # switch from Leslie-Caswell to standard Markov
  U <- t(U)
  
  # make s4 transition matrix from markovchain package
  mcADL <- new("markovchain", states = rownames(U),
               byrow = TRUE, transitionMatrix = U,
               name = "ADL")
  
  # how many sequences should we generate?
  
  # each assuming a start in employment at age 50.
  RHRS  <- replicate(N,
                     rmarkovchain(n = 31, object = mcADL, t0 = "48::H", parallel = TRUE)
  ) 
  RHRS                 <- rbind(rep("48::H",N), RHRS)
  RHRS_clean           <- gsub(".*:","",RHRS)
  rownames(RHRS_clean) <- seq(48,110,by=2)
  colnames(RHRS_clean) <- 1:ncol(RHRS_clean)
  RHRS_clean
}

RHRS_1 <- simulate_HRS("06", .sex = "f", .edu = "all_edu", 
                       .time = 1996, 
                       N = 1e4, data_home=data_home)
RHRS_2 <- simulate_HRS("06", .sex = "f", .edu = "all_edu", 
                       .time = 2006, 
                       N = 1e4, data_home=data_home)
RHRS_3 <- simulate_HRS("06", .sex = "f", .edu = "all_edu", 
                       .time = 2014, 
                       N = 1e4, data_home=data_home)

RHRS_1_l <- melt(RHRS_1, varnames = c("age","id"), value.name = "state")
RHRS_2_l <- melt(RHRS_2, varnames = c("age","id"), value.name = "state")
RHRS_3_l <- melt(RHRS_3, varnames = c("age","id"), value.name = "state")

RHRS_1_l <- RHRS_1_l %>% mutate(year = 1996)
RHRS_2_l <- RHRS_2_l %>% mutate(year = 2006)
RHRS_3_l <- RHRS_3_l %>% mutate(year = 2014)

RHRS <- rbind(RHRS_1_l,RHRS_2_l,RHRS_3_l)

# # Seems reasonable
# mean(colSums(RHRS_clean=="H")) * 2
# mean(colSums(RHRS_clean=="ADL1")) * 2
# mean(colSums(RHRS_clean=="ADL+")) * 2

# saveRDS(RHRS_clean, here("Spells","Data","HRS","RHRS_clean.rds"))
# 
# RHRS_clean <- readRDS(here("Spells","Data","HRS","RHRS_clean.rds"))
RHRS_long <- RHRS %>% 
  mutate(state = as.character(state)) %>% 
  arrange(year, id, age) %>% 
  group_by(year, id) %>% 
  mutate(
    x_exit_health_first = align(x = state, state = "H", type = "right", spell="first"),
    x_exit_health_last = align(x = state, state = "H", type = "right", spell="last"),
    x_death = align(x = state, state = c("H","ADL1","ADL+"), type = "right", spell="last"),
    y_dur_adl = clock(x = state, clock_type = "duration", state = c("ADL1","ADL+"), step_size=2),
    y_step_up_adl = clock(x = state, clock_type = "step", increasing = TRUE, state =  c("ADL1","ADL+"), step_size=2),
    y_step_down_adl = clock(x = state, clock_type = "step", increasing = FALSE, state =  c("ADL1","ADL+"), step_size=2)
  ) %>% 
  gather(key = "alignment",value="x",c("age","x_exit_health_first","x_exit_health_last","x_death")) %>% 
  gather(key = "clock", value = "value", c("y_dur_adl","y_step_up_adl","y_step_down_adl")) %>% 
  filter(!is.na(value))

saveRDS(RHRS_long, here("Spells","Data","HRS","RHRS_long.rds"))

#DAT <- get_TR(version = "02", subset = sex == "f")
#DAT <- get_TR("02",subset = sex == "f" & edu == "all_edu" & time == 1996)

# datself <- get_TR(version = "06",
#                   subset = sex == "f" & edu == "all_edu" & time == 1996, 
#                   home = data_home)
# # All edu females
# U <- data_2_U(datself)
# 
# # complete the Markov matrix with Dead state:
# U <- cbind(U, 0)
# U <- rbind(U, 1 - colSums(U))
# age_state   <- c(outer(seq(48,110,by=2),paste0("::",c("H","ADL1","ADL+")),paste0),"Dead")
# dimnames(U) <- list(to=age_state, from=age_state)
# 
# 
# # V1  V2  V3
# # 1 m11 m12 m13
# # 2 m21 m22 m23
# # 3 m31 m32 m33
# 
# # switch from Leslie-Caswell to standard Markov
# U <- t(U)
# 
# # make s4 transition matrix from markovchain package
# mcADL <- new("markovchain", states = rownames(U),
#               byrow = TRUE, transitionMatrix = U,
#               name = "ADL")
# 
# # how many sequences should we generate?
# N      <- 5e4
# 
# set.seed(1)
# # each assuming a start in employment at age 50.
# RHRS  <- replicate(N,
#                     rmarkovchain(n = 31, object = mcADL, t0 = "48::H", parallel = TRUE)
# ) 
# RHRS                 <- rbind(rep("48::H",N), RHRS)
# RHRS_clean           <- gsub(".*:","",RHRS)
# rownames(RHRS_clean) <- seq(48,110,by=2)
# colnames(RHRS_clean) <- 1:ncol(RHRS_clean)