
library(here)
library(tidyverse)
library(markovchain)
library(reshape2)
library(Spells)
# 0 function preamble

# Make a single U submatrix from a pi (transfer probs) vector
pi2u <- function(pivec, 
                 from ="H",
                 to = "H",
                 start_age = 50,
                 interval = 2) {
  out           <- cbind(rbind(0, diag(pivec)), 0)
  n             <- length(pivec)
  # the final subtraction of the interval is particular to
  # the way these probabilities were estimated and labelled.
  # to technically our first one goes from 48 to 50, not from 50 to 52.
  ages          <- ((0:n) * interval) + start_age - interval
  from_names    <- c(paste(from,ages[-length(ages)],sep="::"),"D::Inf")
  to_names      <-c(paste(to,ages[-1],sep="::"),"D::Inf")
  dimnames(out) <- list(to_names, from_names)
  out
}

# Compose u blocks into U
u2U <- function(HH, HU, UH, UU){
  rbind(
    cbind(HH, UH),
    cbind(HU, UU))
}

# convert transient dynamics into outcomes: the fundamental matrix, N
U2N <- function(U, interval = 2) {
  I   <- diag(nrow(U))
  Nsx <- solve(I - U) * interval
  dimnames(Nsx) <- dimnames(U)
  Nsx
}

# 2) function to get sub U for a given sex and quintile.
getpiu <- function(TR, sexx = "M", InQx = "I", from = "Disabled", to = "Healthy"){
  pivec <- TR %>% 
    filter(sex == {{sexx}} &
             InQ == {{InQx}} &
             state_from == {{from}} &
             state_to == {{to}}) %>% 
    pull(probs)
  
  pi2u(pivec, from = from, to = to, interval = 1, start_age = 16)
}

# function to compose whole U for given sex and quintile
getU <- function(TR, sexx = "M", InQx = "I"){
  HH <- getpiu(TR = TR, sexx = sexx, InQx = InQx, 
               from = "Healthy", to = "Healthy")
  HU <- getpiu(TR = TR, sexx = sexx, InQx = InQx, 
               from = "Healthy", to = "Disabled")
  UH <- getpiu(TR = TR,sexx = sexx, InQx = InQx, 
               from = "Disabled", to = "Healthy")
  UU <- getpiu(TR = TR, sexx = sexx, InQx = InQx, 
               from = "Disabled", to = "Disabled")
  U <- u2U(HH = HH, # healthy to healthy
           HU = HU, # healthy to Disabled
           UH = UH, # Disabled to healthy
           UU = UU) # Disabled to Disabled
  U
}

# turn into usable markov objects:

closeout <- function(U, name = "FV"){
  U[U < 0] <- 0
  U1 <- cbind(U, 0)
  U2 <- rbind(U1, 1 - colSums(U1))
  #all(colSums(U1) < 1)
  
  # transpose to the standard Markov orientation
  U3 <- t(U2)
  
  # give adequate names
  age_state   <- c(outer(seq(16,80,by=1),
                         paste0("::",c("Healthy","Disabled")),paste0),"Dead")
  
  dimnames(U3) <- list(to=age_state, from=age_state)
  # create markovchain object
  new("markovchain", 
      states = rownames(U3),
      byrow = TRUE, 
      transitionMatrix = U3,
      name = name)
}

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
Ntraj <- 10000

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

# now this is tidy and can be analyzed
head(Dat)

DisStats <- 
  Dat %>% 
  # remove age 80, since we closed out
  filter(age < 80) %>% 
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
 
    
# mean spell duration for spells
# starting in age x
DisStats %>% 
  filter(first,
         !is.na(dis_dur)) %>% 
  group_by(sex, InQ, age) %>% 
  summarize(dur_first_mean = mean(dis_dur, na.rm = TRUE)) %>% 
  mutate(dur_first_mean = na_if(dur_first_mean, NaN)) %>% 
  ggplot(mapping = aes(x = age, 
                       y = dur_first_mean, 
                       color = InQ)) + 
  geom_line(size=3) + 
  xlim(16,70) +
  labs(x = "Age", y = "mean spell duration",
       main = "Mean disability spell duration of spells starting in age x")

# mean spell duration for spells
# ending in age x
DisStats %>% 
  filter(last,
         !is.na(dis_dur)) %>% 
  group_by(sex, InQ, age) %>% 
  summarize(dur_last_mean = mean(dis_dur, na.rm = TRUE)) %>% 
  mutate(dur_last_mean = na_if(dur_last_mean, NaN)) %>% 
  ggplot(mapping = aes(x = age, 
                       y = dur_last_mean, 
                       color = InQ)) + 
  geom_line() + 
  xlim(17,80)
  

# mean spell order of spells
# starting in age x
DisStats %>% 
  filter(first,
         !is.na(dis_dur)) %>% 
  group_by(sex, InQ, age) %>% 
  summarize(order_first_mean = mean(dis_order, na.rm = TRUE)) %>% 
  mutate(order_first_mean = na_if(order_first_mean, NaN)) %>% 
  ggplot(mapping = aes(x = age, 
                       y = order_first_mean, 
                       color = InQ)) + 
  geom_line() + 
  xlim(16,70) + 
  geom_segment(aes(x=54,y=2,xend=65,yend=2),color = "black") +
  annotate("text", x = 58, y = 2.05, label = "11 years") +
  labs(x = "Age", 
       y = "mean episode order",
       main = "New disability episodes are on average 2nd episodes by age 54 if you're poor, 65 if you're rich")
  



