
library(here)
library(tidyverse)
library(markovchain)
library(reshape2)
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


# 1) read in ITSILC data:

TR <- readRDS(here("Spells","Data","Lorenti","SILC_tr_prob12_15.RDS"))


# 2) function to get U for a given sex and quintile.

getpiu <- function(TR, sexx = "M", InQx = "I", from = "Disabled", to = "Healthy"){
  pivec <- TR %>% 
    filter(sex == {{sexx}} &
           InQ == {{InQx}} &
           state_from == {{from}} &
           state_to == {{to}}) %>% 
    pull(probs)
  
  pi2u(pivec, from = from, to = to, interval = 1, start_age = 16)
}
getpiu(TR = TR, sexx = "M", InQx = "I", 
       from = "Healthy", to = "Healthy") %>% dim()

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

# highest and lowest quintile, Females:
FV <- getU(TR, sex = "F", InQ = "V") 
FI <- getU(TR, sex = "F", InQ = "I") 

# turn into usable objects:

closeout <- function(U){
  U[U < 0] <- 0
  U1 <- cbind(U, 0)
  U2 <- rbind(U1, 1 - colSums(U1))
  #all(colSums(U1) < 1)
  
  # transpose to the standard Markov orientation
  U3 <- t(U2)
  U3
}

FVs <- closeout(FV)
FIs <- closeout(FI)

age_state   <- c(outer(seq(16,80,by=1),
                       paste0("::",c("Healthy","Disabled")),paste0),"Dead")
dimnames(FVs) <- list(to=age_state, from=age_state)
dimnames(FIs) <- list(to=age_state, from=age_state)
# Get names to match state space


# create markovchain object
# make s4 transition matrix from markovchain package
FVmc <- new("markovchain", 
             states = rownames(FVs),
             byrow = TRUE, 
             transitionMatrix = FVs,
             name = "FV")

FImc <- new("markovchain", 
            states = rownames(FIs),
            byrow = TRUE, 
            transitionMatrix = FIs,
            name = "FI")


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

colnames(FVsim) <- 1:Ntraj
colnames(FIsim) <- 1:Ntraj

rownames(FVsim) <- 16:80
rownames(FIsim) <- 16:80

FVsim           <- gsub(".*:","",FVsim)
FIsim           <- gsub(".*:","",FIsim)

FVlong <- melt(FVsim, varnames = c("age","id"), value.name = "state") %>% 
  mutate(InQ = "V", sex = "F")
FIlong <- melt(FIsim, varnames = c("age","id"), value.name = "state") %>% 
  mutate(InQ = "I", sex = "F")

Dat <- rbind(FVlong, FIlong) %>% 
  mutate(state = as.character(state))

# now this is tidy and can be analyzed









