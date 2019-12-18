
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