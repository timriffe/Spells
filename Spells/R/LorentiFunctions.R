
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
  to_names      <- c(paste(to,ages[-1],sep="::"),"D::Inf")
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
getpiu <- function(TR, from = "Disabled", to = "Healthy", start_age = 15){
  TR %>% 
    # removed because group already selected in pipeline / data.table by statement
    # filter(sex == {{sexx}} &
    #          InQ == {{InQx}} &
    filter(
             state_from == {{from}} &
             state_to == {{to}}) %>% 
    pull(probs) %>% 
    pi2u(from = from, to = to, interval = 1, start_age = start_age)
}

# function to compose whole U for given sex and quintile
getU <- function(TR, start_age = 15){
  HH <- getpiu(TR = TR,
               from = "Healthy", to = "Healthy", 
               start_age = start_age)
  HU <- getpiu(TR = TR, 
               from = "Healthy", to = "Disabled", 
               start_age = start_age)
  UH <- getpiu(TR = TR, 
               from = "Disabled", to = "Healthy",
               start_age = start_age)
  UU <- getpiu(TR = TR,
               from = "Disabled", to = "Disabled",
               start_age = start_age)
  U <- u2U(HH = HH, # healthy to healthy
           HU = HU, # healthy to Disabled
           UH = UH, # Disabled to healthy
           UU = UU) # Disabled to Disabled
  U
}

# turn into usable markov objects:

closeout <- function(U, name = "FV", start_age = 15){
  U[U < 0] <- 0
  U1 <- cbind(U, 0)
  U2 <- rbind(U1, 1 - colSums(U1))
  #all(colSums(U1) < 1)
  
  # transpose to the standard Markov orientation
  U3 <- t(U2)
  
  # give adequate names
  # first data version started at age 16, later at 15?
  age_state   <- c(outer(start_age:80,
                         paste0("::",c("Healthy","Disabled")),paste0),"Dead")
  
  dimnames(U3) <- list(to=age_state, from=age_state)
  # create markovchain object
  new("markovchain", 
      states = rownames(U3),
      byrow = TRUE, 
      transitionMatrix = U3,
      name = name)
}


# Ugly beast function to be called inside do() for boot results..
# This should be called inside a 'do()'?
get_trajectories <- function(
  X, 
  Ntraj = 50000, 
  case = 1){
  
  Fimc <- X %>% 
    getU(start_age = 16) %>% 
    closeout()
  # make sim matrix
  Fsim  <- replicate(Ntraj,
                     rmarkovchain(n = 65, 
                                  object = Fimc, 
                                  t0 = "15::Healthy", 
                                  parallel = TRUE)
  )
  
  dimnames(Fsim) <- list(15:79, 1:Ntraj)
  Fsim           <- gsub(".*:","", Fsim)
  # start pipe
  Fsim <-
    Fsim %>% 
    reshape2::melt(varnames = c("age","id"), 
         value.name = "state") %>% 
    mutate(state = as.character(state)) %>% 
    filter(age < 80) %>% # it's closed out so everyone dead at 80...
    group_by(id) %>% 
    mutate(dead = ifelse(any(state == "Dead"),TRUE,FALSE)) %>% 
    ungroup() %>% 
    filter(state != "Dead") %>% 
    group_by(id) %>% 
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
    group_by(id, dis_order) %>% 
    mutate(first = row_number() == 1L,
           last = row_number() == n(),
           first = ifelse(state == "Disabled" & 
                            max(age) == 79, FALSE, first),
           last = ifelse(state == "Disabled" & 
                           max(age) == 79, FALSE, last)) 
  
  # now toggle output depending on application:
  if (case == 1){
    Fsim <- Fsim %>% 
      ungroup() %>% 
      filter(first,
             !is.na(dis_dur)) %>% 
      group_by(age) %>% 
      summarize(dur_first_mean = mean(dis_dur, na.rm = TRUE)) %>% 
      mutate(dur_first_mean = na_if(dur_first_mean, NaN))
    # ready to plot!
  }
  
  if (case == 2){
    Fsim <- Fsim %>% 
      filter(first,
             !is.na(dis_dur)) %>% 
      group_by(age) %>% 
      summarize(order_first_mean = mean(dis_order, na.rm = TRUE)) %>% 
      mutate(order_first_mean = na_if(order_first_mean, NaN))
  }
  
  if (case == 3){
    Fsim <- Fsim %>%
      filter(dead) %>% 
      group_by(id) %>% 
      mutate(ttd = max(age) - age,
             ad5 = max(age) - max(age) %% 5) %>% 
      ungroup() %>% 
      filter(ad5 > 30) %>% 
      group_by(ttd, ad5) %>% 
      summarize(ttdprev = mean(state == "Disabled"))
  }
  Fsim
}




