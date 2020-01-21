
# Author: tim
###############################################################################
#' impute durations of episodes in specified reference state
#' @description produce a vector of \code{length(x)} values corresponding to the total duration of each episode of the reference state. 
#' @details \code{NA} are given for other states. \code{state} can be a vector of states if you'd rather consider certain states merged into single episodes.
#' 
#' @inheritParams clock
#' @export
#' @examples
#' x <- rep(c("Employed", "Inactive", "Retired", "Employed", "Retired", 
#' 				"Dead"), c(7,  8,  3,  3, 25,  5))
#' # a single 8-year spell of inactivity
#' spell_dur(x, "Inactive")
#' # merges first consecutive employment and inactivity spells into a single spell,
#' # also catches second employment after retirement
#' spell_dur(x, c("Inactive","Employed"))
#' # total remaining lifespan at 50
#' spell_dur(x, c("Inactive","Employed","Retired"))

spell_dur <- function(x, state = "Inactive", step_size = 1){
	
	# starting left x position for each observation
	x_age             <- 1:length(x) - 1
	names(x_age)      <- x
	
	# combine states if necessary
	refvar            <- "REFSTATE"
	x[x %in% state]   <- refvar
	
	sec               <- rle(x)
	spells            <- sec$values
	durs              <- sec$lengths
	durs2             <- durs
	durs2[spells != refvar] <- NA
	# if (not_first){
	# 	if (refvar %in% x){
	# 		durs2[which(spells == refvar)[1]] <- NA
	# 	}
	# 	
	# }
	
	#n_spells          <- sum(spells == state)
	spell_x         <- rep(durs2, durs)
	#spell_age[x != state] <- NA
	spell_x[is.na(x)] <- NA
	
	# adjust to step_size
	spell_x * step_size
}

#' @title impute conditional durations of episodes in specified reference state
#' @description produce a vector of \code{length(x)} values corresponding to the total duration of each episode of the reference state in either the entry time step or the exit time step
#' @details \code{NA} are given for other states. \code{state} can be a vector of states if you'd rather consider certain states merged into single episodes.
#' 
#' @inheritParams clock
#' @param entry logical. Do we impute the episode duration at the point of entry or exit?
#' @export
#' @examples
#' x <- rep(c("Employed", "Inactive", "Retired", "Employed", "Retired", 
#'            "Dead"), c(7,  8,  3,  3, 25,  5))
#' # a single 8-year spell of inactivity, conditional on entry
#' spell_dur_conditional(x, "Inactive", entry = TRUE)
#' # a single 8-year spell of inactivity, conditional on exit
#' spell_dur_conditional(x, "Inactive", entry = FALSE)
#' 
#' # merges first consecutive employment and inactivity spells into a single spell,
#' # also catches second employment after retirement
#' spell_dur_conditional(x, state=c("Inactive","Employed"), entry = TRUE)
#' spell_dur_conditional(x, state=c("Inactive","Employed"), entry = FALSE)

spell_dur_conditional <- function(x, state = "Inactive", entry = TRUE, step_size = 1){
  
  # starting left x position for each observation
  x_age             <- 1:length(x) - 1
  names(x_age)      <- x
  
  # combine states if necessary
  refvar            <- "REFSTATE"
  x[x %in% state]   <- refvar
  
  sec               <- rle(x)
  spells            <- sec$values
  durs              <- sec$lengths
  durs2             <- durs
  durs2[spells != refvar] <- NA

  # need first or last position of each target spell:
  if (entry){
    # first step in spell:
    ind <- cumsum(durs) - durs + 1
  } else {
    ind <- cumsum(durs)
  }
  
  #n_spells          <- sum(spells == state)
  spell_x            <- rep(NA, length(x))
  spell_x[ind]       <- durs2

  # adjust to step_size
  spell_x * step_size
}



#' impute time spent in episodes of specified reference state
#' @description produce a vector of \code{length(x)} values corresponding to the time spent in a given episode of the reference state. 
#' @details Values are returned as midpoints. \code{NA} are given for other states. \code{state} can be a vector of states if you'd rather consider certain states merged into single episodes.
#' 
#' @inheritParams clock
#' @export
#' @examples
#' x <- rep(c("Employed", "Inactive", "Retired", "Employed", "Retired", 
#' 				"Dead"), c(7,  8,  3,  3, 25,  5))
#' # a single 8-year spell of inactivity, counting upwards from left
#' spell_step_increasing(x, "Inactive")
#' # merges first consecutive employment and inactivity spells into a single spell,
#' # also catches second employment after retirement
#' spell_step_increasing(x, c("Inactive","Employed"))

#' # total remaining lifespan at 50
#' spell_step_increasing(x, c("Inactive","Employed","Retired"))
spell_step_increasing <- function(x, state = "Inactive", step_size = 1){
	
	# starting left x position for each observation
	x_age             <- 1:length(x) - 1
	names(x_age)      <- x
	
	# combine states if necessary
	refvar            <- "REFSTATE"
	x[x %in% state]   <- refvar
	
	sec               <- rle(x)
	spells            <- sec$values
	durs              <- sec$lengths
	durs2             <- durs
	durs2[spells != refvar] <- NA
	
	n          <- length(durs)
	x_lefts    <- cumsum(c(0,durs[-n]))
	x_lefts[spells != refvar] <- NA          
	# if (not_first){
	# 	if (refvar %in% x){
	# 		x_lefts[which(spells == refvar)[1]] <- NA
	# 	}
	# }
	
	#n_spells          <- sum(spells == state)
	spell_starts         <- rep(x_lefts, durs)
	time_spent           <- x_age - spell_starts
	#spell_age[x != state] <- NA
	names(time_spent)    <- x_age
	
	time_spent[is.na(x)] <- NA
	(time_spent + .5) * step_size
}

#' impute time left in episodes of specified reference state
#' @description produce a vector of \code{length(x)} values corresponding to the time left in a given episode of the reference state. 
#' @details Values are returned as midpoints. \code{NA} are given for other states. \code{state} can be a vector of states if you'd rather consider certain states merged into single episodes.
#' 
#' @inheritParams clock
#' @export
#' @examples
#' x <- rep(c("Employed", "Inactive", "Retired", "Employed", "Retired", 
#' 				"Dead"), c(7,  8,  3,  3, 25,  5))
#' # a single 8-year spell of inactivity, counting down from left
#  spell_step_decreasing(x, "Inactive", step_size = 1)
#' # merges first consecutive employment and inactivity spells into a single spell,
#' # also catches second employment after retirement
#' spell_step_decreasing(x, c("Inactive","Employed"))
#' # catches just last employment spell
#' spell_step_decreasing(x, c("Inactive","Employed","Retired"))
spell_step_decreasing <- function(x, state = "Inactive",  step_size = 1){

	# starting left x position for each observation
	x_age             <- (1:length(x) - 1) 
	names(x_age)      <- x
	
	# combine states if necessary
	refvar            <- "REFSTATE"
	x[x %in% state]   <- refvar
	
	sec               <- rle(x)
	spells            <- sec$values
	durs              <- sec$lengths
	durs2             <- durs
	durs2[spells != refvar] <- NA
	
	n          <- length(durs)
	x_lefts    <- cumsum(c(0,durs[-n]))
	x_rights   <- x_lefts + durs
	x_rights[spells != refvar] <- NA  
	
	# TR, check flawed logic: not all first episodes, just those
	# that touch the left side.
	# if (not_first){
	# 	if (refvar %in% x){
	# 		x_rights[which(spells == refvar)[1]] <- NA
	# 	}
	# }
	
	#n_spells           <- sum(spells == state)
	spell_ends          <- rep(x_rights, durs)
	time_left           <- spell_ends - x_age
	#spell_age[x != state] <- NA
	names(time_left)    <- x_age
	time_left[is.na(x)] <- NA
	# midpoint time, then adjust for step_size
	(time_left - .5) * step_size
}

#' impute episode order for episodes of a given reference state
#' @description produce a vector of \code{length(x)} values corresponding to the order of each episode of the reference state. 
#' @details \code{NA} are given for other states. \code{state} can be a vector of states if you'd rather consider certain states merged into single episodes.
#' 
#' @inheritParams clock
#' @export
#' @examples
#' x <- rep(c("Employed", "Inactive", "Retired", "Employed", "Retired", 
#' 				"Dead"), c(7,  8,  3,  3, 25,  5))
#' # only one spell of inactivity
#' spell_order(x, state = "Inactive", increasing = TRUE)
#' # two spells of employment
#' spell_order(x, state = "Employed", increasing = TRUE)
#' spell_order(x, state = "Employed", increasing = FALSE)

spell_order <- function(x, 
                        state = "Inactive", 
                        increasing = TRUE, 
                        step_size = 1, 
                        condition = c("total","entry","exit")){
  condition         <- match.arg(condition)
  
	x_age             <- (1:length(x) - 1) * step_size
	names(x_age)      <- x
	
	# combine states if necessary
	refvar            <- "REFSTATE"
	x[x %in% state]   <- refvar
	
	sec               <- rle(x)
	spells            <- sec$values
	
	if (increasing){
		order1            <- cumsum(spells == refvar) 
	} else {
		order1            <- rev(cumsum(rev(spells == refvar))) 
	}
	
	order1[!spells %in% refvar] <- NA
	
	durs              <- sec$lengths
  out               <- rep(order1, durs)
	names(out)        <- x_age
	
	# TR: adding in conditional on entry option
	if (condition == "entry"){
	  out1     <- out
	  # first step in spell:
	  ind      <- cumsum(durs) - durs + 1
	  ind      <- ind[!is.na(out1[ind])]
	  out      <- NA * out
	  out[ind] <- out1[ind]
	} 
	if (condition == "exit"){
	  out1     <- out
	  # last step in spell:
	  ind      <- cumsum(durs)
	  ind      <- ind[!is.na(out1[ind])]
	  out      <- NA * out
	  out[ind] <- out1[ind]
	}

	out
}

# TR: clock works for a single trajectory, ok. how to deal w censoring?
# two args left_censor (logical) and right_censor (logical) ? In that case,
# would need to be applied differently for step, duration, and order clocks.

# Step clocks censoring behavior:

# clock_type = step, left_censor = TRUE, increasing = TRUE
#    *if very first step is state, then NA first spell
# clock_type = step, left_censor = TRUE, increasing = FALSE 
#    *nothing to do.
# clock_type = step, right_censor = TRUE, increasing = TRUE
#    *nothing to do.
# clock_type = step, right_censor = TRUE, increasing = FALSE 
#    *if very last step is state, then NA last spell

# -----------------------
# Duration clock censoring behavior:

# clock_type = duration, left_censor = TRUE
#    *if very first step is state, then NA first spell
# clock_type = duration, right_censor = TRUE
#    *if very last step is state, then NA last spell

# ------------------------
# Order clock censoring behavior:

# clock_type = order, left_censor = TRUE, increasing = TRUE
#    *NA whole series?
# clock_type = order, left_censor = TRUE, increasing = FALSE 
#    *nothing to do.
# clock_type = order, right_censor = TRUE, increasing = TRUE
#    *nothing to do.
# clock_type = order, right_censor = TRUE, increasing = FALSE 
#    *NA whole series?


#' @title impute clock measures
#' @description Given a discrete trajectory, impute clock measures to a given reference state or set of states (treating them ar merged). Clocks include 1) step clocks, those that count up from the start of an episode or down toward the end of it. 2) duration clocks, which record the total episode duration in each time step within the episode. 3) order clocks, which record the episode order in each time step within episodes, and which either count up or down.
#' @details Since sometimes we deal with left-censoring, step and duration measures have an option argument \code{not_first} to throw out counting within the very first episode (which may or may not be the reference episode). The argument \code{step_size} is only relevant for duration and step clocks. Discrete time intervals are assumed equal. 
#' 
#' States can be merged by specifying a vector of state names. To merge all states, one can also specify \code{state = "ALL"}.
#' 
#' @param x character vector of state in each time step
#' @param state character. The reference state. Could be a vector of states too.
#' @param clock_type character, one of \code{"step"}, \code{"duration"}, or \code{"order"}
#' @param increasing logical. Default \code{TRUE}. If \code{clock_type} is either \code{"step"} or \code{"order"} do we count up or count down?
#' @param condition character. For duration clocks, one of \code{"total"}, \code{"entry"}, or \code{"exit"}. Default \code{"total"}.
#' @param not_first logical. Shall we ignore the first episode of the given state? Default \code{FALSE}
#' @param step_size numeric. Default \code{1}. What is the time interval for the discrete bins in \code{x}.
#' @param dead_state state name used for the absorbing state
#' @export
#' @examples 
#' x <- rep(c("Employed", "Inactive", "Retired", "Employed", "Retired", 
#' 				"Dead"), c(7,  8,  3,  3, 25,  5))
#' # a single 8-year spell of inactivity, counting up
#' clock(x, "Inactive", clock_type = "step", increasing = TRUE)
#' # a single 8-year spell of inactivity, counting down
#' clock(x, "Inactive", clock_type = "step", increasing = FALSE)
#' # two, employment spells, each with its own clock
#' clock(x, "Employed", clock_type = "step", increasing = TRUE)
#' # two, employment spells, throw out first because left censored
#' clock(x, "Employed", clock_type = "step", increasing = TRUE, not_first = TRUE)
#' # but no need to throw out first if counting down:
#' clock(x, "Employed", clock_type = "step", increasing = FALSE, not_first = FALSE)
#' # for total durations we do want to throw out the first spell if left censored
#' clock(x, "Employed", clock_type = "duration", not_first = TRUE)
#' # TODO: pathological case: inactivity spells not left censored. Need better ID.
#' clock(x, "Inactivity", clock_type = "duration", not_first = TRUE)#
#' 
#' # total duration at entry or exit of spell:
#' clock(x, "Inactivity", clock_type = "duration", condition = "entry", not_first = TRUE)
#' clock(x, "Inactivity", clock_type = "duration", condition = "exit", not_first = TRUE)
#' # merges first consecutive employment and inactivity spells into a single spell,
#' # also catches second employment after retirement
#' clock(x, c("Inactive","Employed"), clock_type = "step", increasing = FALSE, not_first = FALSE)
#' # count down spell order
#' clock(x, c("Employed"), clock_type = "order", increasing = FALSE, not_first = FALSE)
#' # again w merged states
#' clock(x, c("Inactive","Employed"), clock_type = "order", increasing = FALSE, not_first = FALSE)
#' clock(x, c("Employed"), clock_type = "order", increasing = FALSE, not_first = FALSE)
#' # total lifespan after 50
#' 
#' clock(x, state = c("Inactive","Employed","Retired"), clock_type = "duration", not_first = FALSE)
#' # shortcut for the same:
#' clock(x, "ALL", clock_type = "duration", increasing = FALSE, not_first = FALSE)
#' # remaining lifespan
#' clock(x, "ALL", clock_type = "step", increasing = FALSE, not_first = FALSE)
clock <- function(x, 
                  state, 
                  clock_type = c("step", "duration", "order"), 
                  increasing = TRUE, 
                  condition = c("total","entry","exit"),
                  not_first = FALSE,
                  step_size = 1,
                  dead_state = "Dead"){
  
  # top level best place to throw out first episode, no need to control
  # this within specific methods.
  if (not_first){
    x[1:rle(x)$length[1]] <- NA
  }
  
  # don't let NAs mess things up
  x[is.na(x)] <- "XXXXXXXX"
  
  if (length(state) == 1){
    if (state == "ALL"){
      x[x != dead_state] <- "REFSTATE"
      state              <- "REFSTATE"
    }
  }
  
  clock_type    <- match.arg(clock_type)
  condition <- match.arg(condition)
  # --------------------------
  # step increasing or decreasing
  if (clock_type == "step"){
    if (increasing){
      out <- spell_step_increasing(
               x = x, 
               state = state, 
               step_size = step_size)
    } else {
      out <- spell_step_decreasing(
               x = x, 
               state = state, 
               step_size = step_size)
    }
  }
  
  # --------------------------
  # total duration (increasing ignored)
  if (clock_type == "duration"){
    if (condition == "total"){
      out <- spell_dur(
               x = x, 
               state = state, 
               step_size = step_size)
    }
    if (condition == "entry"){
      out <- spell_dur_conditional(
               x = x, 
               state = state, 
               entry = TRUE, 
               step_size = step_size)
    }
    if (condition == "exit"){
      out <- spell_dur_conditional(
               x = x, 
               state = state, 
               entry = FALSE, 
               step_size = step_size)
    }
  }
  
  # --------------------------
  # spell order
  if (clock_type == "order"){
    out <- spell_order(
             x = x,
             state = state,
             increasing = increasing, 
             step_size = step_size,
             condition = condition)
  }
  
  # --------------------------
  out
}

# clock(x, "Inactive","step",TRUE,1)
