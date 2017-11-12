
# Author: tim
###############################################################################
#' @description produce a vector of \code{length(x)} values corresponding to the total duration of each episode of the reference state. 
#' @details \code{NA} are given for other states. \code{state} can be a vector of states if you'd rather consider certain states merged into single episodes.
#' 
#' @param x character vector of state in each time step
#' @param state character. The reference state. Could be a vector of states too.
#' @param not_first logical. Shall we ignore the first episode of the given state? Default \code{FALSE}
#' @export
#' @examples
#' x <- rep(c("Employed", "Inactive", "Retired", "Employed", "Retired", 
#' 				"Dead"), c(7,  8,  3,  3, 25,  5))
#' # a single 8-year spell of inactivity
#' spell_durAge(x, "Inactive", not_first = FALSE)
#' # catches nothing since there was only one spell of inactivity
#' spell_durAge(x, "Inactive", not_first = TRUE)
#' # merges first consecutive employment and inactivity spells into a single spell,
#' # also catches second employment after retirement
#' spell_durAge(x, c("Inactive","Employed"), not_first = FALSE)
#' # catches just last employment spell
#' spell_durAge(x, c("Inactive","Employed"), not_first = TRUE)
#' # total lifespan 50+
#' spell_durAge(x, c("Inactive","Employed","Retired"), not_first = FALSE)

spell_durAge <- function(x, state = "Inactive", not_first = FALSE){
	
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
	if (not_first){
		if (refvar %in% x){
			durs2[which(spells == refvar)[1]] <- NA
		}
		
	}
	
	#n_spells          <- sum(spells == state)
	spell_age         <- rep(durs2, durs)
	#spell_age[x != state] <- NA
	spell_age[is.na(x)] <- NA
	
	spell_age
}
#' @description produce a vector of \code{length(x)} values corresponding to the time spent in a given episode of the reference state. 
#' @details Values are returned as midpoints. \code{NA} are given for other states. \code{state} can be a vector of states if you'd rather consider certain states merged into single episodes.
#' 
#' @param x character vector of state in each time step
#' @param state character. The reference state. Could be a vector of states too.
#' @param not_first logical. Shall we ignore the first episode of the given state? Default \code{FALSE}
#' @export
#' @examples
#' x <- rep(c("Employed", "Inactive", "Retired", "Employed", "Retired", 
#' 				"Dead"), c(7,  8,  3,  3, 25,  5))
#' # a single 8-year spell of inactivity, counting upwards from left
#' spell_dur_before(x, "Inactive", not_first = FALSE)
#' # catches nothing since there was only one spell of inactivity
#' spell_dur_before(x, "Inactive", not_first = TRUE)
#' # merges first consecutive employment and inactivity spells into a single spell,
#' # also catches second employment after retirement
#' spell_dur_before(x, c("Inactive","Employed"), not_first = FALSE)
#' # catches just last employment spell
#' spell_dur_before(x, c("Inactive","Employed"), not_first = TRUE)
#' # total lifespan 50+
#' spell_dur_before(x, c("Inactive","Employed","Retired"), not_first = FALSE)

spell_dur_before <- function(x, state = "Inactive", not_first = FALSE){
	
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
	if (not_first){
		if (refvar %in% x){
			x_lefts[which(spells == refvar)[1]] <- NA
		}
	}
	
	#n_spells          <- sum(spells == state)
	spell_starts         <- rep(x_lefts, durs)
	time_spent           <- x_age - spell_starts
	#spell_age[x != state] <- NA
	names(time_spent)    <- x_age
	
	time_spent[is.na(x)] <- NA
	time_spent + .5
}
#' @description produce a vector of \code{length(x)} values corresponding to the time left in a given episode of the reference state. 
#' @details Values are returned as midpoints. \code{NA} are given for other states. \code{state} can be a vector of states if you'd rather consider certain states merged into single episodes.
#' 
#' @param x character vector of state in each time step
#' @param state character. The reference state. Could be a vector of states too.
#' @param not_first logical. Shall we ignore the first episode of the given state? Default \code{FALSE}
#' @export
#' @examples
#' x <- rep(c("Employed", "Inactive", "Retired", "Employed", "Retired", 
#' 				"Dead"), c(7,  8,  3,  3, 25,  5))
#' # a single 8-year spell of inactivity, counting down from left
#' spell_dur_before(x, "Inactive", not_first = FALSE)
#' # catches nothing since there was only one spell of inactivity
#' spell_dur_before(x, "Inactive", not_first = TRUE)
#' # merges first consecutive employment and inactivity spells into a single spell,
#' # also catches second employment after retirement
#' spell_dur_before(x, c("Inactive","Employed"), not_first = FALSE)
#' # catches just last employment spell
#' spell_dur_before(x, c("Inactive","Employed"), not_first = TRUE)
#' # total lifespan 50+
#' spell_dur_before(x, c("Inactive","Employed","Retired"), not_first = FALSE)
spell_dur_after <- function(x, state = "Inactive", not_first = FALSE){

	
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
	x_rights   <- x_lefts + durs
	x_rights[spells != refvar] <- NA          
	if (not_first){
		if (refvar %in% x){
			x_rights[which(spells == refvar)[1]] <- NA
		}
	}
	
	#n_spells          <- sum(spells == state)
	spell_ends           <- rep(x_rights, durs)
	time_left            <- spell_ends - x_age
	#spell_age[x != state] <- NA
	names(time_left)    <- x_age
	time_left[is.na(x)] <- NA
	time_left - .5
}

#' @description produce a vector of \code{length(x)} values corresponding to the order of each episode of the reference state. 
#' @details \code{NA} are given for other states. \code{state} can be a vector of states if you'd rather consider certain states merged into single episodes.
#' 
#' @param x character vector of state in each time step
#' @param state character. The reference state. Could be a vector of states too.
#' @param not_first logical. Shall we ignore the first episode of the given state? Default \code{FALSE}
#' @export
#' @examples
#' x <- rep(c("Employed", "Inactive", "Retired", "Employed", "Retired", 
#' 				"Dead"), c(7,  8,  3,  3, 25,  5))
#' # only one spell of inactivity
#' spell_order(x, state = "Inactive", increasing = TRUE)
#' # two spells of employment
#' spell_order(x, state = "Employed", increasing = TRUE)
#' spell_order(x, state = "Employed", increasing = FALSE)

spell_order <- function(x, state = "Inactive", increasing = TRUE){
	x_age             <- 1:length(x) - 1
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
	out
}