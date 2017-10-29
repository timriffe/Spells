
# Author: tim
###############################################################################


spell_durAge <- function(x, state = "Inactive", not_first = FALSE){
	
	# starting left x position for each observation
	x_age             <- 1:length(x) - 1
	names(x_age)      <- x
	
	sec               <- rle(x)
	spells            <- sec$values
	durs              <- sec$lengths
	durs2             <- durs
	durs2[spells != state] <- NA
	if (not_first){
		if (state %in% x){
			durs2[which(spells == state)[1]] <- NA
		}
		
	}
	
	#n_spells          <- sum(spells == state)
	spell_age         <- rep(durs2, durs)
	#spell_age[x != state] <- NA
	
	spell_age
}

spell_dur_before <- function(x, state = "Inactive", not_first = FALSE){
	
	# starting left x position for each observation
	x_age             <- 1:length(x) - 1
	names(x_age)      <- x
	
	sec               <- rle(x)
	spells            <- sec$values
	durs              <- sec$lengths
	durs2             <- durs
	durs2[spells != state] <- NA
	
	n          <- length(durs)
	x_lefts    <- cumsum(c(0,durs[-n]))
	x_lefts[spells != state] <- NA          
	if (not_first){
		if (state %in% x){
			x_lefts[which(spells == state)[1]] <- NA
		}
	}
	
	#n_spells          <- sum(spells == state)
	spell_starts         <- rep(x_lefts, durs)
	time_spent           <- x_age - spell_starts
	#spell_age[x != state] <- NA
	names(time_spent)    <- x_age
	time_spent + .5
}
spell_dur_after <- function(x, state = "Inactive", not_first = FALSE){
	
	# starting left x position for each observation
	x_age             <- 1:length(x) - 1
	names(x_age)      <- x
	
	sec               <- rle(x)
	spells            <- sec$values
	durs              <- sec$lengths
	durs2             <- durs
	durs2[spells != state] <- NA
	
	n          <- length(durs)
	x_lefts    <- cumsum(c(0,durs[-n]))
	x_rights   <- x_lefts + durs
	x_rights[spells != state] <- NA          
	if (not_first){
		if (state %in% x){
			x_rights[which(spells == state)[1]] <- NA
		}
	}
	
	#n_spells          <- sum(spells == state)
	spell_ends           <- rep(x_rights, durs)
	time_left            <- spell_ends - x_age
	#spell_age[x != state] <- NA
	names(time_left)    <- x_age
	time_left - .5
}


