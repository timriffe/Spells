
# Author: tim
###############################################################################


align <- function(x, state = "Inactive", type = "left", spell = "first"){
	
	# starting left x position for each observation
	x_left_all        <- 1:length(x) - 1
	names(x_left_all) <- x
	
	if (state %in% x){
		# now get spell info
		sec        <- rle(x)
		spells     <- sec$values
		durs       <- sec$lengths
		n          <- length(durs)
		x_lefts    <- cumsum(c(0,durs[-n]))
		x_rights   <- x_lefts + durs
		
		# which is the reference spell? 3 choices
		if (spell == "first"){
			this.spell <- which(spells == state)[1]
		}
		if (spell == "longest"){
			s.spells   <- which(spells == state)
			this.spell <- s.spells[which.max(durs[s.spells])]
		}
		if (spell == "last"){
			this.spell <- rev(which(spells == state))[1]
		}
		if (type == "left"){
			shift_time <- x_lefts[this.spell]
		}
		if (type == "right"){
			shift_time <- x_rights[this.spell]
		}
		if (type == "center"){
			shift_time <- (x_lefts[this.spell] +  x_rights[this.spell]) / 2
		}
	} else {
		shift_time <- 0
	}
	
	
	x_left_all - shift_time
}


