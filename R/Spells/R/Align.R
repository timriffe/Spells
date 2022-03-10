
#' shift lower time interval bounds of state sequence
#' @description left, right, or center align a sequence on the first, last, or longest spell of a specified state.
#' @details Time steps of the sequence are assumed uniformly equal to 1.
#' @param x character vector representing a state sequence
#' @param state character string, the reference state.
#' @param type character one of \code{"left"}, \code{"right"}, or \code{"center"}
#' @param spell character one of \code{"first"}, \code{"last"}, or \code{"longest"}
#' @param step_size numeric. uniform size of a time step. default 1.
#' @return lower time interval bounds after shifting, and assuming unit time steps.
#' @export
#' @examples 
#'  set.seed(1)
#' x <- c(sample(c("A","I"),size=10,replace = TRUE, prob = c(.8,.2)),
#'         sample(c("A","I","R"),size=15, replace = TRUE, prob = c(.1,.2,.7)),
#'         rep("R",5),rep("D",5))
#' align(x,state = "A", type = "left", spell = "first")
#' align(x,state = "A", type = "right", spell = "last")
#' align(x,state = "A", type = "center", spell = "longest")
#' align(x,state = "R", type = "left", spell = "first")

align <- function(x, 
                  state = "Inactive", 
                  type = c("left", "right", "center"), 
                  spell = c("first", "last", "longest"),
                  step_size = 1){
  
  # validate arguments
  spell             <- match.arg(spell)
  type              <- match.arg(type)
  
	# starting left x position for each observation
	x_left_all        <- (1:length(x) - 1) * step_size
	names(x_left_all) <- x
	
	# combine states if necessary
	refvar            <- "REFSTATE"
	x[x %in% state]   <- refvar
	if (refvar %in% x){
		# now get spell info
		sec        <- rle(x)
		spells     <- sec$values
		durs       <- sec$lengths * step_size
		n          <- length(durs)
		x_lefts    <- cumsum(c(0,durs[-n])) * step_size
		x_rights   <- x_lefts + durs
		
		# which is the reference spell? 3 choices
		if (spell == "first"){
			this.spell <- which(spells == refvar)[1]
		}
		if (spell == "longest"){
			s.spells   <- which(spells == refvar)
			this.spell <- s.spells[which.max(durs[s.spells])]
		}
		if (spell == "last"){
			this.spell <- rev(which(spells == refvar))[1]
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


