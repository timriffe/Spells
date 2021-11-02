# These extracted from FiguresSeq.R so that they can be sourced in Rmarkdown presentation as well.
# Probably ought to clean these up since they will be used in future presentations.

draw_sequence2 <- function(state_seq, states, cols, y = 0,...){
  xvals       <- 1:length(state_seq) - 1 + 50
  names(cols) <- states
  col_seq     <- cols[state_seq]
  
  rect(xvals,y,xvals+1,y+1,col=col_seq,...)
}
draw_sequence3 <- function(durs, y = 0,...){
  xvals             <- 1:length(durs) - 1 + 50	
  durs[is.na(durs)] <- ""
  text(xvals+.5,y+.5,durs,...)
}
# alignment
draw_sequence4 <- function(state_seq, x, states, cols, y = 0,...){
  names(cols) <- states
  col_seq     <- cols[state_seq]
  rect(x,y,x+1,y+1,col=col_seq,...)
}

# ---------------------------------------------------------- #
# Clean versions, to be in used in future presentations      #
# and in order to have a consistent style in the manuscript. #
# ---------------------------------------------------------- #

#' draw a sequence, for examples
#' @description draws a single sequence using base functions. For use in illustrative diagrams
#' @details if you want to indicate a cesored spell, then you need to include a color for that as well. Since a seqence or a spell can be censored, it may as well be a nice thing to add to the color palette in general, so that it kicks in faithefully whether for individual time steps or for the full sequence. 
#' 
#' We assume the trajectory height is equal to 1, and also that time steps are of width 1.
#' 
#' If \code{box}is \code{TRUE} then any state not equal to \code{"Dead"} is included in the box. Non-flexible aesthetics, sorry.
#'
#' @param state_seq character vector with state names in each time step
#' @param x left bound of each time step, possibly after alignment
#' @param labels vector of time step labels, potentially after clocking
#' @param cols vector of colors, with names equal to state space. 
#' @param y height at which to draw the sequence, assumed 1 unit tall.
#' @param box logical do we draw a box outline around the whole sequence?
#' @param ... optional arguments passed to \code{rect()} or \copde{text()}
#'
#'@examples
#'\dontrun{
#' library(Spells)
#' data(X)
#' library(colorspace)
#' cols  <- c(qualitative_hcl(5, palette = "Dark 3")[c(3,2,4)],NA)
#' cols2 <- lighten(desaturate(cols,.3),.3)
#' names(cols) <- names(cols2) <- c("Employed", "Inactive","Retired","Dead")
#' 
#' plot(NULL, 
#'      type = "n", 
#'      xlim = c(-50,0), 
#'      ylim = c(0,12), 
#'      axes = FALSE, 
#'      xlab = "", 
#'      ylab = "")
#' for (i in 1:10){
#'   draw_sequence(X[,i], 
#'                 x = align(X[,i], 
#'                           state = "Dead",
#'                           type = "left"),
#'                 labels = clock(X[,i], 
#'                                state = "Retired", 
#'                                clock_type = "step", 
#'                                increasing = FALSE),
#'                 cols = cols2,
#'                 box = TRUE,
#'                 y = i,
#'                 border = NA,
#'                 cex=.5
#'                 )
#' }
#' 
#' }

draw_sequence <- function(state_seq, 
              x = 1:length(state_seq)-1, 
              labels, 
              cols,  
              y = 0, 
              box = TRUE,
              ...){
  
  col_seq     <- cols[state_seq]
  rect(x, y, x + 1, y + 1, col = col_seq, ...)
  
  if (!missing(labels)){
    suppressWarnings(text(x + .5, y + .5, labels, ...))
  }
  
  # draw a light border around the whole sequence.
  if (box){
    rect(x[1], y, x[which(state_seq == "Dead")[1]],
         y + 1,
         border = gray(.4), lwd = .5)
  }
}















