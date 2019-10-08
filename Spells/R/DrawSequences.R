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
