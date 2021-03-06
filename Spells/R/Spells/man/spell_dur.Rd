% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Counting.R
\name{spell_dur}
\alias{spell_dur}
\title{impute durations of episodes in specified reference state}
\usage{
spell_dur(x, state = "Inactive", step_size = 1)
}
\arguments{
\item{x}{character vector of state in each time step}

\item{state}{character. The reference state. Could be a vector of states too.}

\item{step_size}{numeric. Default \code{1}. What is the time interval for the discrete bins in \code{x}.}
}
\description{
produce a vector of \code{length(x)} values corresponding to the total duration of each episode of the reference state.
}
\details{
\code{NA} are given for other states. \code{state} can be a vector of states if you'd rather consider certain states merged into single episodes.
}
\examples{
x <- rep(c("Employed", "Inactive", "Retired", "Employed", "Retired", 
				"Dead"), c(7,  8,  3,  3, 25,  5))
# a single 8-year spell of inactivity
spell_dur(x, "Inactive")
# merges first consecutive employment and inactivity spells into a single spell,
# also catches second employment after retirement
spell_dur(x, c("Inactive","Employed"))
# total remaining lifespan at 50
spell_dur(x, c("Inactive","Employed","Retired"))
}
