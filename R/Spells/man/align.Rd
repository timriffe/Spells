% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Align.R
\name{align}
\alias{align}
\title{shift lower time interval bounds of state sequence}
\usage{
align(
  x,
  state = "Inactive",
  type = c("left", "right", "center"),
  spell = c("first", "last", "longest"),
  step_size = 1
)
}
\arguments{
\item{x}{character vector representing a state sequence}

\item{state}{character string, the reference state.}

\item{type}{character one of \code{"left"}, \code{"right"}, or \code{"center"}}

\item{spell}{character one of \code{"first"}, \code{"last"}, or \code{"longest"}}

\item{step_size}{numeric. uniform size of a time step. default 1.}
}
\value{
lower time interval bounds after shifting, and assuming unit time steps.
}
\description{
left, right, or center align a sequence on the first, last, or longest spell of a specified state.
}
\details{
Time steps of the sequence are assumed uniformly equal to 1.
}
\examples{
 set.seed(1)
x <- c(sample(c("A","I"),size=10,replace = TRUE, prob = c(.8,.2)),
        sample(c("A","I","R"),size=15, replace = TRUE, prob = c(.1,.2,.7)),
        rep("R",5),rep("D",5))
align(x,state = "A", type = "left", spell = "first")
align(x,state = "A", type = "right", spell = "last")
align(x,state = "A", type = "center", spell = "longest")
align(x,state = "R", type = "left", spell = "first")
align(x,state = "R", type = "left", spell = "first", step_size = .25)
}
