
#' estimate quantiles from a matrix of sequences
#' @description Within a time step, first fit a smooth density, then estimate a given quantile on the density.
#' @details if \code{p} is given as a vector, then the result is a matrix with percentiles in rows and time steps in columns.
#' @param X numeric matrix, with time steps in rows and individuals in columns
#' @param p desired quantiles
#' @return numeric vector (matrix) of the desired quantile(s) per time step.
#' @importFrom spatstat quantile.density
#' @importFrom stats density
#' @export
#' @examples 
#' X <- runif(5000)
#' dim(X) <- c(50,100)
#' qdens(X, p = .5)
#' qdens(X, p = c(.25, .75))
qdens <- function(X,p=.5){
	apply(X,1,function(x,p){
				if (sum(!is.na(x))>2){
					return(spatstat::quantile.density(stats::density(x,na.rm=TRUE),probs=p))
				} else {
					return(NA)
				}
			}, p = p)
}

#' draw a polygon, used for interval fans
#' @description Draw a confidence band
#' @details upper and lower bounds must have the same time steps in \code{x}
#' @param x numeric vector of time steps
#' @param u numeric vector of upper bound
#' @param l numeric vector of lower bound
#' @param ... optional arguments to pass to \code{polygon()}
#' @return function renders a polygon in the graphics device.
#' @export
intervalpoly <- function(x,u,l,...){
	una <- is.na(u)
	lna <- is.na(l)
	keep <- !(una | lna)
	polygon(c(x[keep],rev(x[keep])),
			c(u[keep],rev(l[keep])),...)
}

#' draw a polygon, used for interval fans
#' @description Draw a confidence band
#' @details upper and lower bounds must have the same time steps in \code{x}
#' @param x numeric vector of time steps
#' @param u numeric vector of upper bound
#' @param l numeric vector of lower bound
#' @param ... optional arguments to pass to \code{polygon()}
#' @return function renders a polygon in the graphics device.
#' @export
#' @examples 
#' X <- runif(5000)
#' dim(X) <- c(50,100)
#' plot(NULL, type="n",xlim=c(0,50),ylim = c(0,1))
#' intervalfan(X,x=0:49,alphas=seq(.1, .9, by = .1),col = "#FF000020")
intervalfan <- function(X, x = 50:100, alphas = seq(.1, .9, by = .1), ...){
  lp    <- alphas / 2
  up    <- 1 - alphas / 2
  q     <- qdens(X,p = sort(c(lp, up)))
  n     <- length(alphas) * 2
	for (i in 1:length(alphas)){
		intervalpoly(x,l = q[i, ],u = q[n - i + 1,], ...)
	}
}
