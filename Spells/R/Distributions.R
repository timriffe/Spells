
# Author: tim
###############################################################################
require(spatstat)
qdens <- function(X,p=.5){
	apply(X,1,function(x,p){
				if (sum(!is.na(x))>2){
					return(spatstat::quantile.density(density(x,na.rm=TRUE),prob=p))
				} else {
					return(NA)
				}
			},p=p)
}

intervalpoly <- function(x,u,l,...){
	una <- is.na(u)
	lna <- is.na(l)
	keep <- !(una | lna)
	polygon(c(x[keep],rev(x[keep])),
			c(u[keep],rev(l[keep])),...)
}

intervalfan <- function(X,x=50:100,probs = seq(.1,.9,by=.1),...){
	for (i in probs){
		alpha <- i / 2
		ua    <- 1- alpha
		la    <- alpha
		ql <- qdens(X,la)
		qu <- qdens(X,ua)
		intervalpoly(x,ql,qu,...)
	}
}
