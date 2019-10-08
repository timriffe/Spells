
# Author: tim
###############################################################################


library(here)
source(here("Spells","R","GenerateStationary.R"))
# source("R/Counting.R")
# source("R/Distributions.R")
# source("R/Align.R")

ls()

dim(RTraj_clean)
# 51 age classes (50-100)
# 10k trajectories.

# type "center" "right" "last"
# spell "first" "last" "longest"
# state "Inactive" "Employed" "Retired" "Dead"
unique(RTraj_clean[5,])



RTraj_clean[,1]
args(align)

x <- 50:100
q <- seq(.05,.95,by=.05)

# look at spell duration of inactivity.
# Countring.R has spell_durAge()     (total dur, age can be any alignment)
#                 spell_dur_before() (time spent)
#                 spell_dur_after()  (time left)

# so, can take average spell duration of inactivity as function of 
# time since end of first employment, for example:

rightfirstemply           <- apply(RTraj_clean, 2, align, spell = "first", state = "Employed", type = "right")

colnames(rightfirstemply) <- 1:ncol(rightfirstemply)
colnames(RTraj_clean)     <- 1:ncol(RTraj_clean)
rownames(rightfirstemply) <- x
rownames(RTraj_clean)     <- x

# get into long format
library(reshape2)
LongTime  <- melt(rightfirstemply, 
		   varnames = c("Age","ID"), 
		   value.name = "TimeSinceEndOfFirstEmploy")
LongState  <- melt(RTraj_clean, 
		   varnames = c("Age","ID"), 
		   value.name = "State")
 
LongState <- merge(LongTime, LongState)
LongState$State <- as.character(LongState$State)
LongState$State[LongState$State == "Dead"] <- NA


Xaligned <- acast(LongState, TimeSinceEndOfFirstEmploy ~ ID, value.var = "State", fill = NA)



# now get spell duration object
dur      <- apply(Xaligned,2,clock, clock_type = "duration", state = "Inactive")
durleft  <- apply(Xaligned,2,clock,clock_type="step",increasing=FALSE, state = "Inactive")
durspent <- apply(Xaligned,2,clock,clock_type="step",increasing=TRUE, state = "Inactive")


xnew     <- as.integer(rownames(Xaligned))


pdf("Figures/Macro1.pdf")
par(mai=c(1,1,.0,0))
plot(xnew, rowMeans(dur, na.rm=TRUE), type = 'l', ylim = c(0,25), xlim = c(0,25),
		xlab = "Time since first employment exit", ylab = "Avg duration")
lines(xnew, rowMeans(durleft, na.rm=TRUE), col = "blue")
lines(xnew, rowMeans(durspent, na.rm=TRUE), col = "red")
text(10,9,"Total duration",cex=1.5)
text(9,6,"Time spent",col="red",pos=2,cex=1.5)
text(10,2,"Time left",col="blue",pos=4,cex=1.5)
dev.off()
#lines(xnew, qdens(dur,.5), lwd = 2)
#lines(xnew, qdens(durleft, .5), lwd = 2, col = "blue")
#lines(xnew, qdens(durspent, .5), lwd = 2, col = "red")
#
alphas = seq(.1, .9, by = .1)
lp    <- alphas / 2
up    <- 1 - alphas / 2
p = sort(c(lp, up))
q     <- qdens(dur,p = sort(c(lp, up)))

apply(dur,1,function(x,p){
  if (sum(!is.na(x))>5){
    return(spatstat::quantile.density(stats::density(x,na.rm=TRUE),probs=p))
  } else {
    return(rep(NA, length(p)))
  }
}, p = p)


n     <- length(alphas) * 2
for (i in 1:length(alphas)){
  intervalpoly(xnew,l = q[i, ],u = q[n - i + 1,], col = "#00000010")
}
xnewid <- xnew >= 0 & xnew <= 24

pdf("Figures/Macro2.pdf")
par(mai=c(1,1,.0,0))
plot(xnew, rowMeans(dur, na.rm=TRUE), type = 'l', ylim = c(0,25), xlim = c(0,25),
		xlab = "Time since first employment exit", ylab = "Avg duration")
intervalfan(dur[xnewid,],x=xnew[xnewid], col = "#00000010", border = NA)
lines(xnew, qdens(dur,.5), lwd = 2)
dev.off()

pdf("Figures/Macro3.pdf")
par(mai=c(1,1,.0,0))
plot(xnew, rowMeans(durleft, na.rm=TRUE), type = 'l', ylim = c(0,25), xlim = c(0,25), col = "blue",
		xlab = "Time since first employment exit", ylab = "Remaining time in spell")
intervalfan(durleft,x=xnew, col = "#0000FF10", border = NA)
lines(xnew, qdens(durleft, .5), lwd = 2, col = "blue")
dev.off()

pdf("Figures/Macro4.pdf")
par(mai=c(1,1,.0,0))
plot(xnew, rowMeans(durspent, na.rm=TRUE), type = 'l', ylim = c(0,25), xlim = c(0,25), col = "red",
		xlab = "Time since first employment exit", ylab = "Time spent in spell")
intervalfan(durspent,x=xnew, col = "#FF000010", border = NA)
lines(xnew, qdens(durspent,.5), lwd = 2,col="red")
dev.off()