
# Author: tim
###############################################################################


set.seed(1)
setwd("/home/tim/git/Spells/Spells")
source("R/GenerateStationary.R")
source("R/Counting.R")
source("R/Distributions.R")
source("R/Align.R")



draw_sequence2 <- function(state_seq, states, cols, y = 0,...){
	xvals <- 1:length(state_seq) - 1 + 50
	names(cols) <- states
	col_seq     <- cols[state_seq]
	
	rect(xvals,y,xvals+1,y+1,col=col_seq,...)
}
draw_sequence3 <- function(durs, y = 0,...){
	xvals <- 1:length(durs) - 1 + 50	
	durs[is.na(durs)] <- ""
	text(xvals+.5,y+.5,durs,...)
}
# alignment
draw_sequence4 <- function(state_seq, x, states, cols, y = 0,...){
	
	names(cols) <- states
	col_seq     <- cols[state_seq]
	
	rect(x,y,x+1,y+1,col=col_seq,...)
}


cols   <- c("#74ee65", "#773129", "#41bbc5",NA)
states <- c("Employed", "Inactive","Retired","Dead")
yvals  <- 9:0 * 1.1


X <- RTraj_clean[,1:10]

pdf("DGD/Figures/Seq10.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,101), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
	draw_sequence2(X[,i],states,cols,y=yvals[i], border = NA)
}
axis(1)
text(50,yvals+.5,1:10,pos=2,xpd=TRUE)
text(47,6,"Random individual i",xpd=TRUE,srt=90)
legend(60,-2,fill = cols, legend = states[-4],horiz = TRUE,xpd=TRUE,bty="n")
dev.off()


# ---------------------------------------
# figure 2
# total duration.


Ones            <- X == "Inactive"
Ones[Ones]      <- 1
Ones[Ones == 0] <- NA

pdf("DGD/Figures/Seq10clock_prev.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,101), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
	draw_sequence3(Ones[,i],y=yvals[i])
	rect(50,yvals[i],(which(X[,i] == "Dead")[1]+49),yvals[i]+1,border = gray(.4),lwd=.5)
}
axis(1)
text(50,yvals+.5,1:10,pos=2,xpd=TRUE)
text(47,6,"Random individual i",xpd=TRUE,srt=90)
dev.off()




Dur <- apply(X,2,spell_durAge,state = "Inactive")

pdf("DGD/Figures/Seq10clock_dur.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,101), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
	draw_sequence3(Dur[,i],y=yvals[i])
	rect(50,yvals[i],(which(X[,i] == "Dead")[1]+49),yvals[i]+1,border = gray(.4),lwd=.5)
}
axis(1)
text(50,yvals+.5,1:10,pos=2,xpd=TRUE)
text(47,6,"Random individual i",xpd=TRUE,srt=90)
dev.off()

OrdUp   <- apply(X,2,spell_order,state = "Inactive",increasing=TRUE)
OrdDown <- apply(X,2,spell_order,state = "Inactive",increasing=FALSE)


pdf("DGD/Figures/Seq10clock_ordUp.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,101), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
	draw_sequence3(OrdUp[,i],y=yvals[i])
	rect(50,yvals[i],(which(X[,i] == "Dead")[1]+49),yvals[i]+1,border = gray(.4),lwd=.5)
}
axis(1)
text(50,yvals+.5,1:10,pos=2,xpd=TRUE)
text(47,6,"Random individual i",xpd=TRUE,srt=90)
dev.off()


pdf("DGD/Figures/Seq10clock_ordDown.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,101), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
	draw_sequence3(OrdDown[,i],y=yvals[i])
	rect(50,yvals[i],(which(X[,i] == "Dead")[1]+49),yvals[i]+1,border = gray(.4),lwd=.5)
}
axis(1)
text(50,yvals+.5,1:10,pos=2,xpd=TRUE)
text(47,6,"Random individual i",xpd=TRUE,srt=90)
dev.off()




# time spent


TimeSpent <- apply(X,2,spell_dur_before,state = "Inactive")

pdf("DGD/Figures/Seq10clock_timespent.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,101), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
	draw_sequence3(floor(TimeSpent)[,i],y=yvals[i])
	rect(50,yvals[i],(which(X[,i] == "Dead")[1]+49),yvals[i]+1,border = gray(.4),lwd=.5)
}
axis(1)
text(50,yvals+.5,1:10,pos=2,xpd=TRUE)
text(47,6,"Random individual i",xpd=TRUE,srt=90)
dev.off()


# cumulative time spent (need to add arg to function, but ad hoc for now)
tot <- colSums(!is.na(TimeSpent))
TimeSpentC <- TimeSpent * 0
for (i in 1:ncol(TimeSpentC)){
	ind <- !is.na(TimeSpentC[,i])
	TimeSpentC[ind, i] <- 0:(sum(ind)-1)
}
pdf("DGD/Figures/Seq10clock_timespentcumul.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,101), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
	draw_sequence3(TimeSpentC[,i],y=yvals[i])
	rect(50,yvals[i],(which(X[,i] == "Dead")[1]+49),yvals[i]+1,border = gray(.4),lwd=.5)
}
axis(1)
text(50,yvals+.5,1:10,pos=2,xpd=TRUE)
text(47,6,"Random individual i",xpd=TRUE,srt=90)
dev.off()



TimeLeft <- apply(X,2,spell_dur_after,state = "Inactive")

pdf("DGD/Figures/Seq10clock_timeleft.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,101), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
	draw_sequence3(floor(TimeLeft)[,i],y=yvals[i])
	rect(50,yvals[i],(which(X[,i] == "Dead")[1]+49),yvals[i]+1,border = gray(.4),lwd=.5)
}
axis(1)
text(50,yvals+.5,1:10,pos=2,xpd=TRUE)
text(47,6,"Random individual i",xpd=TRUE,srt=90)
dev.off()



# alignment
source("R/Align.R")
draw_sequence4 <- function(state_seq, x, states, cols, y = 0,...){
    
	names(cols) <- states
	col_seq     <- cols[state_seq]
	
	rect(x,y,x+1,y+1,col=col_seq,...)
}

pdf("DGD/Figures/Seq10aligncoords.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(40,110), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
	draw_sequence2(X[,i],states,cols,y=yvals[i], border = NA)
}
axis(1)
text(50,yvals+.5,1:10,pos=2,xpd=TRUE)
text(36,6,"Random individual i",xpd=TRUE,srt=90)
legend(60,-2,fill = cols, legend = states[-4],horiz = TRUE,xpd=TRUE,bty="n")
dev.off()


XdeathAlign <- apply(X,2,align,state = "Dead",type="left")
pdf("DGD/Figures/Seq10align_death.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(-80,0), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
	draw_sequence4(state_seq = X[,i],x=XdeathAlign[,i],states,cols,y=yvals[i], border = NA)
}
axis(1)
text(-80,yvals+.5,1:10,pos=2,xpd=TRUE)
text(-84,6,"Random individual i",xpd=TRUE,srt=90)
legend(-60,-2,fill = cols, legend = states[-4],horiz = TRUE,xpd=TRUE,bty="n")
dev.off()


# align on first retirement:
XretirefirstAlign <- apply(X,2,align,state = "Retired",type="left",spell = "first")
XretirelongAlign <- apply(X,2,align,state = "Retired",type="left",spell = "longest")

Xinactlongleft    <- apply(X,2,align,state = "Inactive",type="left",spell = "longest")
Xinactlongright   <- apply(X,2,align,state = "Inactive",type="right",spell = "longest")

pdf("DGD/Figures/Seq10align_firstret.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(-30,50), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
	if ("Retired" %in% X[,i]){
	draw_sequence4(state_seq = X[,i],x=XretirefirstAlign[,i],states,cols,y=yvals[i], border = NA)
}
}
axis(1)
text(min(XretirefirstAlign),yvals+.5,1:10,pos=2,xpd=TRUE)
text(min(XretirefirstAlign)-4,6,"Random individual i",xpd=TRUE,srt=90)
legend(-10,-2,fill = cols, legend = states[-4],horiz = TRUE,xpd=TRUE,bty="n")
dev.off()

pdf("DGD/Figures/Seq10align_longestret.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(-30,50), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
	if ("Retired" %in% X[,i]){
		draw_sequence4(state_seq = X[,i],x=XretirelongAlign[,i],states,cols,y=yvals[i], border = NA)	
	}
}
axis(1)
text(min(XretirelongAlign),yvals+.5,1:10,pos=2,xpd=TRUE)
text(min(XretirelongAlign)-4,6,"Random individual i",xpd=TRUE,srt=90)
legend(-10,-2,fill = cols, legend = states[-4],horiz = TRUE,xpd=TRUE,bty="n")
dev.off()

pdf("DGD/Figures/Seq10align_inactlongleft.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(-30,50), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
	draw_sequence4(state_seq = X[,i],x=Xinactlongleft[,i],states,cols,y=yvals[i], border = NA)
}
axis(1)
text(min(XretirefirstAlign),yvals+.5,1:10,pos=2,xpd=TRUE)
text(min(XretirefirstAlign)-4,6,"Random individual i",xpd=TRUE,srt=90)
legend(-10,-2,fill = cols, legend = states[-4],horiz = TRUE,xpd=TRUE,bty="n")
dev.off()

pdf("DGD/Figures/Seq10align_inactlongright.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(-30,50), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
	draw_sequence4(state_seq = X[,i],x=Xinactlongright[,i],states,cols,y=yvals[i], border = NA)
}
axis(1)
text(min(XretirefirstAlign),yvals+.5,1:10,pos=2,xpd=TRUE)
text(min(XretirefirstAlign)-4,6,"Random individual i",xpd=TRUE,srt=90)
legend(-10,-2,fill = cols, legend = states[-4],horiz = TRUE,xpd=TRUE,bty="n")
dev.off()


# -------------------------------------
Xcenter   <- apply(X,2,align,state = c("Employed"),type="center",spell = "longest")
pdf("DGD/Figures/Seq10align_centerlongempl.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(-30,50), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
	draw_sequence4(state_seq = X[,i],x=Xcenter[,i],states,cols,y=yvals[i], border = NA)
}
axis(1)
text(min(XretirefirstAlign),yvals+.5,1:10,pos=2,xpd=TRUE)
text(min(XretirefirstAlign)-4,6,"Random individual i",xpd=TRUE,srt=90)
legend(-10,-2,fill = cols, legend = states[-4],horiz = TRUE,xpd=TRUE,bty="n")
dev.off()