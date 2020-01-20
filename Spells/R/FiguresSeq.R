
# Author: tim
###############################################################################

library(here)
library(devtools)
library(colorspace)
install_github("timriffe/Spells/Spells/R/Spells")

source(here("Spells","R","GenerateStationary.R"))

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


#cols   <- c("#74ee65", "#773129", "#41bbc5",NA)
cols <- c(qualitative_hcl(5, palette = "Dark 3")[c(3,2,4)],NA)
cols2 <- lighten(desaturate(cols,.3),.3)
states <- c("Employed", "Inactive","Retired","Dead")
yvals  <- 9:0 * 1.1



X <- RTraj_clean[,1:10]

saveRDS(RTraj_clean, here("Spells","LabTalk","RTraj_clean.rds"))
saveRDS(X, here("Spells","LabTalk","X.rds"))

pdf("Figures/Seq10.pdf",height=4,width=9)
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

par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,101), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
  draw_sequence2(X[,i],states,cols2,y=yvals[i], border = NA)
}
axis(1)
text(50,yvals+.5,1:10,pos=2,xpd=TRUE)
text(47,6,"Random individual i",xpd=TRUE,srt=90)
legend(60,-2,fill = cols2, legend = states[-4],horiz = TRUE,xpd=TRUE,bty="n")

# ---------------------------------------
# figure 2
# total duration.


Ones            <- X == "Employed"
Ones[Ones]      <- 1
Ones[Ones == 0] <- NA

pdf("Figures/Seq10ones.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,101), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
  draw_sequence2(X[,i],states,cols2,y=yvals[i], border = NA)
	draw_sequence3(Ones[,i],y=yvals[i])
	rect(50,yvals[i],(which(X[,i] == "Dead")[1]+49),yvals[i]+1,border = gray(.4),lwd=.5)
}
axis(1)
text(50,yvals+.5,1:10,pos=2,xpd=TRUE)
text(47,6,"Random individual i",xpd=TRUE,srt=90)
dev.off()




Dur <- apply(X,2,clock,clock_type="duration",state = "Inactive")

pdf("Figures/Seq10dur.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,101), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
  draw_sequence2(X[,i],states,cols2,y=yvals[i], border = NA)
	draw_sequence3(Dur[,i],y=yvals[i])
	rect(50,yvals[i],(which(X[,i] == "Dead")[1]+49),yvals[i]+1,border = gray(.4),lwd=.5)
}
axis(1)
text(50,yvals+.5,1:10,pos=2,xpd=TRUE)
text(47,6,"Random individual i",xpd=TRUE,srt=90)
dev.off()

# conditional on age at entry:
# (function not yet wrapped in clock())
DurEntry      <- apply(X,2,spell_dur_conditional,state = "Inactive", entry = TRUE)
DurExit       <- apply(X,2,spell_dur_conditional,state = "Inactive", entry = FALSE)

par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,101), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
  draw_sequence3(DurEntry[,i],y=yvals[i])
  rect(50,yvals[i],(which(X[,i] == "Dead")[1]+49),yvals[i]+1,border = gray(.4),lwd=.5)
}
axis(1)
text(50,yvals+.5,1:10,pos=2,xpd=TRUE)
text(47,6,"Random individual i",xpd=TRUE,srt=90)



OrdUp   <- apply(X,2,clock,clock_type="order",state = "Employed",increasing=TRUE, step_size = 1)
OrdDown <- apply(X,2,clock,clock_type="order",state = "Employed",increasing=FALSE, step_size = 1)


pdf("Figures/Seq10ordUp.pdf",height=4,width=9)
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


pdf("Figures/Seq10ordDown.pdf",height=4,width=9)
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

pdf("Figures/Seq10timespent.pdf",height=4,width=9)
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


TimeLeft <- apply(X,2,spell_dur_after,state = "Inactive")

pdf("Figures/Seq10timeleft.pdf",height=4,width=9)
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



# ---------------------------------
# Alignment
XdeathAlign <- apply(X,2,align,state = "Dead",type="left")
pdf("Figures/Seq10deathalign.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(-80,0), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
	draw_sequence4(state_seq = X[,i],x=XdeathAlign[,i],states,cols,y=yvals[i], border = NA)
}
axis(1)
text(-80,yvals+.5,1:10,pos=2,xpd=TRUE)
text(-83,6,"Random individual i",xpd=TRUE,srt=90)
legend(-40,-2,fill = cols, legend = states[-4],horiz = TRUE,xpd=TRUE,bty="n")
dev.off()


# align on first retirement:
XdeathAlign <- apply(X,2,align,state = "Dead",type="left")
XretirefirstAlign <- apply(X,2,align,state = "Retired",type="left",spell = "first")
Xinactlongleft    <- apply(X,2,align,state = "Inactive",type="left",spell = "longest")
Xinactlongright   <- apply(X,2,align,state = "Inactive",type="right",spell = "longest")

pdf("Figures/Seq10firstretirealign.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(-30,50), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
	draw_sequence4(state_seq = X[,i],x=XretirefirstAlign[,i],states,cols,y=yvals[i], border = NA)
}
axis(1)
text(min(XretirefirstAlign),yvals+.5,1:10,pos=2,xpd=TRUE)
text(min(XretirefirstAlign)-3,6,"Random individual i",xpd=TRUE,srt=90)
#legend(-10,-2,fill = cols, legend = states[-4],horiz = TRUE,xpd=TRUE,bty="n")
dev.off()

pdf("Figures/Seq10inactlongleft.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(-30,50), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
	draw_sequence4(state_seq = X[,i],x=Xinactlongleft[,i],states,cols,y=yvals[i], border = NA)
}
axis(1)
text(min(XretirefirstAlign),yvals+.5,1:10,pos=2,xpd=TRUE)
text(min(XretirefirstAlign)-3,6,"Random individual i",xpd=TRUE,srt=90)
#legend(-10,-2,fill = cols, legend = states[-4],horiz = TRUE,xpd=TRUE,bty="n")
dev.off()

pdf("Figures/Seq10inactlongright.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(-30,50), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
	draw_sequence4(state_seq = X[,i],x=Xinactlongright[,i],states,cols,y=yvals[i], border = NA)
}
axis(1)
text(min(XretirefirstAlign),yvals+.5,1:10,pos=2,xpd=TRUE)
text(min(XretirefirstAlign)-3,6,"Random individual i",xpd=TRUE,srt=90)
legend(-10,-2,fill = cols, legend = states[-4],horiz = TRUE,xpd=TRUE,bty="n")
dev.off()


# -------------------------------------
Xcenter   <- apply(X,2,align,state = c("Employed"),type="center",spell = "longest")
pdf("Figures/Seq10centerlongempl.pdf",height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(-30,50), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
	draw_sequence4(state_seq = X[,i],x=Xcenter[,i],states,cols,y=yvals[i], border = NA)
}
axis(1)
text(min(XretirefirstAlign),yvals+.5,1:10,pos=2,xpd=TRUE)
text(min(XretirefirstAlign)-3,6,"Random individual i",xpd=TRUE,srt=90)
legend(-10,-2,fill = cols, legend = states[-4],horiz = TRUE,xpd=TRUE,bty="n")
dev.off()


library(here)
library(tidyverse)
source(here::here("Spells","R","DrawSequences.R"))
Dat <- readRDS(here::here("Spells","Data","Lorenti","SILCsim.rds"))
Dat$state <- as.character(Dat$state)
X <- Dat %>% 
  filter(age < 80) %>% 
  group_by(InQ, id) %>% 
  mutate(dead = ifelse(any(state == "Dead"),TRUE,FALSE)) %>% 
  ungroup() %>% 
  filter(dead) %>% 
  filter(InQ == "I",
         age >= 50 & age <= 80) %>% 
  mutate(state = case_when(state == "Healthy" ~ "H",
                           state == "Disabled" ~ "D",
                           TRUE ~ "Dead")) %>%
  acast(id~age, value.var = "state") 
X <- X[1:10,]
DurEntry      <- apply(X, 1, spell_dur_conditional,state = "D", entry = TRUE)

DurEntry <- t(DurEntry)

# this appears in draft manuscript
pdf(here("Spells","Figures","DisTrajExample.pdf"),width=7,height=4.5)
colsHD <- c("#add996","#bf9319",Dead="#FFFFFF00")
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,81), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
  draw_sequence2(X[i,],states = c("H","D","Dead"),colsHD,y=yvals[i], border = NA)
  draw_sequence3(DurEntry[i,],y=yvals[i])
  rect(50,yvals[i],(which(X[,i] == "Dead")[1]+49),yvals[i]+1,border = gray(.4),lwd=.5)
}
axis(1)
text(50,yvals+.5,1:10,pos=2,xpd=TRUE)
text(47,6,"Random individual i",xpd=TRUE,srt=90)
legend(-10,-2,fill = colsHD[1:2], legend = c("Healthy","Disabled"),horiz = TRUE,xpd=TRUE,bty="n")
dev.off()


