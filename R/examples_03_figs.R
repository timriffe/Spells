
# Author: tim
###############################################################################

source(here::here("R","00_install_packages.R"))

Rtraj_clean <- readRDS(here::here("Data","Dudel","Rtraj_clean.rds"))


# antique colors
colsIN <- c("#855C75","#D9AF6B","#AF6458","#736F4C","#526A83","#625377","#68855C","#9C9C5E","#A06177","#8C785D","#467378","#7C7C7C")

# colsIN <-c("#88CCEE","#CC6677","#DDCC77","#117733","#332288","#AA4499","#44AA99","#999933","#882255","#661100","#6699CC","#888888")
length(colsIN)
plot(NULL,xlim=c(0,12),ylim=c(0,1),asp=1)
for (i in 1:12){
  rect(i-1,0,i,1,col=colsIN[i])
}
text(1:12-.5,.5,1:12)

# cols <- c(colsIN[c(4,3,9)],NA)
cols <- c(colsIN[c(7,2,1)],NA)
# cols[1]<- "#6c945c"
# cols[3]<- "#826ca1"
# cols <- c("#003f5c","#ffa600","#bc5090",NA)
#cols   <- c("#74ee65", "#773129", "#41bbc5",NA)
# cols <- lighten(desaturate(c(qualitative_hcl(5, palette = "Dark 3")[c(3,2,4)],NA),.1),.05)
cols2 <- lighten(desaturate(cols,.01),.01)

states <- c("Employed", "Inactive","Retired","Dead")
yvals  <- 9:0 * 1.1

X <- RTraj_clean[,1:10]


# hcl_palettes(plot=T)

par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,101), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
  draw_sequence2(X[,i],states,cols,y=yvals[i], border = NA)
}
axis(1)
text(50,yvals+.5,1:10,pos=2,xpd=TRUE)
text(47,6,"Random individual i",xpd=TRUE,srt=90)
legend(60,-2,fill = cols2, legend = states[-4],horiz = TRUE,xpd=TRUE,bty="n")

# saveRDS(RTraj_clean, here("LabTalk","RTraj_clean.rds"))
# saveRDS(X, here("LabTalk","X.rds"))

pdf(here("Figures","Seq10.pdf"),height=4,width=9)
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


Ones            <- X == "Employed"
Ones[Ones]      <- 1
Ones[Ones == 0] <- NA

pdf(here("Figures","Seq10ones.pdf"),height=4,width=9)
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

pdf(here("Figures","Seq10dur.pdf"),height=4,width=9)
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
  draw_sequence2(X[,i],states,cols2,y=yvals[i], border = NA)
  draw_sequence3(DurEntry[,i],y=yvals[i])
  rect(50,yvals[i],(which(X[,i] == "Dead")[1]+49),yvals[i]+1,border = gray(.4),lwd=.5)
}
axis(1)
text(50,yvals+.5,1:10,pos=2,xpd=TRUE)
text(47,6,"Random individual i",xpd=TRUE,srt=90)



OrdUp   <- apply(X,2,clock,clock_type="order",state = "Employed",increasing=TRUE, step_size = 1)
OrdDown <- apply(X,2,clock,clock_type="order",state = "Employed",increasing=FALSE, step_size = 1)


pdf(here("Figures","Seq10ordUp.pdf"),height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,101), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
  draw_sequence2(X[,i],states,cols2,y=yvals[i], border = NA)
	draw_sequence3(OrdUp[,i],y=yvals[i])
	rect(50,yvals[i],(which(X[,i] == "Dead")[1]+49),yvals[i]+1,border = gray(.4),lwd=.5)
}
axis(1)
text(50,yvals+.5,1:10,pos=2,xpd=TRUE)
text(47,6,"Random individual i",xpd=TRUE,srt=90)
dev.off()


pdf(here("Figures","Seq10ordDown.pdf"),height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,101), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
  draw_sequence2(X[,i],states,cols2,y=yvals[i], border = NA)
	draw_sequence3(OrdDown[,i],y=yvals[i])
	rect(50,yvals[i],(which(X[,i] == "Dead")[1]+49),yvals[i]+1,border = gray(.4),lwd=.5)
}
axis(1)
text(50,yvals+.5,1:10,pos=2,xpd=TRUE)
text(47,6,"Random individual i",xpd=TRUE,srt=90)
dev.off()




# time spent

# args(clock)
TimeSpent <- apply(X,2,clock,state = "Inactive",clock_type="step")

pdf(here("Figures","Seq10timespent.pdf"),height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,101), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
  draw_sequence2(X[,i],states,cols2,y=yvals[i], border = NA)
	draw_sequence3(floor(TimeSpent)[,i],y=yvals[i])
	rect(50,yvals[i],(which(X[,i] == "Dead")[1]+49),yvals[i]+1,border = gray(.4),lwd=.5)
}
axis(1)
text(50,yvals+.5,1:10,pos=2,xpd=TRUE)
text(47,6,"Random individual i",xpd=TRUE,srt=90)
dev.off()

# 
# TimeLeft <- apply(X,2,spell_dur_after,state = "Inactive")
TimeLeft <- apply(X,2,clock,state = "Inactive",clock_type="step",increasing = FALSE)
pdf(here("Figures","Seq10timeleft.pdf"),height=4,width=9)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,101), ylim = c(0,12), axes = FALSE, xlab = "", ylab = "")
for (i in 1:10){
  draw_sequence2(X[,i],states,cols2,y=yvals[i], border = NA)
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
pdf(here("Figures","Seq10deathalign.pdf"),height=4,width=9)
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

pdf(here("Figures","Seq10inactlongright.pdf"),height=4,width=9)
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
pdf(here("Figures","Seq10centerlongempl.pdf"),height=4,width=9)
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

# prevalence curve
Ones     <- XX == "Employed"
Ones[XX == "Dead"] <- NA
PrevEmpl <- rowMeans(Ones, na.rm=TRUE)
par(mai=c(.8,1,0,0))
plot(NULL, type = "n", xlim = c(50,101), ylim = c(0,1), axes = FALSE, xlab = "", ylab = "")
lines(50:100, PrevEmpl, lwd = 2)
axis(1)
text(43,.5,"Prevalence",xpd=TRUE,srt=90)
axis(2, las = 1)

Dat %>% 
  filter(age < 80) %>% 
  group_by(InQ, id) %>% 
  mutate(dead = ifelse(any(state == "Dead"),TRUE,FALSE)) %>% 
  ungroup() %>% 
  filter(dead) %>% 
  filter(InQ == "I",
         age >= 50 & age <= 80) %>% 
  mutate(state = case_when(state == "Healthy" ~ "H",
                           state == "Disabled" ~ "D",
                           TRUE ~ "Dead"))