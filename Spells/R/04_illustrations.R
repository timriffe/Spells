library(here); library(devtools); library(TraMineR); library(tidyverse); library(Spells)
library(RColorBrewer); library(car); library(reshape2); library(toOrdinal)
remove(list=ls())


db<-data.frame(readRDS(here::here("Spells","Data","Castro","cas_wom_colombia.rds")))
#db<-data.frame(readRDS(here::here("Spells","Data","Castro","cas_wom_senegal.rds")))
db<-db[db$ceb==4 & db$maget>=39,]


db$sexpar<-factor(paste(car::recode(db$sex, "1='B'; 2='G'; NA=' '"), 
                 recode(as.numeric(db$parity), "3:19='3+'"), sep=''))
db$sexpar<-factor(recode(db$sexpar, "'NB0'='NB'; 'NB1'='NB';
                  'NB2'='NB'; 'NB3+'='NB'"))
table(db$gparity)
table(db$sexpar)

setwd(here("Spells","Figures"))
set.seed(423)
# n<-floor(runif(10, 1, length(unique(db$ident))))
sub <- db %>% 
  filter(
    ident %in% sample(unique(db$ident),10,replace=FALSE),
    mage >= 10,
    mage <= 39
  ) %>% 
  replace_na(list(sex = " "))


mpar <- sub %>% acast(mage ~ ident, value.var = "parity")
bpar <- sub %>% acast(mage ~ ident, value.var = "bparity")
gpar <- sub %>% acast(mage ~ ident, value.var = "gparity")

seqs <- sub %>% acast(mage ~ ident, value.var = "sex")

cols <- c(brewer.pal(9, "PuBu")[4],
          brewer.pal(9, "PuRd")[4],
          gray(.9))
names(cols) <- c(1,2," ")

cex8<-0.7

par(mfrow=c(2,2), mar=c(3.5,3.5,1,1))
plot(NULL, type="n", xlim=c(10,40),  ylim=c(0,12), axes = FALSE, xlab="", ylab="",
     main='')
for (i in 1:10){
  draw_sequence(as.character(seqs[,i]), x=10:39, y=i, cols=cols, border = 'white',
                box = TRUE, cex=cex8)
}
axis(side=1, line=0, at=seq(10,40,2))
mtext(side=1, line=2, text="Age")
text(7,6,"Random individual i",xpd=TRUE,srt=90)
text(10,1:10+0.5, labels=10:1, pos=2,xpd=TRUE)
legend(10, 1, fill = cols, legend=c('Boy','Girl','No birth'), horiz = TRUE,xpd=TRUE,bty="n")

plot(NULL, type="n", xlim=c(0,25),  ylim=c(0,12), axes = FALSE, xlab="", ylab="",
     main="")
for (i in 1:10){
   x <- Spells::align(mpar[,i], state="1", type='left')
   draw_sequence(as.character(seqs[,i]), x=x, y=i, cols=cols,
                 labels=floor(clock(x=mpar[,i], state="1",clock_type="step",increasing=FALSE)),
                 border=NA, box = TRUE, cex=cex8)
   xr = sum(mpar[,i] == "1", na.rm = TRUE)
   rect(0,i,xr,i+1, border = "gray40", lwd=1)
}
axis(side=1, line=0, at=seq(0,24,2))
mtext(side=1, line=2, text="Time since first birth")
text(-3,6,"Random individual i",xpd=TRUE,srt=90)
text(-1, 1:10+0.5, labels=10:1, pos=2,xpd=TRUE)
legend(0, 1, fill = cols, legend=c('Boy','Girl','No birth'), horiz = TRUE,xpd=TRUE,bty="n")


plot(NULL, type="n", xlim=c(0,25),  ylim=c(0,12), axes = FALSE, xlab="", ylab="",
     main='')
for (i in 1:10){
  draw_sequence(seqs[,i], x=Spells::align(mpar[,i], state="1", type='left'), y=i, cols=cols,
                labels=floor(clock(x=bpar[,i], state="X",clock_type="step",increasing=FALSE)),
                border=NA, box = TRUE, cex=cex8)
  xr = sum(bpar[,i] == "X", na.rm = TRUE)
  rect(0,i,xr,i+1, border = "gray40", lwd=1)
}
axis(side=1, line=0, at=seq(0,24,2))
mtext(side=1, line=2, text="Time since first birth")
text(-3,6,"Random individual i",xpd=TRUE,srt=90)
text(-1, 1:10+0.5, labels=10:1, pos=2,xpd=TRUE)
legend(0, 1, fill = cols, legend=c('Boy','Girl','No birth'), horiz = TRUE,xpd=TRUE,bty="n")


plot(NULL, type="n", xlim=c(0,25),  ylim=c(0,12), axes = FALSE, xlab="", ylab="",
     main="")
for (i in 1:10){
  draw_sequence(seqs[,i], x=Spells::align(mpar[,i], state="1", type='left'), y=i, cols=cols,
                labels=floor(clock(x=gpar[,i], state="X",clock_type="step",increasing=FALSE)),
                border=NA, box = TRUE, cex=cex8)
  xr = sum(gpar[,i] == "X", na.rm = TRUE)
  rect(0,i,xr,i+1, border = "gray40", lwd=1)
}
axis(side=1, line=0, at=seq(0,24,2))
mtext(side=1, line=2, text="Time since first birth")
text(-3,6,"Random individual i",xpd=TRUE,srt=90)
text(-1, 1:10+0.5, labels=10:1, pos=2,xpd=TRUE)
legend(0, 1, fill = cols, legend=c('Boy','Girl','No birth'), horiz = TRUE,xpd=TRUE,bty="n")

dev.print(device=pdf, 'illu_fertility.pdf', width=14, height=8)
