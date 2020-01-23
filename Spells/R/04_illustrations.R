library(here); library(devtools); library(TraMineR); library(tidyverse); library(Spells)
library(RColorBrewer); library(car)
remove(list=ls())


db<-data.frame(readRDS(here("Spells","Data","Castro","cas_wom_cohort.rds")))
db<-db[db$ceb==4,]

db$sexpar<-paste(car::recode(db$sex, "1='B'; 2='G'; NA='NB'"), 
                 recode(pmax(as.numeric(db$bparity), as.numeric(db$gparity)),
                        "3:19='3+'"), sep='-')
db$sexpar<-factor(recode(db$sexpar, "'NB-0'='NB'; 'NB-1'='NB';
                  'NB-2'='NB'; 'NB-3+'='NB'"))

table(db$sexpar)

set.seed(22012020)
n<-floor(runif(10, 1, length(unique(db$ident))))

col7<-c(brewer.pal(9, "PuBu")[4:6], brewer.pal(9, "PuRd")[4:6], 'gray90')
names(col7)<-levels(db$sexpar)

# case 8
mpar<-NULL; seqs<-NULL; bpar<-NULL; gpar<-NULL
for(i in 1:length(n)){
  cons<-db$mage>=15 & db$mage<=39 & db$ident==unique(db$ident)[n[i]]
  
  cona<-db$left_par1>=0 & db$left_par1<=15 & db$ident==unique(db$ident)[n[i]]
  
  seqs<-cbind(seqs, as.character(db$sexpar[cons]))
  
  mpar<-cbind(mpar, as.character(db$parity[cons]))
  
  bpar<-cbind(bpar, as.character(db$bparity[cons]))
  
  gpar<-cbind(gpar, as.character(db$gparity[cons]))
  
}

par(mfrow=c(2,2), mar=c(3,3,1,1))
plot(NULL, type="n", xlim=c(15,40),  ylim=c(0,12), axes = FALSE, xlab="", ylab="",
     main='Original data')
for (i in 1:10){
  draw_sequence(seqs[,i], 
                x=15:39, 
                y=i, 
                labels=seqs[,i],
                cols=col7,
                border = NA,
                box = TRUE, cex=0.55)
}

plot(NULL, type="n", xlim=c(1,25),  ylim=c(0,12), axes = FALSE, xlab="", ylab="",
     main="Clock 1: time to next birth")
for (i in 1:10){
   draw_sequence(seqs[,i], x=align(mpar[,i], state="1", type='left'), y=i, cols=col7,
                 labels=clock(x=mpar[,i], state="1",clock_type="step",increasing=FALSE),
                 border=NA, box = TRUE, cex=0.65)
}

plot(NULL, type="n", xlim=c(1,25),  ylim=c(0,12), axes = FALSE, xlab="", ylab="",
     main='Clock 2: time to next boy')
for (i in 1:10){
  draw_sequence(seqs[,i], x=align(mpar[,i], state="1", type='left'), y=i, cols=col7,
                labels=clock(x=bpar[,i], state="1",clock_type="step",increasing=FALSE),
                border=NA, box = TRUE, cex=0.65)
}

plot(NULL, type="n", xlim=c(1,25),  ylim=c(0,12), axes = FALSE, xlab="", ylab="",
     main="Clock 3: time to next girl")
for (i in 1:10){
  draw_sequence(seqs[,i], x=align(mpar[,i], state="1", type='left'), y=i, cols=col7,
                labels=clock(x=gpar[,i], state="1",clock_type="step",increasing=FALSE),
                border=NA, box = TRUE, cex=0.65)
}





