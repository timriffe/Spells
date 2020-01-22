library(here); library(devtools); library(TraMineR); library(tidyverse); library(Spells)
remove(list=ls())


db<-data.frame(readRDS(here("Spells","Data","Castro","cas_wom_cohort.rds")))
db<-db[db$ceb==4,]

set.seed(22012020)
n<-floor(runif(10, 1, length(unique(db$ident))))

# case 8
c1<-NULL; seqs<-NULL
for(i in 1:length(n)){
  cons<-db$mage>=15 & db$mage<=39 & db$ident==unique(db$ident)[n[i]]
  cona<-db$left_par1>=0 & db$left_par1<=15 & db$ident==unique(db$ident)[n[i]]
  seqs<-cbind(seqs, db$sex[cons])
  c1<-db[cond, c('mage', 'left_par1', 'sex', grep('c_', colnames(db), value=T)[1])]
  
}
seqs[seqs==1]<-'Boy'
seqs[seqs==2]<-'Girl'
seqs[is.na(seqs)]<-'--'


plot(NULL, type="n", xlim=c(15,40),  ylim=c(0,12), axes = FALSE, xlab="", ylab="")
for (i in 1:10){
  draw_sequence(seqs[,i], x=15:39, y=i, cols=1:2, labels=seqs)
}

