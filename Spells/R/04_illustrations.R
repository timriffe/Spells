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
  c1<-db[cona, c('mage', 'left_par1', 'sex', grep('c_', colnames(db), value=T)[1])]
  
}
seqs[seqs==1]<-'Boy'
seqs[seqs==2]<-'Girl'
seqs[is.na(seqs)]<-'nobirth'

# key: get a named vector of colors, which includes at least all states in your space.
cols <- c(Girl = "red", Boy = "blue", nobirth = gray(.95))

plot(NULL, type="n", xlim=c(15,40),  ylim=c(0,12), axes = FALSE, xlab="", ylab="")
for (i in 1:10){
  draw_sequence(seqs[,i], 
                x=15:39, 
                y=i, 
                cols=cols,
                border = NA,
                box = TRUE)
}
axis(1)


# REally to do the clock / alsign stuff you want at least one more matrix. One for parity sequences. Then do align() to x and clock straight to the labels argument inside draw_sequence()
#See ?draw_sequence for an example of that

# #
# plot(NULL, type="n", xlim=c(15,40),  ylim=c(0,12), axes = FALSE, xlab="", ylab="")
# for (i in 1:10){
#   draw_sequence(seqs[,i], 
#                 x=align(seqs[,i]), 
#                 y=i, 
#                 cols=cols,
#                 labels = clock()
#                 border = NA,
#                 box = TRUE)
# }
# 
# 




axis(1)