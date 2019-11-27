
setwd("U:/Cloud/Spells/Spells/Data/Castro")
load('cas_wom_seqs.RData')

library(TraMineR)

df$tbirths<-apply(m_births, 1, sum, na.rm=T)
table(df$tbirths)

pmax<-3
cond<-df$tbirths>=pmax

ds<-m_parity
ds[m_parity>=pmax & !is.na(m_parity)]<-NA

apply(m_parity, 2, mean, na.rm=T)
apply(ds, 2, mean, na.rm=T)


summary((df$v008[cond]-df$v011[cond])/12)
seq<-seqdef(data=ds[cond,], vars=colnames(m_orders), cnames=paste0('a',10:50))
entro<-seqient(seqdata = seq)

summary(seq)

png('top_mid_bottom_entropy.png', height=3200, width=4200, res=320)
par(mar=c(4,4,1,1), mfrow=c(1,4))
seqiplot(seq, i=rev(order(entro))[seq(1,5000,50)], use.layout=F, main='top')
seqiplot(seq, i=(order(entro))[(nrow(seq)/2-50):(nrow(seq)/2+49)], use.layout=F, main='mid')
seqiplot(seq, i=(order(entro))[seq(1,5000,50)], use.layout=F, main='bottom')
seqIplot(seq, sortv=entro, use.layout=F, main='all')
abline(h=c(5000, 42004), lwd=2)
dev.off()

