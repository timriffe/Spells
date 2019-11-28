
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

getwd()
library(devtools)
library(here)
load_all(here("Spells","R","Spells"))


x <- c(sample(c("A","I"),size=10,replace = TRUE, prob = c(.8,.2)),
       sample(c("A","I","R"),size=15, replace = TRUE, prob = c(.1,.2,.7)),
       rep("R",5),rep("D",5))

dsc <- as.character(as.matrix(ds))

dim(dsc) <- dim(ds)
unique(dsc)
dsa <- t(apply(dsc[cond, ], 1, align, state = "1", type = "left", spell = "first" ))

head(dsa)

step_up_1 <- t(apply(dsc[cond, ], 1, clock, state = "1", clock_type  = "step", increasing  = TRUE ))

head(step_up_1)

######################################
# build a tidy object

library(tidyverse)
library(reshape2)

# births to long
m_births           <- as.matrix(m_births)
dimnames(m_births) <- list(1:nrow(m_births),10:50)
b_tidy             <- reshape2::melt(m_births, 
                                     varnames = c("id","age"), 
                                     value.name = "birth")
 
# parity to long
m_parity           <- as.matrix(m_parity)
dimnames(m_parity) <- list(1:nrow(m_parity),10:50)
p_tidy             <- reshape2::melt(m_parity, 
                                     varnames = c("id","age"), 
                                     value.name = "parity")
# union to long
m_unions           <- as.matrix(m_unions)
dimnames(m_unions) <- list(1:nrow(m_unions),10:50)
u_tidy             <- reshape2::melt(m_unions, 
                                     varnames = c("id","age"), 
                                     value.name = "union")

# join operation takes a little time
db_tidy <- 
  left_join(b_tidy, p_tidy, by = c("id","age")) %>% 
  left_join(u_tidy, by = c("id","age"))

db_tidy %>% 
  mutate(birth = as.character(birth),
         parity = as.character(parity),
         union = as.character(union)) %>% 
  group_by(id) %>% 
  mutate(ceb = max(as.numeric(parity)),
         ceb = ifelse(ceb > 3, 4, ceb),
         left_union = align(x = union,
                            state = "Never m/u", 
                            type ="right",
                            spell = "first"),
         # left_par1 = aligns(x = parity,
         #                    state = "1", 
         #                    type ="left",
         #                    spell = "first"),
         c_step_down_par1 = clock(
                            x = parity,
                            state = "1", 
                            clock_type  = "step", 
                            increasing  = FALSE)
         ) %>% 
  ungroup() %>% 
  filter(ceb >= 2) %>% 
  group_by(ceb, left_union) %>% 
  summarize(med_time_left = median(c_step_down_par1, na.rm=TRUE)) %>% 
  ggplot(mapping = aes(x = left_union, y = med_time_left)) + 
  geom_line()
  

