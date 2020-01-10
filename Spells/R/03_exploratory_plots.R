library(here); library(devtools)
library(TraMineR); library(tidyverse)
library(reshape2); library(colorspace)
library(xtable)

load_all(here("Spells","R","Spells"))
# load(here("Spells","Data","Castro","cas_wom_seqs.RData"))

db_tidy <- readRDS(here("Spells","Data","Castro","cas_wom_tidy.rds"))

first_sex <- function(x){
  x[!is.na(x)][1]
}


db_to_summarize_and_plot <- db_tidy %>% 
  mutate(parity = as.character(parity),
         bparity = as.character(bparity),
         gparity = as.character(gparity)) %>% 
  group_by(ident) %>% 
  # need to filter on ever-union, if we use it to align.
  mutate(dfb = min(dob, na.rm=T),
         wdb = min(v011, na.rm=T),
         afb = floor((dfb- wdb)/12), afb5 = afb - afb %% 5,
         sexf = first_sex(sex),
         ev_union = any(evmar == "M", na.rm = TRUE),
         ceb = max(as.numeric(parity)),
         ceb = ifelse(ceb > 3, 4, ceb),
         
         # Alingments
         left_union = align(x = ev_union,
                            state = "NM", 
                            type ="left",
                            spell = "first"),
         
         left_par1 = align(x = parity,
                           state = "1", 
                           type ="left",
                           spell = "first"),
         
         # Note: if first birth is a girl, la_fboy just starts at 0
         la_fboy = align(x = bparity, state = "1", type ="left", spell = "first"),
         la_fgir = align(x = gparity, state = "1", type ="left", spell = "first"),
         
         # Clocks
         c_step_down_par1 = clock(x = parity, state = "1",  clock_type  = "step", 
           increasing  = FALSE),
         
         c_step_down_boy1 = clock(x = bparity, state = "1", clock_type  = "step", 
           increasing  = FALSE),
         
         c_step_down_gir1 = clock(x = gparity, state = "1", clock_type  = "step", 
           increasing  = FALSE)
  ) %>% 
  ungroup() %>% 
  filter(ceb >= 2 & afb<40, !is.na(mage))

db<-db_to_summarize_and_plot
cc<-db$ceb>=2 & db$afb<40 & db$mage == db$maget & db$maget>=39
ct<-db$ceb>=2 & db$afb<40 & db$mage == db$maget

xtable(addmargins(table(db$afb5[ct], db$ceb[ct])), digits=0)
xtable(addmargins(table(db$afb5[cc], db$ceb[cc])), digits=0)


glimpse(db_to_summarize_and_plot)  
table(db_to_summarize_and_plot$ceb, db_to_summarize_and_plot$afb)

saveRDS(db_to_summarize_and_plot, here("Spells","Data","Castro","cas_wom_measures.rds"))
setwd(here("Spells","Figures"))

# left first BIRTH time left to second birth, stratified by
# sex of the first birth AND age at first birth
db_to_summarize_and_plot %>% 
  # aligned on ev union, so need to filter on it.
  group_by(sexf, afb5, left_par1) %>% 
  summarize(med_time_left = median(c_step_down_par1, na.rm=TRUE),
            mean_time_left = mean(c_step_down_par1, na.rm=TRUE))  %>% 
  ggplot(mapping = aes(x = left_par1, 
                       y = mean_time_left, 
                       color = as.factor(sexf))) + 
  geom_line() + 
  #geom_vline(xintercept = c(0,1)) +
  # transparent band presumed to isolate twins
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 5, alpha = .2) +
  facet_wrap(~afb5) + 
  xlim(0,10)+ ylim(0,5) + 
  scale_color_manual(labels = c("Boy", "Girl"), values = c("blue", "red")) +
  guides(color=guide_legend(title="First child")) + 
  xlab("Time since first birth") + 
  ylab("Mean time to second birth")
dev.print(device=pdf, 'mt_second_birth_by_sex_first.pdf', width=7, height=5)



# TR: this one is freaking cool!
# left first BIRTH time left to second BOY, stratified by
# sex of the first birth AND age at first birth
db_to_summarize_and_plot %>% 
  # aligned on ev union, so need to filter on it.
  group_by(sexf, afb5, left_par1) %>% 
  summarize(med_time_left = median(c_step_down_boy1, na.rm=TRUE),
            mean_time_left = mean(c_step_down_boy1, na.rm=TRUE))  %>% 
  ggplot(mapping = aes(x = left_par1, 
                       y = mean_time_left, 
                       color = as.factor(sexf))) + 
  geom_line() + 
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 9, alpha = .2) +
  facet_wrap(~afb5) + 
  xlim(0,10) + ylim(0,9) +
  scale_color_manual(labels = c("Boy", "Girl"), values = c("blue", "red")) +
  guides(color=guide_legend(title="First child")) + 
  xlab("Time since first birth") + 
  ylab("Mean time to next boy")
dev.print(device=pdf, 'mt_second_boy_by_sex_first.pdf', width=7, height=5)



# TR: OMG they balance out!
# left first BIRTH time left to second GIRL, stratified by
# sex of the first birth AND age at first birth
db_to_summarize_and_plot %>% 
  # aligned on ev union, so need to filter on it.
  group_by(sexf, afb5, left_par1) %>% 
  summarize(med_time_left = median(c_step_down_gir1, na.rm=TRUE),
            mean_time_left = mean(c_step_down_gir1, na.rm=TRUE))  %>% 
  ggplot(mapping = aes(x = left_par1, 
                       y = mean_time_left, 
                       color = as.factor(sexf))) + 
  geom_line() + 
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 9, alpha = .2) +
  facet_wrap(~afb5) + 
  xlim(0,10) + ylim(0,9) +
  scale_color_manual(labels = c("Boy", "Girl"), values = c("blue", "red")) +
  guides(color=guide_legend(title="First child")) + 
  xlab("Time since first birth") + 
  ylab("Mean time to next girl")
dev.print(device=pdf, 'mt_second_gir_by_sex_first.pdf', width=7, height=5)





## first two pipes for plotting

# left union time left to second birth, stratified by
# ceb AND age at first birth
db_to_summarize_and_plot %>% 
  # aligned on ev union, so need to filter on it.
  filter(ev_union) %>% 
  group_by(ceb, left_union, afb5) %>% 
  summarize(med_time_left = median(c_step_down_par1, na.rm=TRUE),
            mean_time_left = mean(c_step_down_par1, na.rm=TRUE))  %>% 
  ggplot(mapping = aes(x = left_union, 
                       y = mean_time_left, 
                       color = as.factor(ceb))) + 
  geom_line() + 
  # annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 5,
  #          alpha = .2) +
  facet_wrap(~afb5, scales="free_x") + 
  guides(color=guide_legend(title="CEB")) + 
  scale_color_discrete_sequential(palette = "Peach", rev = TRUE, nmax = 3) 


# left align on first birth, mean time left to second birth, stratified by
# ceb AND age at first birth. CED already filtered
db_to_summarize_and_plot %>% 
  group_by(ceb, left_par1, afb5) %>% 
  summarize(med_time_left = median(c_step_down_par1, na.rm=TRUE),
            mean_time_left = mean(c_step_down_par1, na.rm=TRUE))  %>% 
  ggplot(mapping = aes(x = left_par1, 
                       y = mean_time_left, 
                       color = as.factor(ceb))) + 
  geom_line() + 
  # annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 5,
  #          alpha = .2) +
  facet_wrap(~afb5) + 
  guides(color=guide_legend(title="CEB")) +
  xlim(0,20)+
  scale_color_discrete_sequential(palette = "Peach",rev = TRUE, nmax = 3)





### old stuff
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

# orders to long
m_orders           <- as.matrix(m_orders)
dimnames(m_orders) <- list(1:nrow(m_births),10:50)
o_tidy             <- reshape2::melt(m_orders, 
                                     varnames = c("id","age"), 
                                     value.name = "order")

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
  left_join(u_tidy, by = c("id","age")) %>% 
  left_join(o_tidy, by = c("id","age")) 


db_to_summarize_and_plot <- db_tidy %>% 
  mutate(birth = as.character(birth),
         parity = as.character(parity),
         union = as.character(union)) %>% 
  group_by(id) %>% 
  # need to filter on ever-union, if we use it to align.
  mutate(ev_union = any(union == "Married/In union", na.rm = TRUE),
         ceb = max(as.numeric(parity)),
         ceb = ifelse(ceb > 3, 4, ceb),
         left_union = align(x = union,
                            state = "Never m/u", 
                            type ="left",
                            spell = "first"),
         left_par1 = align(x = parity,
                            state = "1", 
                            type ="left",
                            spell = "first"),
         c_step_down_par1 = clock(
                            x = parity,
                            state = "1", 
                            clock_type  = "step", 
                            increasing  = FALSE)
         ) %>% 
  ungroup() %>% 
  filter(ceb >= 2,
         !is.na(age)) %>% 
  group_by(id) %>% 
  mutate(afb = min(as.numeric(age[birth == "1"])),
         afb5 = afb - afb %% 5) 


# left union time left to second birth, stratified by
# ceb AND age at first birth
db_to_summarize_and_plot %>% 
  # aligned on ev union, so need to filter on it.
  filter(ev_union) %>% 
  group_by(ceb, left_union, afb5) %>% 
  summarize(med_time_left = median(c_step_down_par1, na.rm=TRUE),
            mean_time_left = mean(c_step_down_par1, na.rm=TRUE))  %>% 
  ggplot(mapping = aes(x = left_union, 
                       y = mean_time_left, 
                       color = as.factor(ceb))) + 
  geom_line() + 
  geom_vline(xintercept = 1) +
  facet_wrap(~afb5) + 
  guides(color=guide_legend(title="CEB"))

# left align on first birth, mean time left to second birth, stratified by
# ceb AND age at first birth. CED already filtered
db_to_summarize_and_plot %>% 
  group_by(ceb, left_par1, afb5) %>% 
  summarize(med_time_left = median(c_step_down_par1, na.rm=TRUE),
            mean_time_left = mean(c_step_down_par1, na.rm=TRUE))  %>% 
  ggplot(mapping = aes(x = left_par1, 
                       y = mean_time_left, 
                       color = as.factor(ceb))) + 
  geom_line() + 
  geom_vline(xintercept = 0) +
  facet_wrap(~afb5) + 
  guides(color=guide_legend(title="CEB")) +
  xlim(0,20)