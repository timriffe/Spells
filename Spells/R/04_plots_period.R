library(here); library(devtools); library(TraMineR); library(tidyverse)
remove(list=ls())

db<-readRDS(here("Spells","Data","Castro","cas_wom_period.rds"))
col7<-c(brewer.pal(9, "PuBu")[4:6], brewer.pal(9, "PuRd")[4:6], 'gray90')

cc<-db$ceb>=2 & db$afb<40 & db$mage == db$maget & db$maget>=39
ct<-db$ceb>=2 & db$afb<40 & db$mage == db$maget
prop.table(table(db$afb5))

xtable(addmargins(table(db$afb5[ct], db$ceb[ct])), digits=0)
xtable(addmargins(table(db$afb5[cc], db$ceb[cc])), digits=0)

glimpse(db)  
table(db$ceb, db$afb)

setwd(here("Spells","Figures"))

# left first BIRTH time left to second birth, stratified by
# sex of the first birth AND age at first birth
db %>% 
  # aligned on ev union, so need to filter on it.
  group_by(sexf, afb5, left_par1) %>% 
  summarize(med_time_left = median(c_step_down_par1, na.rm=TRUE),
            mean_time_left = weighted.mean(c_step_down_par1, w=pwt, na.rm=TRUE))  %>% 
            #mean_time_left = mean(c_step_down_par1, na.rm=TRUE))  %>% 
  ggplot(mapping = aes(x = left_par1, 
                       y = mean_time_left, 
                       color = as.factor(sexf))) + 
  geom_line() + 
  #geom_vline(xintercept = c(0,1)) +
  # transparent band presumed to isolate twins
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 5, alpha = .2) +
  facet_wrap(~afb5) + 
  xlim(0,10)+ ylim(0,5) + 
  scale_color_manual(labels = c("Boy", "Girl"), values = col7[c(2,5)]) +
  guides(color=guide_legend(title="First child")) + 
  xlab("Time since first birth") + 
  ylab("Mean time to second birth")
dev.print(device=pdf, 'mt_second_birth_by_sex_first.pdf', width=7, height=5)

# TR: this one is freaking cool!
# left first BIRTH time left to second BOY, stratified by
# sex of the first birth AND age at first birth
db %>% 
  # aligned on ev union, so need to filter on it.
  group_by(sexf, afb5, left_par1) %>% 
  summarize(med_time_left = median(c_step_down_boy1, na.rm=TRUE),
            mean_time_left = weighted.mean(c_step_down_boy1, w=pwt, na.rm=TRUE))  %>% 
  ggplot(mapping = aes(x = left_par1, 
                       y = mean_time_left, 
                       color = as.factor(sexf))) + 
  geom_line() + 
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 9, alpha = .2) +
  facet_wrap(~afb5) + 
  xlim(0,10) + ylim(0,9) +
  scale_color_manual(labels = c("Boy", "Girl"), values = col7[c(2,5)]) +
  guides(color=guide_legend(title="First child")) + 
  xlab("Time since first birth") + 
  ylab("Mean time to next boy")
dev.print(device=pdf, 'mt_second_boy_by_sex_first.pdf', width=7, height=5)



# TR: OMG they balance out!
# left first BIRTH time left to second GIRL, stratified by
# sex of the first birth AND age at first birth
db %>% 
  # aligned on ev union, so need to filter on it.
  group_by(sexf, afb5, left_par1) %>% 
  summarize(med_time_left = median(c_step_down_gir1, na.rm=TRUE),
            mean_time_left = weighted.mean(c_step_down_gir1, w=pwt, na.rm=TRUE))  %>% 
  ggplot(mapping = aes(x = left_par1, 
                       y = mean_time_left, 
                       color = as.factor(sexf))) + 
  geom_line() + 
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 9, alpha = .2) +
  facet_wrap(~afb5) + 
  xlim(0,10) + ylim(0,9) +
  scale_color_manual(labels = c("Boy", "Girl"), values = col7[c(2,5)]) +
  guides(color=guide_legend(title="First child")) + 
  xlab("Time since first birth") + 
  ylab("Mean time to next girl")
dev.print(device=pdf, 'mt_second_gir_by_sex_first.pdf', width=7, height=5)