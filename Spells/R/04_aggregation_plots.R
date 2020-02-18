library(here); library(devtools); library(TraMineR); library(tidyverse); library(doBy)
library(descr); library(survey); library(RColorBrewer)
remove(list=ls())

#db<-readRDS(here("Spells","Data","Castro","cas_wom_senegal.rds"))
#db<-readRDS(here("Spells","Data","Castro","cas_wom_colombia.rds"))

c<-2
analysis<-list(cohort=db$maget>=39, period=!is.na(db$maget))

db<-db[analysis[[c]],]
col7<-c(brewer.pal(9, "PuBu")[4:6], brewer.pal(9, "PuRd")[4:6], 'gray90')

### SURVEY 
db$psu<-paste(db$filenw, db$v001)
sd<-svydesign(~1, weights=~pwt, data=db); sd

# Estimates for the next birth
est_nc<-svyby(~c_step_down_par1, ~left_par1+afb5+sexf, 
           subset(sd, left_par1>=0 & left_par1<=10), 
           FUN=svymean, na.rm=T)

# Estimates for the next birth
est_nb<-svyby(~c_step_down_boy1, ~left_par1+afb5+sexf, 
              subset(sd, left_par1>=0 & left_par1<=10), 
              FUN=svymean, na.rm=T)

# Estimates for the next birth
est_ng<-svyby(~c_step_down_gir1, ~left_par1+afb5+sexf, 
              subset(sd, left_par1>=0 & left_par1<=10), 
              FUN=svymean, na.rm=T)




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
dev.print(device=pdf, paste0("mt_birth2_", names(analysis)[c], ".pdf"), width=7, height=5)

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
  xlim(0,10) + ylim(0,6) +
  scale_color_manual(labels = c("Boy", "Girl"), values = col7[c(2,5)]) +
  guides(color=guide_legend(title="First child")) + 
  xlab("Time since first birth") + 
  ylab("Mean time to next boy")
dev.print(device=pdf, paste0("mt_boy2_", names(analysis)[c], ".pdf"), width=7, height=5)



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
  xlim(0,10) + ylim(0,6) +
  scale_color_manual(labels = c("Boy", "Girl"), values = col7[c(2,5)]) +
  guides(color=guide_legend(title="First child")) + 
  xlab("Time since first birth") + 
  ylab("Mean time to next girl")
dev.print(device=pdf, paste0("mt_girl2_", names(analysis)[c], ".pdf"), width=7, height=5)