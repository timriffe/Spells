library(here); library(devtools); library(TraMineR); library(tidyverse); library(doBy)
library(descr); library(survey); library(RColorBrewer)
remove(list=ls())
setwd("U:/Cloud/Spells/Spells/Data/Castro")
col7<-c(brewer.pal(9, "PuBu")[4:6], brewer.pal(9, "PuRd")[4:6], 'gray90')

# Load one of the four
load('estimates_colombia_cohort.RData')
#load('estimates_colombia_period.RData')
#load('estimates_senegal_cohort.RData')
#load('estimates_senegal_period.RData')


setwd(here("Spells","Figures"))


# Mean time to next 
est_nc %>% 
  mutate(sexf = as.factor(sexf)) %>% 
  ggplot(mapping = aes(x = left_par1, 
                       y = y, 
                       color = sexf)) + 
  geom_line() + 
  geom_ribbon(mapping=aes(ymin = y - 1.96*se, ymax = y + 1.96* se, 
                          fill = sexf),alpha=.3, colour = NA)+
  #geom_vline(xintercept = c(0,1)) +
  # transparent band presumed to isolate twins
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 5, alpha = .2) +
  facet_wrap(~afb5) + 
  xlim(0,10)+ ylim(floor(range(est_nc$y)*1.2)) + 
  scale_color_manual(labels = c("Boy", "Girl"), values = col7[c(2,5)]) +
  scale_fill_manual(labels = c("Boy", "Girl"), values = col7[c(2,5)]) +
  guides(color=guide_legend(title="First child")) + 
  xlab("Time since first birth") + 
  ylab("Mean time to second birth")

dev.print(device=pdf, paste0(file_out, "_mt_birth2", ".pdf"), width=7.5, height=5.5)


est_nb %>% 
  mutate(sexf = as.factor(sexf)) %>% 
  ggplot(mapping = aes(x = left_par1, 
                       y = y, 
                       color = sexf)) + 
  geom_line() + 
  geom_ribbon(mapping=aes(ymin = y - 1.96*se, ymax = y + 1.96* se, 
                          fill = sexf),alpha=.3, colour = NA)+
  #geom_vline(xintercept = c(0,1)) +
  # transparent band presumed to isolate twins
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 5, alpha = .2) +
  facet_wrap(~afb5) + 
  xlim(0,10)+ ylim(floor(range(est_nb$y, est_ng$y)*1.2)) + 
  scale_color_manual(labels = c("Boy", "Girl"), values = col7[c(2,5)]) +
  scale_fill_manual(labels = c("Boy", "Girl"), values = col7[c(2,5)]) +
  guides(color=guide_legend(title="First child")) + 
  xlab("Time since first birth") + 
  ylab("Mean time to next boy")

dev.print(device=pdf, paste0(file_out, "_mt_nextb", ".pdf"), width=7.5, height=5.5)


est_ng %>% 
  mutate(sexf = as.factor(sexf)) %>% 
  ggplot(mapping = aes(x = left_par1, 
                       y = y, 
                       color = sexf)) + 
  geom_line() + 
  geom_ribbon(mapping=aes(ymin = y - 1.96*se, ymax = y + 1.96* se, 
                          fill = sexf),alpha=.3, colour = NA)+
  #geom_vline(xintercept = c(0,1)) +
  # transparent band presumed to isolate twins
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 5, alpha = .2) +
  facet_wrap(~afb5) + 
  xlim(0,10)+ ylim(floor(range(est_nb$y, est_ng$y)*1.2)) + 
  scale_color_manual(labels = c("Boy", "Girl"), values = col7[c(2,5)]) +
  scale_fill_manual(labels = c("Boy", "Girl"), values = col7[c(2,5)]) +
  guides(color=guide_legend(title="First child")) + 
  xlab("Time since first birth") + 
  ylab("Mean time to next girl")

dev.print(device=pdf, paste0(file_out, "_mt_nextg", ".pdf"), width=7.5, height=5.5)