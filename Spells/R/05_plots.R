library(here); library(devtools); library(TraMineR); library(tidyverse); library(doBy)
library(descr); library(survey); library(RColorBrewer); library(tidyr); library(tidyverse)
library(here)
remove(list=ls())
setwd("U:/Cloud/Spells/Spells/Data/Castro")
col7<-c(brewer.pal(9, "PuBu")[4:6], brewer.pal(9, "PuRd")[4:6], 'gray90')

# Load one of the four
#load('estimates_colombia_cohort.RData')
load('estimates_colombia_period.RData')
#load('estimates_senegal_cohort.RData')
#load('estimates_senegal_period.RData')


setwd(here("Spells","Figures"))


#################################################################### TIME TO NEXT CHILD
ylab<-c("Mean time to second birth", "Mean time to next boy", "Mean time to next girl")

for(i in 1:3){
  sst<-data.frame(list(sst_nc, sst_nb, sst_ng)[[i]])
  sst %>% 
    mutate(sexf = as.factor(sexf)) %>% 
    ggplot(mapping = aes(x = left_par1, 
                         y = ymed, 
                         color = sexf)) + 
    geom_line() + 
    geom_ribbon(mapping=aes(ymin = ymin, ymax = ymax, 
                            fill = sexf),alpha=.3, colour = NA, show.legend = FALSE)+
    #geom_vline(xintercept = c(0,1)) +
    # transparent band presumed to isolate twins
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 3.3, alpha = .2) +
    facet_wrap(~afb5) + 
    xlim(0,8)+ ylim(0,3.3) + 
    scale_color_manual(labels = c("Boy", "Girl"), values = col7[c(2,5)]) +
    scale_fill_manual(labels = c("Boy", "Girl"), values = col7[c(2,5)]) +
    guides(color=guide_legend(title="First child")) + 
    xlab("Time since first birth") + 
    ylab(ylab[i])
  dev.print(device=pdf, paste0(file_out, "_new_", i, ".pdf"), width=7.5, height=5.5)
  dev.off()
}

