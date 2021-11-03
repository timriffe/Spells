source(here::here("R","00_install_packages.R"))

load(here('Data', 'Application2', 'cas_wom_dhs_raw_colombia.RData'))
dr<-db[, c('ident', 'filenw','v005','v001')]

df<-data.frame(readRDS(here("Data","Application2","cas_wom_colombia.rds")))
#db<-readRDS(here("Data","Application2","cas_wom_colombia.rds"))


# merge these two
db<-merge(df[,-grep('v005', colnames(df))], dr, by='ident')
db$pwt<-db$v005/1000000

c<-1
analysis<-list(cohort=db$maget>=39, period=!is.na(db$maget))[c]

db<-db[analysis[[c]],]
col7<-c(brewer.pal(9, "PuBu")[4:6], brewer.pal(9, "PuRd")[4:6], 'gray90')

Sys.time(); # runs in 1 min
sd<-svydesign(~filenw.y+v001.y, weights=~pwt, data=db); sd
#sd<-svydesign(~1, weights=~pwt, data=db); sd

# Estimates for the next birth
est_nc<-svyby(~c_step_down_par1, ~left_par1+afb5+sexf, 
           subset(sd, left_par1>=0), 
           FUN=svymean, na.rm=T)
summary(est_nc)
est_nc[1:22,]

# Estimates for the next birth
est_nb<-svyby(~c_step_down_boy1, ~left_par1+afb5+sexf, 
              subset(sd, left_par1>=0), 
              FUN=svymean, na.rm=T)
summary(est_nb)
est_nb[1:22,]

# Estimates for the next birth
est_ng<-svyby(~c_step_down_gir1, ~left_par1+afb5+sexf, 
              subset(sd, left_par1>=0), 
              FUN=svymean, na.rm=T)
summary(est_ng)
est_ng[1:22,]
Sys.time()

# for sake of easier plotting, change colnames of x variable to be identical

est_nc <- 
  est_nc %>% 
  rename(y = c_step_down_par1)
est_nb <- 
  est_nb %>% 
  rename(y = c_step_down_boy1)
est_ng <- 
  est_ng %>% 
  rename(y = c_step_down_gir1)


est_nb
est_nb %>% 
  mutate(sexf = as.factor(sexf)) %>% 
ggplot(mapping = aes(x = left_par1, 
                     y = y, 
                     color = sexf)) + 
  geom_line() + 
  geom_ribbon(mapping=aes(ymin = y - 2*se, ymax = y + 2 * se, 
                          fill = sexf),alpha=.3, colour = NA)+
  #geom_vline(xintercept = c(0,1)) +
  # transparent band presumed to isolate twins
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 5, alpha = .2) +
  facet_wrap(~afb5) + 
  xlim(0,10)+ ylim(0,5) + 
  scale_color_manual(labels = c("Boy", "Girl"), values = col7[c(2,5)]) +
  scale_fill_manual(labels = c("Boy", "Girl"), values = col7[c(2,5)]) +
  guides(color=guide_legend(title="First child")) + 
  xlab("Time since first birth") + 
  ylab("Mean time to second birth")
  
  
  
dev.print(device=pdf, here::here("Figures",paste0("mt_birth2_", names(analysis)[c], ".pdf")), width = 7, height = 5)


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
dev.print(device=pdf, here::here("Figures",paste0("mt_birth2_", names(analysis)[c], ".pdf")), width=7, height=5)

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
dev.print(device=pdf, here::here("Figures",paste0("mt_boy2_", names(analysis)[c], ".pdf")), width=7, height=5)


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
dev.print(device=pdf, here::here("Figures",paste0("mt_girl2_", names(analysis)[c], ".pdf")), width=7, height=5)
