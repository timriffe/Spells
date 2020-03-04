library(here); library(devtools); library(TraMineR); library(tidyverse); library(doBy)
library(descr); library(survey); library(RColorBrewer)
remove(list=ls())

# SELECT the country. 1=Colombia, 2=Senegal
country<-c('colombia', 'senegal')[2]
file_seq<-paste0("cas_wom_", country, ".rds")
file_raw<-paste0('cas_wom_dhs_raw_', country, '.RData')
  
load(here('Spells', 'Data', 'Castro', file_raw))
dr<-db[, c('ident', 'filenw','v005','v001')]
df<-data.frame(readRDS(here("Spells","Data","Castro", file_seq)))

# merge these two
db<-merge(df[,-grep('v005', colnames(df))], dr, by='ident')
db$pwt<-db$v005/1000000

# Select the type of analysis 1=Cohort, 2=Period
c<-2
analysis<-list(cohort=db$maget>=39, period=!is.na(db$maget))[[c]]

db<-db[analysis[[c]],]
#########################################################################################

file_out<-paste0(country, '_', c('cohort', 'period')[c])

Sys.time(); # runs in 1 min (cohort) 3 min (period)
#sd<-svydesign(~filenw.y+v001.y, weights=~pwt, data=db); sd
sd<-svydesign(~filenw.y, weights=~pwt, data=db); sd

# Estimates for the next birth
est_nc<-svyby(~c_step_down_par1, ~left_par1+afb5+sexf, subset(sd, left_par1>=0), 
           FUN=svymean, na.rm=T)
summary(est_nc)
est_nc[1:22,]

# Estimates for the next birth
est_nb<-svyby(~c_step_down_boy1, ~left_par1+afb5+sexf, subset(sd, left_par1>=0), 
              FUN=svymean, na.rm=T)
summary(est_nb)
est_nb[1:22,]

# Estimates for the next birth
est_ng<-svyby(~c_step_down_gir1, ~left_par1+afb5+sexf, subset(sd, left_par1>=0), 
              FUN=svymean, na.rm=T)
summary(est_ng)
est_ng[1:22,]
Sys.time()

# for sake of easier plotting, change colnames of x variable to be identical
est_nc <- est_nc %>% rename(y = c_step_down_par1)
est_nb <- est_nb %>% rename(y = c_step_down_boy1)
est_ng <- est_ng %>% rename(y = c_step_down_gir1)

remove(list=ls()[-c(grep('_out', ls()), grep('est_', ls()))])
save.image(paste0("U:/Cloud/Spells/Spells/Data/Castro/",'estimates_', file_out, '.RData'))


