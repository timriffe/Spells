library(here); library(devtools); library(TraMineR); library(tidyverse); library(doBy)
library(descr); library(survey); library(RColorBrewer); library(xtable)
remove(list=ls())

#for(b in 1:2){; 
b<-1
country<-c('colombia', 'senegal')[b]
file_seq<-paste0("cas_wom_", country, ".rds")
file_raw<-paste0('cas_wom_dhs_raw_', country, '.RData')

setwd("U:/Cloud/Spells/Spells/Data/Castro")
load(paste(file_raw))
dr<-db[, c('ident', 'filenw','v005','v001','v008')]
df<-data.frame(readRDS(paste(file_seq)))

# merge these two
omit<--c(grep('v005', colnames(df)), 
         grep('v008', colnames(df)),grep('filenw', colnames(df)))
db<-merge(df[,omit], dr, by='ident')
db$pwt<-db$v005/1000000
#########################################################################################

# Select the type of analysis 1=Cohort, 2=Period
analysis<-data.frame(cohort=db$maget>=39, 
                     period=db$dfb>=db$v008-(12*10) & db$afb>=15 & db$afb<=45 & 
                       db$filenw!='COIR72FL.DTA')
c<-2
file_out<-paste0(country, '_', names(analysis)[c]); file_out
da<-db[analysis[,c],]

labs<-c('1977-1987','1980-1990','1985-1995','1990-2000','1995-2005','2000-2010','2005-2015')
da$afb5<-factor(as.numeric(cut(round(da$v008/12+1900, 0), c(1987, 1990, 1995, 2000, 2005, 2010, 2015, 2017),
                               right=F, include.lowest=F)), levels=1:7, labels=labs)

ds<-droplevels(unique(da[, c('ident', 'sexf', 'afb5')]))

xtable(addmargins(table(ds$sexf, ds$afb5)), digits=0)
