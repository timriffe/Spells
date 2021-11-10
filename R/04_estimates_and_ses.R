library(here); library(devtools); library(TraMineR); library(tidyverse); library(doBy)
library(descr); library(survey); library(RColorBrewer)
remove(list=ls())

#for(b in 1:2){; 
b<-2
country<-c('colombia', 'senegal')[b]
file_seq<-paste0("cas_wom_", country, ".rds")
file_raw<-paste0('cas_wom_dhs_raw_', country, '.RData')
  
load(here('Spells', 'Data', 'Castro', file_raw))
dr<-db[, c('ident', 'filenw','v005','v001','v008')]
df<-data.frame(readRDS(here("Spells","Data","Castro", file_seq)))

# merge these two
omit<--c(grep('v005', colnames(df)), grep('v001', colnames(df)),
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

#########################################################################################
bst_nc<-NULL; bst_nb<-NULL; bst_ng<-NULL
Sys.time(); # runs in 5hr with 1,000 rep

for(i in 1:1000){
  if(i==1){
    db<-da
    sd<-svydesign(v001~1, weights=~pwt, data=db)
  }
  if(i!=1){
    sb<-sample(1:nrow(da), nrow(da), replace=TRUE)
    db<-da[sb, ]
    sd<-svydesign(~1, weights=~pwt, data=db)
  }
  
  # Estimates for the next birth
  est_nc<-svyby(~c_step_down_par1, ~left_par1+afb5+sexf, subset(sd, left_par1>=0), 
                FUN=svymean, na.rm=T)
  rownames(est_nc)<-
    paste("s_", substr('0000',1, 4-nchar(i)), i, "_", 1:nrow(est_nc), sep='')
  bst_nc<-rbind(bst_nc, est_nc)
  
  # Estimates for the next boy
  est_nb<-svyby(~c_step_down_boy1, ~left_par1+afb5+sexf, subset(sd, left_par1>=0), 
                FUN=svymean, na.rm=T)
  rownames(est_nb)<-
    paste("s_", substr('0000',1, 4-nchar(i)), i, "_", 1:nrow(est_nb), sep='')
  bst_nb<-rbind(bst_nb, est_nb)
  
  # Estimates for the next girl
  est_ng<-svyby(~c_step_down_gir1, ~left_par1+afb5+sexf, subset(sd, left_par1>=0), 
                FUN=svymean, na.rm=T)
  rownames(est_ng)<-
    paste("s_", substr('0000',1, 4-nchar(i)), i, "_", 1:nrow(est_ng), sep='')
  bst_ng<-rbind(bst_ng, est_ng)
  
}
Sys.time()

# for sake of easier plotting, change colnames of x variable to be identical
bst_nc <- bst_nc %>% rename(y = c_step_down_par1)
bst_nb <- bst_nb %>% rename(y = c_step_down_boy1)
bst_ng <- bst_ng %>% rename(y = c_step_down_gir1)

est_nc<-bst_nc[substr(rownames(bst_nc),1,6)=='s_0001',]
est_nb<-bst_nb[substr(rownames(bst_nb),1,6)=='s_0001',]
est_ng<-bst_ng[substr(rownames(bst_ng),1,6)=='s_0001',]

quan<-c(0.025, 0.5, 0.975)
sst_nc<-summaryBy(y~sexf+afb5+left_par1, data=bst_nc, FUN=quantile, probs=quan)
colnames(sst_nc)[grep("y.", colnames(sst_nc))]<-c('ymin', 'ymed', 'ymax')

sst_nb<-summaryBy(y~sexf+afb5+left_par1, data=bst_nb, FUN=quantile, probs=quan)
colnames(sst_nb)[grep("y.", colnames(sst_nb))]<-c('ymin', 'ymed', 'ymax')

sst_ng<-summaryBy(y~sexf+afb5+left_par1, data=bst_ng, FUN=quantile, probs=quan)
colnames(sst_ng)[grep("y.", colnames(sst_ng))]<-c('ymin', 'ymed', 'ymax')

remove(list=ls()[-c(grep('_out', ls()), grep('st_', ls()))])
save.image(paste0("U:/Cloud/Spells/Spells/Data/Castro/",'estimates_', file_out, '.RData'))
#}

