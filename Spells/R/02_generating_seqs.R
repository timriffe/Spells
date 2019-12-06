library(here)
library(tidyverse)

load(here('Spells', 'Data', 'Castro', 'cas_wom_dhs_raw.RData'))


# move to long format, includes
# only women > age 10, up until (and including) age at survey
# and the variables mage, dob, sex, order, and parity.
# This lacks a union status variable, which could still be calculated
dat <- db %>% 
  select(-starts_with("b0_")) %>% 
  pivot_longer(cols = b3_01:bord_20,
               names_to = c(".value","value"),
               names_sep = "_",
               values_drop_na = TRUE) %>% 
  rename(dob = b3, sex = b4) %>%  
  mutate(mage = floor((dob - v011) / 12),
         maget =  floor((v008 - v011) / 12),
         afu = floor((v509 - v011) / 12)) %>% 
  complete(mage,  nesting(ident, maget)) %>% 
  filter(mage <= maget,
         mage >= 10) %>% 
  group_by(ident) %>% 
  mutate(parity = ifelse(is.na(dob),0,1),
         parity = cumsum(parity),
         bparity = ifelse(!is.na(sex) & sex == 1, 1, 0),
         bparity = cumsum(bparity),
         gparity = ifelse(!is.na(sex) & sex == 2, 1, 0),
         gparity = cumsum(gparity),
         pwt = unique(v005[!is.na(v005)]),
         v008 = unique(v008[!is.na(v008)]),
         yr = v008 / 12 + 1900,
         waveyr = round(yr / 5) * 5,
         waveyr = ifelse(waveyr == 1985, 1986, waveyr),
         afu = floor((v509-v011) / 12),
         evmar = case_when(afu > mage ~ "NM",
                           mage >= afu ~ "M",
                           TRUE ~ "NM")) %>% 
  ungroup() %>% 
  arrange(ident, mage) 
  
# pipe stops here, ready to go with exploratory plots.

# So no need to make these 28hr matrices
maxage<-51
# age at the survey ; 
s <- floor((df$v008-df$v011)/12); 
# age at first marriage/union
a <- floor((df$v509-df$v011)/12)

# matrix of birth order ; matrix of births
o <- df[,grep('bord_', colnames(df))]; 
m <- floor((df[,grep('b3_', colnames(df))]-df$v011)/12)
x <- df[,grep('b4_', colnames(df))]

# initializing objects
m_births <- NULL; 
m_orders <- NULL; 
m_parity <- NULL; 
m_unions <- NULL

###############################################################################################

Sys.time(); # runs in 28h!! WTF
for(i in 1:nrow(df)){
  birth<-rep(0, maxage); order<-rep(0, maxage); 
  union<-rep("Never m/u", maxage); sexch<-rep(NA, maxage)
  unobs<-(s[i]+1):maxage
  
  birth[sort(as.numeric(m[i, !is.na(m[i,])]))]<-1
  birth[unobs] <-NA
  
  parit <- cumsum(birth)
  parit[unobs]<-NA
  
  order[sort(as.numeric(m[i, !is.na(m[i,])]))]<-as.numeric(o[i, as.numeric(o[i, !is.na(o[i,])])])
  order[unobs]<-NA
  
  if(!is.na((a[i]))){
    union[(a[i]:maxage)]<-'Married/In union'
  }
  union[unobs]<-NA
  
  
  
  m_births<-data.frame(rbind(m_births, birth))
  m_orders<-data.frame(rbind(m_orders, order))
  m_parity<-data.frame(rbind(m_parity, parit))
  m_unions<-data.frame(rbind(m_unions, union), stringsAsFactors=FALSE)
  
  gc()
}
Sys.time()

m_births<-data.frame(m_births[,10:50]); colnames(m_births)<-paste0('birth_', 10:50)

m_orders<-data.frame(m_orders[,10:50]); colnames(m_orders)<-paste0('order_', 10:50)

m_parity<-data.frame(m_parity[,10:50]); colnames(m_parity)<-paste0('parit_', 10:50)

m_unions<-data.frame(m_unions[,10:50]); colnames(m_unions)<-paste0('union_', 10:50)

remove(list=ls()[-c(grep("dsets",ls()), grep("df",ls()), grep("vars",ls()), grep("m_",ls()))])
setwd("//sas/psc/dept/global_family_change/outputs/raw_datasets")
save.image('cas_wom_seqs.RData')


