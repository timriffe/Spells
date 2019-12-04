setwd("//sas/psc/dept/global_family_change/outputs/raw_datasets")
load('cas_wom_dhs_raw.RData')
df<-db

maxage<-51
# age at the survey ; age at first marriage/union
s<-round((df$v008-df$v011)/12); a<-round((df$v509-df$v011)/12)

# matrix of birth order ; matrix of births
o<-df[,grep('bord_', colnames(df))]; m<-round((df[,grep('b3_', colnames(df))]-df$v011)/12)
x<-df[,grep('b4_', colnames(df))]

# initializing objects
m_births<-NULL; m_orders<-NULL; m_parity<-NULL; m_unions<-NULL

###############################################################################################

Sys.time(); # runs in 28h!! WTF
for(i in 1:nrow(df)){
  birth<-rep(0, maxage); order<-rep(0, maxage); 
  union<-rep("Never m/u", maxage); sexch<-rep(NA, maxage)
  unobs<-(s[i]+1):maxage
  
  birth[sort(as.numeric(m[i, !is.na(m[i,])]))]<-1
  birth[unobs]<-NA
  
  parit<-cumsum(birth)
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


