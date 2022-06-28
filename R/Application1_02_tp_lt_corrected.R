rm(list=ls())
libraries <- list("data.table")
lapply(libraries,require, character=T)
# Estimation of transition probabilities between health, disability and death states
# using Silc data 2012-15, survival correction matching survival probs with italian 
# life tables 2014. The matching algorithm follows https://doi.org/10.1007/s13524-017-0619-6

#------------------------------------------------------------------------------
dat <- readRDS("./raw_silc_data/SILC_panel_12_15_spells.RDS")

mineta <- 25
maxeta <- 79
yr <- 2012
age_init <- seq(mineta-10,mineta+10,1)


dat <- setDT(dat[RX010%in%15:84,])
dat <- dat[!is.na(ADL),]

#dati <- dat

setkeyv(dat,c("PB030","PB010"))

# data and models ------------------------------------------------
sub2 <- dat[IDmax==2,]
sub3 <- dat[IDmax==3,]
sub4 <- dat[IDmax==4,] 

boot_fx <-function(){
  
  dati <- as.data.frame(rbind(
    sub2[sample(nrow(sub2), replace = T), ],
    sub3[sample(nrow(sub3), replace = T), ],
    sub4[sample(nrow(sub4), replace = T), ]))
  
  #---------------------------------------------------------------------------------
  tr_format <- setDT(subset(dati,select = c("PB030","RX010","FROM","TO","PB150","PB010","DB040","HX100","INQ_I","INQ_II","INQ_III","INQ_IV","INQ_V","North","Centre","South")))
  names(tr_format) <- c("pid","age","from","to","gender","year","area","INCQ","INQ_I","INQ_II","INQ_III","INQ_IV","INQ_V","North","Centre","South")
  
  
  tr_format[,from:=factor(from,levels = c(0,1,2),labels = c("Healthy","Disabled","Dead")),]
  tr_format[,to:=factor(to,levels = c(0,1,2),labels = c("Healthy","Disabled","Dead")),]
  
  tr_format <- tr_format[from!="Dead",]
  tr_format <- droplevels(tr_format[complete.cases(to),,])
  
  setkey(tr_format,"pid")
  tr_id <- tr_format[J(unique(pid)),mult="first"] 
  
  
  fit <- formula(to ~ from + s(age) + INQ_I + INQ_II + INQ_IV + INQ_V)
  
  fit.m <- vgam(fit 
                ,family=multinomial(refLevel=1), data=droplevels(tr_format[tr_format$gender==1,]), trace=T,control=vgam.control(maxit=50))
  
  # # Model for females
  fit.f <- vgam(fit
                ,family=multinomial(refLevel=1), data=droplevels(tr_format[tr_format$gender==2,]), trace=T,control=vgam.control(maxit=50))
  
  
  
  f2 <- formula(from ~  s(age) + s(age) + INQ_I + INQ_II + INQ_IV + INQ_V)
  
  # Models for corrections
  
  fit_m_weights <- vgam(f2  
                        ,family=multinomial(refLevel=2), data=droplevels(tr_format[tr_format$gender==1,]), trace=T,control=vgam.control(maxit=50))
  
  
  fit_f_weights <- vgam(f2
                        ,family=multinomial(refLevel=2), data=droplevels(tr_format[tr_format$gender==2,]), trace=T,control=vgam.control(maxit=50))
  
  #---------------------------state space variables----------------------
  
  age <- paste(mineta:maxeta)
  hstatus <- c("Healthy","Disabled")
  variables <- list(age=age,hstatus=hstatus)
  
  # Generate state space
  tstates <- suppressWarnings(levels(interaction(variables,sep="::")))
  astates <- c("Dead")
  states <- c(tstates,astates)
  
  #------- probabilitites ---------------------------------------------------------------------------------------------------------
  # Data frame for prediction
  # Dummy data 
  
  tmp_probs <- data.frame(state=tstates)
  setDT(tmp_probs)[, c("from","age") := tstrsplit(state, "::", fixed=TRUE,keep=c(2,1),type.convert=TRUE),]
  tmp_probs[,from:=as.factor(from),]
  
  
  # ITA I Quintile  ====================================================================================
  
  probs.m <- probs.f <- tmp_probs
  
  mvar <- data.frame(INQ_I = 1)
  
  means_q <- tr_format %>%  
    filter(INQ_I == 1) %>% 
    mutate(gender = factor(gender, c(1,2),c("Men","Women"))) %>% 
    group_by(gender) %>%  
    summarize(
      
      INQ_I = mean(INQ_I),
      INQ_II = mean(INQ_II),
      INQ_III = mean(INQ_III),
      INQ_IV = mean(INQ_IV),
      INQ_V = mean(INQ_V),
      .groups = 'drop') %>% 
    select(c(INQ_I,INQ_II,INQ_III,INQ_IV,INQ_V))
  
  
  probs.m <- crossing(probs.m,means_q[1,])
  probs.f <- crossing(probs.f,means_q[2,])
  
  probs.m_q1 <- cbind(probs.m,predict(fit.m,probs.m,"response"))
  probs.f_q1 <- cbind(probs.f,predict(fit.f,probs.f,"response"))
  
  means.m <- crossing(age=mineta:maxeta,means_q[1,])
  means.f <- crossing(age=mineta:maxeta,means_q[2,])
  
  ### Life table correction
  # for each age get the Health distribution 
  
  weights_m_q1 <- cbind(means.m,predict(fit_m_weights,means.m,"response"))[,c("age","Healthy","Disabled")]
  weights_f_q1 <- cbind(means.f,predict(fit_f_weights,means.f,"response"))[,c("age","Healthy","Disabled")]
  
  setnames(weights_m_q1,c("Healthy","Disabled"),c("w_Healthy","w_Disabled")) 
  setnames(weights_f_q1,c("Healthy","Disabled"),c("w_Healthy","w_Disabled")) 
  
  probs.m_q1 <- merge(probs.m_q1,weights_m_q1)
  probs.f_q1 <- merge(probs.f_q1,weights_f_q1)
  
  # ITA II Quintile  ====================================================================================
  
  probs.m <- probs.f <- tmp_probs
  
  mvar <- data.frame(INQ_II = 1)
  
  means_q <- tr_format %>%  
    filter(INQ_II == 1) %>% 
    mutate(gender = factor(gender, c(1,2),c("Men","Women"))) %>% 
    group_by(gender) %>%  
    summarize(
      
      INQ_I = mean(INQ_I),
      INQ_II = mean(INQ_II),
      INQ_III = mean(INQ_III),
      INQ_IV = mean(INQ_IV),
      INQ_V = mean(INQ_V),
      .groups = 'drop') %>% 
    select(c(INQ_I,INQ_II,INQ_III,INQ_IV,INQ_V))
  
  
  probs.m <- crossing(probs.m,means_q[1,])
  probs.f <- crossing(probs.f,means_q[2,])
  
  probs.m_q2 <- cbind(probs.m,predict(fit.m,probs.m,"response"))
  probs.f_q2 <- cbind(probs.f,predict(fit.f,probs.f,"response"))
  
  means.m <- crossing(age=mineta:maxeta,means_q[1,])
  means.f <- crossing(age=mineta:maxeta,means_q[2,])
  
  weights_m_q2 <- cbind(means.m,predict(fit_m_weights,means.m,"response"))[,c("age","Healthy","Disabled")]
  weights_f_q2 <- cbind(means.f,predict(fit_f_weights,means.f,"response"))[,c("age","Healthy","Disabled")]
  
  setnames(weights_m_q2,c("Healthy","Disabled"),c("w_Healthy","w_Disabled")) 
  setnames(weights_f_q2,c("Healthy","Disabled"),c("w_Healthy","w_Disabled")) 
  
  probs.m_q2 <- merge(probs.m_q2,weights_m_q2)
  probs.f_q2 <- merge(probs.f_q2,weights_f_q2)
  
  # ITA III Quintile  ====================================================================================
  
  probs.m <- probs.f <- tmp_probs
  
  mvar <- data.frame(INQ_III = 1)
  
  means_q <- tr_format %>%  
    filter(INQ_III == 1) %>% 
    mutate(gender = factor(gender, c(1,2),c("Men","Women"))) %>% 
    group_by(gender) %>%  
    summarize(
      
      INQ_I = mean(INQ_I),
      INQ_II = mean(INQ_II),
      INQ_III = mean(INQ_III),
      INQ_IV = mean(INQ_IV),
      INQ_V = mean(INQ_V),
      .groups = 'drop') %>% 
    select(c(INQ_I,INQ_II,INQ_III,INQ_IV,INQ_V))
  
  
  probs.m <- crossing(probs.m,means_q[1,])
  probs.f <- crossing(probs.f,means_q[2,])
  
  probs.m_q3 <- cbind(probs.m,predict(fit.m,probs.m,"response"))
  probs.f_q3 <- cbind(probs.f,predict(fit.f,probs.f,"response"))
  
  means.m <- crossing(age=mineta:maxeta,means_q[1,])
  means.f <- crossing(age=mineta:maxeta,means_q[2,])
  
  weights_m_q3 <- cbind(means.m,predict(fit_m_weights,means.m,"response"))[,c("age","Healthy","Disabled")]
  weights_f_q3 <- cbind(means.f,predict(fit_f_weights,means.f,"response"))[,c("age","Healthy","Disabled")]
  
  setnames(weights_m_q3,c("Healthy","Disabled"),c("w_Healthy","w_Disabled")) 
  setnames(weights_f_q3,c("Healthy","Disabled"),c("w_Healthy","w_Disabled")) 
  
  probs.m_q3 <- merge(probs.m_q3,weights_m_q3)
  probs.f_q3 <- merge(probs.f_q3,weights_f_q3)
  
  
  
  # ITA IV Quintile  ====================================================================================
  
  probs.m <- probs.f <- tmp_probs
  
  mvar <- data.frame(INQ_IV = 1)
  
  means_q <- tr_format %>%  
    filter(INQ_IV == 1) %>% 
    mutate(gender = factor(gender, c(1,2),c("Men","Women"))) %>% 
    group_by(gender) %>%  
    summarize(
      
      INQ_I = mean(INQ_I),
      INQ_II = mean(INQ_II),
      INQ_III = mean(INQ_III),
      INQ_IV = mean(INQ_IV),
      INQ_V = mean(INQ_V),
      .groups = 'drop') %>% 
    select(c(INQ_I,INQ_II,INQ_III,INQ_IV,INQ_V))
  
  
  probs.m <- crossing(probs.m,means_q[1,])
  probs.f <- crossing(probs.f,means_q[2,])
  
  probs.m_q4 <- cbind(probs.m,predict(fit.m,probs.m,"response"))
  probs.f_q4 <- cbind(probs.f,predict(fit.f,probs.f,"response"))
  
  means.m <- crossing(age=mineta:maxeta,means_q[1,])
  means.f <- crossing(age=mineta:maxeta,means_q[2,])
  
  weights_m_q4 <- cbind(means.m,predict(fit_m_weights,means.m,"response"))[,c("age","Healthy","Disabled")]
  weights_f_q4 <- cbind(means.f,predict(fit_f_weights,means.f,"response"))[,c("age","Healthy","Disabled")]
  
  setnames(weights_m_q4,c("Healthy","Disabled"),c("w_Healthy","w_Disabled")) 
  setnames(weights_f_q4,c("Healthy","Disabled"),c("w_Healthy","w_Disabled")) 
  
  probs.m_q4 <- merge(probs.m_q4,weights_m_q4)
  probs.f_q4 <- merge(probs.f_q4,weights_f_q4)
  
  
  
  # ITA V Quintile  ====================================================================================
  
  
  probs.m <- probs.f <- tmp_probs
  
  mvar <- data.frame(INQ_V = 1)
  
  means_q <- tr_format %>%  
    filter(INQ_V == 1) %>% 
    mutate(gender = factor(gender, c(1,2),c("Men","Women"))) %>% 
    group_by(gender) %>%  
    summarize(
      
      INQ_I = mean(INQ_I),
      INQ_II = mean(INQ_II),
      INQ_III = mean(INQ_III),
      INQ_IV = mean(INQ_IV),
      INQ_V = mean(INQ_V),
      .groups = 'drop') %>% 
    select(c(INQ_I,INQ_II,INQ_III,INQ_IV,INQ_V))
  
  
  probs.m <- crossing(probs.m,means_q[1,])
  probs.f <- crossing(probs.f,means_q[2,])
  
  probs.m_q5 <- cbind(probs.m,predict(fit.m,probs.m,"response"))
  probs.f_q5 <- cbind(probs.f,predict(fit.f,probs.f,"response"))
  
  means.m <- crossing(age=mineta:maxeta,means_q[1,])
  means.f <- crossing(age=mineta:maxeta,means_q[2,])
  
  ### Life table correction
  # for each age get the Health distribution 
  
  weights_m_q5 <- cbind(means.m,predict(fit_m_weights,means.m,"response"))[,c("age","Healthy","Disabled")]
  weights_f_q5 <- cbind(means.f,predict(fit_f_weights,means.f,"response"))[,c("age","Healthy","Disabled")]
  
  setnames(weights_m_q5,c("Healthy","Disabled"),c("w_Healthy","w_Disabled")) 
  setnames(weights_f_q5,c("Healthy","Disabled"),c("w_Healthy","w_Disabled")) 
  
  probs.m_q5 <- merge(probs.m_q5,weights_m_q5)
  probs.f_q5 <- merge(probs.f_q5,weights_f_q5)
  
  
  # ITA V Quintile  ====================================================================================
  
  probs.m_ita <- rbind(probs.m_q1,probs.m_q2,probs.m_q3,probs.m_q4,probs.m_q5)
  probs.f_ita <- rbind(probs.f_q1,probs.f_q2,probs.f_q3,probs.f_q4,probs.f_q5)
  
  # life tables
  lt_female <- paste("./Data/Application1/Italia",as.numeric(yr)+2,"Femmine.csv",sep="_")
  
  lt_male <- paste("./Data/Application1/Italia",as.numeric(yr)+2,"Maschi.csv",sep="_")
  col_nm <- c("age", "lx", "dx", "qx", "Lx", "px", "ex")
  
  female <- read.csv2(lt_female,skip = 1, header=T, col.names = col_nm)
  male <- read.csv2(lt_male,skip = 1, header=T, col.names = col_nm)
  
  library(tidyverse)
  # merge the health distribution to trans probabilities
  
  sr_probs.f <- merge(probs.f_ita,female[,c("age","qx")],by="age")
  sr_probs.m <- merge(probs.m_ita,male[,c("age","qx")],by="age")
  
  sr_probs.f_long <- sr_probs.f %>% pivot_longer(cols = starts_with("INQ"),names_to = "incomeq",values_to = "values") %>% filter(values == 1)
  sr_probs.m_long <- sr_probs.m %>% pivot_longer(cols = starts_with("INQ"),names_to = "incomeq",values_to = "values") %>% filter(values == 1)
  
  # first we get the survival probabilities by origin state 
  sr_probs.f_long <-
    sr_probs.f_long %>%
    group_by(from, age, incomeq) %>%
    mutate(surv = Healthy + Disabled) %>%
    ungroup() %>%
    # then we get the survival prob by age as weighted avg of the survival by health state
    mutate(
      states_surv = ifelse(from == "Healthy", surv * w_Healthy, surv * w_Disabled)) %>%
    group_by(age,incomeq) %>%
    mutate(
      from_surv = sum(states_surv)) %>%
    ungroup() %>%
    # now we get a scaling factor as a ratio btw estimated surv prob and life table prob
    mutate(
      scaling_surv = from_surv / (1-(qx/1000)),
      # use the scaling factor to update the transition probabilities
      Healthy =  Healthy/scaling_surv,
      Disabled = Disabled/scaling_surv,
      Dead = 1- Healthy - Disabled
      
    )
  
  sr_probs.m_long <-
    sr_probs.m_long %>%
    group_by(from, age, incomeq) %>%
    mutate(surv = Healthy + Disabled) %>%
    ungroup() %>%
    # then we get the survival prob by age as weighted avg of the survival by health state
    mutate(
      states_surv = ifelse(from == "Healthy", surv * w_Healthy, surv * w_Disabled)) %>%
    group_by(age,incomeq) %>%
    mutate(
      from_surv = sum(states_surv)) %>%
    ungroup() %>%
    # now we get a scaling factor as a ratio btw estimated surv prob and life table prob
    mutate(
      scaling_surv = from_surv / (1-(qx/1000)),
      # use the scaling factor to update the transition probabilities
      Healthy =  Healthy/scaling_surv,
      Disabled = Disabled/scaling_surv,
      Dead = 1 - Healthy - Disabled
      
    )
  
  sr_probs.f_long <- setDT(sr_probs.f_long)
  sr_probs.m_long <- setDT(sr_probs.m_long)
  
  probs.m_q1 <- sr_probs.m_long[incomeq=="INQ_I",.(age,state,from,Healthy,Disabled)]
  probs.m_q2 <- sr_probs.m_long[incomeq=="INQ_II",.(age,state,from,Healthy,Disabled)]
  probs.m_q3 <- sr_probs.m_long[incomeq=="INQ_III",.(age,state,from,Healthy,Disabled)]
  probs.m_q4 <- sr_probs.m_long[incomeq=="INQ_IV",.(age,state,from,Healthy,Disabled)]
  probs.m_q5 <- sr_probs.m_long[incomeq=="INQ_V",.(age,state,from,Healthy,Disabled)]
  
  
  probs.f_q1 <- sr_probs.f_long[incomeq=="INQ_I",.(age,state,from,Healthy,Disabled)]
  probs.f_q2 <- sr_probs.f_long[incomeq=="INQ_II",.(age,state,from,Healthy,Disabled)]
  probs.f_q3 <- sr_probs.f_long[incomeq=="INQ_III",.(age,state,from,Healthy,Disabled)]
  probs.f_q4 <- sr_probs.f_long[incomeq=="INQ_IV",.(age,state,from,Healthy,Disabled)]
  probs.f_q5 <- sr_probs.f_long[incomeq=="INQ_V",.(age,state,from,Healthy,Disabled)]
  
  #==============================================================================================
  
  transitions.m <- expand.grid(from=tstates,to=tstates)
  
  
  transitions <- function(dati_tmp){
    require("data.table")
    data_tp <- data.table::melt(setDT(dati_tmp),id.vars=c("state","age"),measure.vars = hstatus,variable.name = "state_to", value.name = "probs")
    data_tp[,to:=paste(age+1,state_to,sep="::"),]
    setnames(data_tp,"state","from")
    data_tp <- merge(transitions.m,data_tp[,c("from","to","probs"),with=F],by=c("from","to"),all.x=T)
    data_tp <- setDT(na.omit(data_tp))
  }
  
  tp_m_q1 <- transitions(probs.m_q1)[,incomeq := "1"][]
  tp_f_q1 <- transitions(probs.f_q1)[,incomeq := "1"][]
  
  tp_m_q2 <- transitions(probs.m_q2)[,incomeq := "2"][]
  tp_f_q2 <- transitions(probs.f_q2)[,incomeq := "2"][]
  
  tp_m_q3 <- transitions(probs.m_q3)[,incomeq := "3"][]
  tp_f_q3 <- transitions(probs.f_q3)[,incomeq := "3"][]
  
  tp_m_q4 <- transitions(probs.m_q4)[,incomeq := "4"][]
  tp_f_q4 <- transitions(probs.f_q4)[,incomeq := "4"][]
  
  tp_m_q5 <- transitions(probs.m_q5)[,incomeq := "5"][]
  tp_f_q5 <- transitions(probs.f_q5)[,incomeq := "5"][]
  
  
  Umat_m_q1 <- dcast(setDT(tp_m_q1[,c("from","to","probs")][]),to~from,value.var = "probs",drop=FALSE, fill=0)
  Umat_f_q1 <- dcast(setDT(tp_f_q1[,c("from","to","probs")][]),to~from,value.var = "probs",drop=FALSE, fill=0)
  
  Umat_m_q1 <- as.matrix(Umat_m_q1,rownames="to")
  Umat_f_q1 <- as.matrix(Umat_f_q1,rownames="to")
  
  
  Umat_m_q2 <- dcast(setDT(tp_m_q2[,c("from","to","probs")][]),to~from,value.var = "probs",drop=FALSE, fill=0)
  Umat_f_q2 <- dcast(setDT(tp_f_q2[,c("from","to","probs")][]),to~from,value.var = "probs",drop=FALSE, fill=0)
  
  Umat_m_q2 <- as.matrix(Umat_m_q2,rownames="to")
  Umat_f_q2 <- as.matrix(Umat_f_q2,rownames="to")
  
  Umat_m_q3 <- dcast(setDT(tp_m_q3[,c("from","to","probs")][]),to~from,value.var = "probs",drop=FALSE, fill=0)
  Umat_f_q3 <- dcast(setDT(tp_f_q3[,c("from","to","probs")][]),to~from,value.var = "probs",drop=FALSE, fill=0)
  
  Umat_m_q3 <- as.matrix(Umat_m_q3,rownames="to")
  Umat_f_q3 <- as.matrix(Umat_f_q3,rownames="to")
  
  
  Umat_m_q4 <- dcast(setDT(tp_m_q4[,c("from","to","probs")][]),to~from,value.var = "probs",drop=FALSE, fill=0)
  Umat_f_q4 <- dcast(setDT(tp_f_q4[,c("from","to","probs")][]),to~from,value.var = "probs",drop=FALSE, fill=0)
  
  Umat_m_q4 <- as.matrix(Umat_m_q4,rownames="to")
  Umat_f_q4 <- as.matrix(Umat_f_q4,rownames="to")
  
  Umat_m_q5 <- dcast(setDT(tp_m_q5[,c("from","to","probs")][]),to~from,value.var = "probs",drop=FALSE, fill=0)
  Umat_f_q5 <- dcast(setDT(tp_f_q5[,c("from","to","probs")][]),to~from,value.var = "probs",drop=FALSE, fill=0)
  
  Umat_m_q5 <- as.matrix(Umat_m_q5,rownames="to")
  Umat_f_q5 <- as.matrix(Umat_f_q5,rownames="to")
  
  
  get_tp <- function(mat,inc_q,sex){
    
    tmp <- as.data.frame(mat)
    tmp[,"to"] <- rownames(tmp)
    row.names(tmp) <- NULL
    tp <- pivot_longer(tmp, !to, names_to = "from",values_to = "probs")
    tp <- tp[tp[,"probs"]>0,c("from","to","probs")]
    tp$INC_Q <- inc_q
    tp$sex <- sex
    tp
  }
  
  
  tp_I_m <- get_tp(Umat_m_q1,"I","M")
  tp_II_m <- get_tp(Umat_m_q2,"II","M")
  tp_III_m <- get_tp(Umat_m_q3,"III","M")
  tp_IV_m <- get_tp(Umat_m_q4,"IV","M")
  tp_V_m <- get_tp(Umat_m_q5,"V","M")
  
  tp_I_f <- get_tp(Umat_f_q1,"I","F")
  tp_II_f <- get_tp(Umat_f_q2,"II","F")
  tp_III_f <- get_tp(Umat_f_q3,"III","F")
  tp_IV_f <- get_tp(Umat_f_q4,"IV","F")
  tp_V_f <- get_tp(Umat_f_q5,"V","F")
  
  
  
  dd <- as.data.frame(rbind(tp_I_m,tp_II_m,tp_III_m,tp_IV_m,tp_V_m, tp_I_f,tp_II_f,tp_III_f,tp_IV_f,tp_V_f))
  
  return(dd)
  
}

library(doParallel)
library(foreach)

options(cores=30)

cl <- makeCluster(30)
registerDoParallel(cl)
getDoParWorkers()

trials <- 1000
out <- NULL
out <- foreach(i = icount(trials),
               .combine = 'rbind',
               .packages=c('VGAM','data.table','tidyverse','janitor'),.errorhandling = 'remove') %dopar% {
                 out <- cbind(boot_fx(),i)
                 
               }               

# Save to start
#saveRDS(dd,"Data/Application1/tp_limitations.rds")

saveRDS(out,"Data/Application1/boot_tp_limitations.rds")

