
# Estimation of transition probabilities between health, disability and death states
# using Silc data 2012-15, survival correction matching survival probs with italian 
# life tables 2014. The matching algorithm follows https://doi.org/10.1007/s13524-017-0619-6

#------------------------------------------------------------------------------
dat <- readRDS("U:/NextCloud/Projects/Spells/Data/SILC_panel_12_15_spells.RDS")

mineta <- 16
maxeta <- 80
yr <- 2012
age_init <- seq(mineta-10,mineta+10,1)


dat <- setDT(dat[RX010%in%15:84,])
dat <- dat[!is.na(ADL),]

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
  select(c(South, Centre, INQ_I,INQ_II,INQ_III,INQ_IV,INQ_V))


probs.m <- crossing(probs.m,means_q[1,])
probs.f <- crossing(probs.f,means_q[2,])

probs.m_q1 <- cbind(probs.m,predict(fit.m,probs.m,"response"))
probs.f_q1 <- cbind(probs.f,predict(fit.f,probs.f,"response"))

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
  select(c(South, Centre, INQ_I,INQ_II,INQ_III,INQ_IV,INQ_V))


probs.m <- crossing(probs.m,means_q[1,])
probs.f <- crossing(probs.f,means_q[2,])

probs.m_q2 <- cbind(probs.m,predict(fit.m,probs.m,"response"))
probs.f_q2 <- cbind(probs.f,predict(fit.f,probs.f,"response"))

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
  select(c(South, Centre, INQ_I,INQ_II,INQ_III,INQ_IV,INQ_V))


probs.m <- crossing(probs.m,means_q[1,])
probs.f <- crossing(probs.f,means_q[2,])

probs.m_q3 <- cbind(probs.m,predict(fit.m,probs.m,"response"))
probs.f_q3 <- cbind(probs.f,predict(fit.f,probs.f,"response"))

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
  select(c(South, Centre, INQ_I,INQ_II,INQ_III,INQ_IV,INQ_V))


probs.m <- crossing(probs.m,means_q[1,])
probs.f <- crossing(probs.f,means_q[2,])

probs.m_q4 <- cbind(probs.m,predict(fit.m,probs.m,"response"))
probs.f_q4 <- cbind(probs.f,predict(fit.f,probs.f,"response"))

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
  select(c(South, Centre, INQ_I,INQ_II,INQ_III,INQ_IV,INQ_V))


probs.m <- crossing(probs.m,means_q[1,])
probs.f <- crossing(probs.f,means_q[2,])

probs.m_q5 <- cbind(probs.m,predict(fit.m,probs.m,"response"))
probs.f_q5 <- cbind(probs.f,predict(fit.f,probs.f,"response"))


#==============================================================================================
transitions.m <- expand.grid(from=tstates,to=tstates)


transitions <- function(dati_tmp){
  
  data_tp <- melt(setDT(dati_tmp),id.vars=c("state","age"),measure.vars = hstatus,variable.name = "state_to", value.name = "probs")
  data_tp[,to:=paste(age+1,state_to,sep="::"),]
  setnames(data_tp,"state","from")
  data_tp <- merge(transitions.m,data_tp[,c("from","to","probs"),with=F],by=c("from","to"),all.x=T)
  data_tp <- setDT(na.omit(data_tp))
}

tp_m_q1 <- transitions(probs.m_q1)[,geo_edu := "ita"][]
tp_f_q1 <- transitions(probs.f_q1)[,geo_edu := "ita"][]

tp_m_q2 <- transitions(probs.m_q2)[,geo_edu := "ita"][]
tp_f_q2 <- transitions(probs.f_q2)[,geo_edu := "ita"][]

tp_m_q3 <- transitions(probs.m_q3)[,geo_edu := "ita"][]
tp_f_q3 <- transitions(probs.f_q3)[,geo_edu := "ita"][]

tp_m_q4 <- transitions(probs.m_q4)[,geo_edu := "ita"][]
tp_f_q4 <- transitions(probs.f_q4)[,geo_edu := "ita"][]

tp_m_q5 <- transitions(probs.m_q5)[,geo_edu := "ita"][]
tp_f_q5 <- transitions(probs.f_q5)[,geo_edu := "ita"][]


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

# Mortality Corrention using LIFE TABLES ================================================================

# TR: instructions for getting these files. Can they be publicly downloaded, or even added to this repo?
# path should be relative to repo
lt_female <- paste("./Data/LT/Italia",as.numeric(yr)+2,"Femmine.csv",sep="_")

lt_male <- paste("./Data/LT/Italia",as.numeric(yr)+2,"Maschi.csv",sep="_")

col_nm <- c("age", "lx", "dx", "qx", "Lx", "px", "ex")

female <- read.csv2(lt_female,skip = 1, header=T, col.names = col_nm)
male <- read.csv2(lt_male,skip = 1, header=T, col.names = col_nm)

# General
ages <- c(as.numeric(age))

minage <- min(ages)-1

ltf <- female[as.numeric(female$age)%in%ages,]

surv_f <- 1 - ltf$qx/1000

state.space <- list(setdiff(states,"Dead"),"Dead")

ltm <- male[as.numeric(male$age)%in%ages,]

surv_m <- 1 - ltm$qx/1000

weight_fm_q <- tr_id %>%
  filter(age >= mineta, age <= maxeta) %>%
  mutate(gender = factor(gender, c(1, 2), c("Men", "Women")),
         INC_Q  = factor(INCQ, c(1, 2, 3, 4, 5), c("I", "II", "III", "IV", "V"))) %>%
  janitor::tabyl(from, INC_Q,gender) %>% 
  janitor::adorn_percentages("col")


weight_m_q1 <- pluck(weight_fm_q,1)[,"I"]
weight_m_q2 <- pluck(weight_fm_q,1)[,"II"]
weight_m_q3 <- pluck(weight_fm_q,1)[,"III"]
weight_m_q4 <- pluck(weight_fm_q,1)[,"IV"]
weight_m_q5 <- pluck(weight_fm_q,1)[,"V"]

weight_f_q1 <- pluck(weight_fm_q,2)[,"I"]
weight_f_q2 <- pluck(weight_fm_q,2)[,"II"]
weight_f_q3 <- pluck(weight_fm_q,2)[,"III"]
weight_f_q4 <- pluck(weight_fm_q,2)[,"IV"]
weight_f_q5 <- pluck(weight_fm_q,2)[,"V"]



weight_fm <- tr_id %>%
  filter(age >= mineta, age <= maxeta) %>%
  mutate(gender = factor(gender, c(1, 2), c("Men", "Women")),
         INC_Q  = factor(INCQ, c(1, 2, 3, 4, 5), c("I", "II", "III", "IV", "V"))) %>%
  janitor::tabyl(INCQ, gender) %>% 
  janitor::adorn_percentages("col")

weights_m <- pluck(weight_fm,2)
weights_f <- pluck(weight_fm,3)


# Match S probs women ==============================


start.states <- paste(minage + 1,hstatus,sep="::")

start.distr_q1 <- numeric(length(state.space[[1]]))

start.distr_q1[match(start.states,state.space [[1]])] <- weight_f_q1

start.distr_q2 <- numeric(length(state.space[[1]]))

start.distr_q2[match(start.states,state.space [[1]])] <- weight_f_q2

start.distr_q3 <- numeric(length(state.space[[1]]))

start.distr_q3[match(start.states,state.space [[1]])] <- weight_f_q3

start.distr_q4 <- numeric(length(state.space[[1]]))

start.distr_q4[match(start.states,state.space [[1]])] <- weight_f_q4

start.distr_q5 <- numeric(length(state.space[[1]]))

start.distr_q5[match(start.states,state.space [[1]])] <- weight_f_q5

for(which.age in ages[-length(ages)]) {
  
  which.states <- paste(which.age,hstatus,sep="::")
  
  surv.states_q1_f <- colSums(Umat_f_q1[,which.states]) 
  surv.states_q2_f <- colSums(Umat_f_q2[,which.states]) 
  surv.states_q3_f <- colSums(Umat_f_q3[,which.states]) 
  surv.states_q4_f <- colSums(Umat_f_q4[,which.states]) 
  surv.states_q5_f <- colSums(Umat_f_q5[,which.states])
  
  index <- (which.age - minage)
  new.surv <- surv_f[index]
  
  # Forecast to get distribution
  
  steps <- ( which.age - (minage + 1) )
  
  distr_q1 <- start.distr_q1
  distr_q2 <- start.distr_q2  
  distr_q3 <- start.distr_q3  
  distr_q4 <- start.distr_q4
  distr_q5 <- start.distr_q5
  
  if(steps > 0) {
    
    for( j in  1 : (steps)   ) {
      
      
      distr_q1 <- Umat_f_q1 %*% distr_q1
      distr_q2 <- Umat_f_q2 %*% distr_q2
      distr_q3 <- Umat_f_q3 %*% distr_q3
      distr_q4 <- Umat_f_q4 %*% distr_q4
      distr_q5 <- Umat_f_q5 %*% distr_q5
      
    }
  }
  
  
  weights_q1 <- distr_q1[match(which.states,state.space [[1]])]
  weights_q2 <- distr_q2[match(which.states,state.space [[1]])]
  weights_q3 <- distr_q3[match(which.states,state.space [[1]])]
  weights_q4 <- distr_q4[match(which.states,state.space [[1]])]
  weights_q5 <- distr_q5[match(which.states,state.space [[1]])]
  
  weights <- weights_f*c(sum(weights_q1),sum(weights_q2),sum(weights_q3),
                         sum(weights_q4),sum(weights_q5))
  
  weights <- weights/sum(weights)
  
  weights_q1 <- weights_q1/sum(weights_q1)
  weights_q2 <- weights_q2/sum(weights_q2)
  weights_q3 <- weights_q3/sum(weights_q3)
  weights_q4 <- weights_q4/sum(weights_q4)
  weights_q5 <- weights_q5/sum(weights_q5)
  
  
  # Rescale
  
  factor <- sum(c(sum(surv.states_q1_f*weights_q1),sum(surv.states_q2_f*weights_q2),sum(surv.states_q3_f*weights_q3),
                  sum(surv.states_q4_f*weights_q4),sum(surv.states_q5_f*weights_q5))*weights)/new.surv
  
  tmp_q1 <- Umat_f_q1[,which.states]/matrix(data=factor,nrow=dim(Umat_f_q1)[1],ncol=length(which.states)) 
  tmp_q2 <- Umat_f_q2[,which.states]/matrix(data=factor,nrow=dim(Umat_f_q2)[1],ncol=length(which.states)) 
  tmp_q3 <- Umat_f_q3[,which.states]/matrix(data=factor,nrow=dim(Umat_f_q3)[1],ncol=length(which.states)) 
  tmp_q4 <- Umat_f_q4[,which.states]/matrix(data=factor,nrow=dim(Umat_f_q4)[1],ncol=length(which.states)) 
  tmp_q5 <- Umat_f_q5[,which.states]/matrix(data=factor,nrow=dim(Umat_f_q5)[1],ncol=length(which.states)) 
  
  # Check
  if(any(colSums(tmp_q1)>1)) {
    
    ratio_q1 <- surv.states_q1_f/new.surv
    
    ratio_q1[ratio_q1%in%c(NA,0)] <- 1
    
    Umat_f_q1[,which.states] <- Umat_f_q1[,which.states]/matrix(data=ratio_q1,nrow=dim(Umat_f_q1)[1],ncol=length(hstatus),byrow=T) 
    
  } else {
    
    Umat_f_q1[,which.states] <- tmp_q1
  }
  
  if(any(colSums(tmp_q2)>1)) {
    
    ratio_q2 <- surv.states_q2_f/new.surv
    
    ratio_q2[ratio_q2%in%c(NA,0)] <- 1
    
    Umat_f_q2[,which.states] <- Umat_f_q2[,which.states]/matrix(data=ratio_q2,nrow=dim(Umat_f_q2)[1],ncol=length(hstatus),byrow=T) 
    
  } else {
    
    Umat_f_q2[,which.states] <- tmp_q2
  }
  
  if(any(colSums(tmp_q3)>1)) {
    
    ratio_q3 <- surv.states_q3_f/new.surv
    
    ratio_q3[ratio_q3%in%c(NA,0)] <- 1
    
    Umat_f_q3[,which.states] <- Umat_f_q3[,which.states]/matrix(data=ratio_q3,nrow=dim(Umat_f_q3)[1],ncol=length(hstatus),byrow=T) 
    
  } else {
    
    Umat_f_q3[,which.states] <- tmp_q3
  }
  
  if(any(colSums(tmp_q4)>1)) {
    
    ratio_q4 <- surv.states_q4_f/new.surv
    
    ratio_q4[ratio_q4%in%c(NA,0)] <- 1
    
    Umat_f_q4[,which.states] <- Umat_f_q4[,which.states]/matrix(data=ratio_q4,nrow=dim(Umat_f_q4)[1],ncol=length(hstatus),byrow=T) 
    
  } else {
    
    Umat_f_q4[,which.states] <- tmp_q4
  }
  
  if(any(colSums(tmp_q5)>1)) {
    
    ratio_q5 <- surv.states_q5_f/new.surv
    
    ratio_q5[ratio_q5%in%c(NA,0)] <- 1
    
    Umat_f_q5[,which.states] <- Umat_f_q5[,which.states]/matrix(data=ratio_q5,nrow=dim(Umat_f_q5)[1],ncol=length(hstatus),byrow=T) 
    
  } else {
    
    Umat_f_q5[,which.states] <- tmp_q5
  }
  
}

# Match S probs men ==============================

start.states <- paste(minage + 1,hstatus,sep="::")

start.distr_q1 <- numeric(length(state.space[[1]]))

start.distr_q1[match(start.states,state.space [[1]])] <- weight_m_q1

start.distr_q2 <- numeric(length(state.space[[1]]))

start.distr_q2[match(start.states,state.space [[1]])] <- weight_m_q2

start.distr_q3 <- numeric(length(state.space[[1]]))

start.distr_q3[match(start.states,state.space [[1]])] <- weight_m_q3

start.distr_q4 <- numeric(length(state.space[[1]]))

start.distr_q4[match(start.states,state.space [[1]])] <- weight_m_q4

start.distr_q5 <- numeric(length(state.space[[1]]))

start.distr_q5[match(start.states,state.space [[1]])] <- weight_m_q5

for(which.age in ages[-length(ages)]) {
  
  which.states <- paste(which.age,hstatus,sep="::")
  
  surv.states_q1_m <- colSums(Umat_m_q1[,which.states]) 
  surv.states_q2_m <- colSums(Umat_m_q2[,which.states]) 
  surv.states_q3_m <- colSums(Umat_m_q3[,which.states]) 
  surv.states_q4_m <- colSums(Umat_m_q4[,which.states]) 
  surv.states_q5_m <- colSums(Umat_m_q5[,which.states])
  
  index <- (which.age - minage)
  new.surv <- surv_m[index]
  
  # Forecast to get distribution
  
  steps <- ( which.age - (minage + 1) )
  
  distr_q1 <- start.distr_q1
  distr_q2 <- start.distr_q2  
  distr_q3 <- start.distr_q3  
  distr_q4 <- start.distr_q4
  distr_q5 <- start.distr_q5
  
  if(steps > 0) {
    
    for( j in  1 : (steps)   ) {
      
      
      distr_q1 <- Umat_m_q1 %*% distr_q1
      distr_q2 <- Umat_m_q2 %*% distr_q2
      distr_q3 <- Umat_m_q3 %*% distr_q3
      distr_q4 <- Umat_m_q4 %*% distr_q4
      distr_q5 <- Umat_m_q5 %*% distr_q5
      
    }
  }
  
  
  weights_q1 <- distr_q1[match(which.states,state.space [[1]])]
  weights_q2 <- distr_q2[match(which.states,state.space [[1]])]
  weights_q3 <- distr_q3[match(which.states,state.space [[1]])]
  weights_q4 <- distr_q4[match(which.states,state.space [[1]])]
  weights_q5 <- distr_q5[match(which.states,state.space [[1]])]
  
  weights <- weights_m*c(sum(weights_q1),sum(weights_q2),sum(weights_q3),
                         sum(weights_q4),sum(weights_q5))
  
  weights <- weights/sum(weights)
  
  weights_q1 <- weights_q1/sum(weights_q1)
  weights_q2 <- weights_q2/sum(weights_q2)
  weights_q3 <- weights_q3/sum(weights_q3)
  weights_q4 <- weights_q4/sum(weights_q4)
  weights_q5 <- weights_q5/sum(weights_q5)
  
  
  # Rescale
  
  factor <- sum(c(sum(surv.states_q1_m*weights_q1),sum(surv.states_q2_m*weights_q2),sum(surv.states_q3_m*weights_q3),
                  sum(surv.states_q4_m*weights_q4),sum(surv.states_q5_m*weights_q5))*weights)/new.surv
  
  tmp_q1 <- Umat_m_q1[,which.states]/matrix(data=factor,nrow=dim(Umat_m_q1)[1],ncol=length(which.states)) 
  tmp_q2 <- Umat_m_q2[,which.states]/matrix(data=factor,nrow=dim(Umat_m_q2)[1],ncol=length(which.states)) 
  tmp_q3 <- Umat_m_q3[,which.states]/matrix(data=factor,nrow=dim(Umat_m_q3)[1],ncol=length(which.states)) 
  tmp_q4 <- Umat_m_q4[,which.states]/matrix(data=factor,nrow=dim(Umat_m_q4)[1],ncol=length(which.states)) 
  tmp_q5 <- Umat_m_q5[,which.states]/matrix(data=factor,nrow=dim(Umat_m_q5)[1],ncol=length(which.states)) 
  
  # Check
  if(any(colSums(tmp_q1)>1)) {
    
    ratio_q1 <- surv.states_q1_m/new.surv
    
    ratio_q1[ratio_q1%in%c(NA,0)] <- 1
    
    Umat_m_q1[,which.states] <- Umat_m_q1[,which.states]/matrix(data=ratio_q1,nrow=dim(Umat_m_q1)[1],ncol=length(hstatus),byrow=T) 
    
  } else {
    
    Umat_m_q1[,which.states] <- tmp_q1
  }
  
  if(any(colSums(tmp_q2)>1)) {
    
    ratio_q2 <- surv.states_q2_m/new.surv
    
    ratio_q2[ratio_q2%in%c(NA,0)] <- 1
    
    Umat_m_q2[,which.states] <- Umat_m_q2[,which.states]/matrix(data=ratio_q2,nrow=dim(Umat_m_q2)[1],ncol=length(hstatus),byrow=T) 
    
  } else {
    
    Umat_m_q2[,which.states] <- tmp_q2
  }
  
  if(any(colSums(tmp_q3)>1)) {
    
    ratio_q3 <- surv.states_q3_m/new.surv
    
    ratio_q3[ratio_q3%in%c(NA,0)] <- 1
    
    Umat_m_q3[,which.states] <- Umat_m_q3[,which.states]/matrix(data=ratio_q3,nrow=dim(Umat_m_q3)[1],ncol=length(hstatus),byrow=T) 
    
  } else {
    
    Umat_m_q3[,which.states] <- tmp_q3
  }
  
  if(any(colSums(tmp_q4)>1)) {
    
    ratio_q4 <- surv.states_q4_m/new.surv
    
    ratio_q4[ratio_q4%in%c(NA,0)] <- 1
    
    Umat_m_q4[,which.states] <- Umat_m_q4[,which.states]/matrix(data=ratio_q4,nrow=dim(Umat_m_q4)[1],ncol=length(hstatus),byrow=T) 
    
  } else {
    
    Umat_m_q4[,which.states] <- tmp_q4
  }
  
  if(any(colSums(tmp_q5)>1)) {
    
    ratio_q5 <- surv.states_q5_m/new.surv
    
    ratio_q5[ratio_q5%in%c(NA,0)] <- 1
    
    Umat_m_q5[,which.states] <- Umat_m_q5[,which.states]/matrix(data=ratio_q5,nrow=dim(Umat_m_q5)[1],ncol=length(hstatus),byrow=T) 
    
  } else {
    
    Umat_m_q5[,which.states] <- tmp_q5
  }
  
}

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
# saveRDS(dd,"U:/NextCloud/Projects/Spells/Data/tp_limitations.rds")

# TR: this should point to a folder in the repo (Data/Application1 for example)
saveRDS(out,"U:/NextCloud/Projects/Spells/Data/boot_tp_limitations.rds")

