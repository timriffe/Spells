rm(list=ls())
libraries <- list("data.table")
lapply(libraries,require, character=T)

# Get the SILC data 2012-15 for Italy and prepare a dataset in transition format
# with relevant variables for the estimation of health expectancies through multistate models

source(here::here("R","00_load_functions.R"))
#------------------files to be imported list------------------------#
getwd()
# TR: should be path in repo, i.e. relative to repo folder
# Also, what is this file, and how would someone request it / download it?

# This is one of four csv files included in a zip folder when you request
# the data from ISTAT. p stands for 'p'erson-file.
# These are the data you want:
# https://www.istat.it/it/archivio/4204
# To request this, go here:
# https://contact.istat.it/index.php?Lingua=Inglese
# Register to become a user
# Then fill out a form here:
# https://www.istat.it/en/analysis-and-products/microdata-files#file_ricerca
# scroll down to 'standard files', choose the doc or pdf versions of the form.
# after filling it out, submit it in your registered profile.
# Processing times vary, but 1-4 weeks can be expected. 
# You'll end up getting access to a zip file.
# extract these to Data/Application1

dat <- fread("U:/Nextcloud/Data/IT-SILC/2012-15/File/l15p.csv")
head(dat)


dat[,.N,by=PB010]
dat[,IDcount:=seq_len(.N),by=PB030]
dat[,IDmax:=.N,by=PB030]
dat[IDmax>1,.N,by=PB010]

#View(dat[,.(PB030,IDmax),])
#----------------------------------------------------------------------------------------------------------------
#PB010: Year
#................................................................................................................
#PB030: Personal ID
##................................................................................................................
# PB100: MONTH OF THE PERSONAL INTERVIEW ........................................................................
# PB110: YEAR OF THE PERSONAL INTERVIEW ......................................................................... 
##................................................................................................................
# PB130: MONTH OF BIRTH ........................................................................................
# PB140: YEAR OF BIRTH ......................................................................................... 
# All month info are in trimester
# PB150: SEX ................................................................................................... 
##................................................................................................................
# PE040: HIGHEST ISCED LEVEL ATTAINED ........................................................................... 
##................................................................................................................
# PH010: GENERAL HEALTH .........................................................................................
# Values 
# 1 Very good 
# 2 Good 
# 3 Fair 
# 4 Bad 
# 5 Very bad 
##................................................................................................................
# PH020: SUFFER FROM ANY CHRONIC (LONG-STANDING) ILLNESS OR CONDITION ...........................................
# Suffer from any illness or health problem of a duration of at least six months
# Values
# 1 Yes 
# 2 No 
##................................................................................................................
# PH030: LIMITATION IN ACTIVITIES BECAUSE OF HEALTH PROBLEMS [GENERAL ACTIVITY] 
# Values 
# 1 Yes, strongly limited 
# 2 Yes, limited 
# 3 No, not limited 
##................................................................................................................
# select variables of interest

datp <- subset(dat,select=c("PB010","PB030","PB150","PE040","PH010","PH020","PH030"))

#----------------------------------------------------------------------------------------------------------------
# get individual data from register file

# TR: should be path in repo, i.e. relative to repo folder
# Also, what is this file, and how would someone request it / download it?
dat <- fread("U:/Nextcloud/Data/IT-SILC/2012-15/File/l15r.csv")

# RB010: YEAR OF THE SURVEY ..................................................................................... 
# RB030: PERSONAL ID ............................................................................................
# RB040: CURRENT HOUSEHOLD ID ................................................................................... 
#...............................................................................................................
# RB050: PERSONAL CROSS-SECTIONAL WEIGHT ........................................................................
# RB060: PERSONAL BASE WEIGHT ................................................................................... 
# RB062: LONGITUDINAL WEIGHT (TWO-YEAR DURATION) ................................................................ 
# RB063: LONGITUDINAL WEIGHT (THREE-YEAR DURATION) .............................................................. 
# RB064: LONGITUDINAL WEIGHT (FOUR-YEAR DURATION) ...............................................................  
# RB070: MONTH OF BIRTH ......................................................................................... 
# RB080: YEAR OF BIRTH ..........................................................................................
#...............................................................................................................
# RB120: MOVED TO [LOCATION WHERE  THE PERSON MOVED].............................................................
# RB140: MONTH MOVED OUT OR DIED [MONTH WHEN THE PERSON MOVED OUT OR DIED] ...................................... 
# RB150: YEAR MOVED OUT OR DIED [YEAR WHEN THE PERSON MOVED OUT OR DIED] ........................................  
#...............................................................................................................
# RX010: Age at the time of the interview .......................................................................
# RX020: Age at the end of income reference period .............................................................. 
# RB110: Membership status
# For current household members 
# 1 Was in this household in previous waves or current 
# household member  
# 2 Moved into this household from another sample household 
# since previous wave 
# 3 Moved into this household from outside sample since 
# previous wave 
# 4 Newly born into this household since last wave 
# 5 Moved out since previous wave or last interview if not contacted in previous wave 
# 6 Died 
# 7 Lived in the household at least three months during the income 
#reference period and was not recorded in the register of this  household 
#...............................................................................................................
# select variables of interest

datr <- subset(dat,select=c("RB010","RB030","RB040","RB060","RB062","RB063","RB150","RB110","RX010","RX020"))
setnames(datr,c("RB010","RB030","RB040"),c("PB010","PB030","PB040"))


dat[RB110==6,PB010D:=ifelse(is.na(RB150),RB010,RB150),]
dat[RB110==6,PH010D:=6,]

datmort <- subset(dat,select=c("RB030","PB010D","PH010D"),RB110==6)
setnames(datmort,c("RB030"),c("PB030"))
#----------------------------------------------------------------------------------------------------------------
# get household data 
# from same data request
dat <- fread("U:/Nextcloud/Data/IT-SILC/2012-15/File/l15h.csv")


# HB010: TOTAL DISPOSABLE HOUSEHOLD INCOME
# HB030: Household Size
# HY020: TOTAL DISPOSABLE HOUSEHOLD INCOME
# HX040: Household Size
# HX050: Equivalised Household Size    
# HX090: (HY020 X HY025)/ HX050 - TOTAL DISPOSABLE HOUSEHOLD INCOME X Equivalised HOUSEHOLD SIZE 
# HX100: Equivalised disposable income quintile
#----------------------------------------------------------------------------------------------------------------

# select variables of interest      
dath <- subset(dat,select=c(HB010,HB030,HY020,HX090,HX100))
#----------------------------------------------------------------------------------------------------------------
# get household data from register file

dat <- fread("U:/Nextcloud/Data/IT-SILC/2012-15/File/l15d.csv")

# DB010: YEAR OF THE SURVEY ....................................................................................... 
# DB030: HOUSEHOLD ID ............................................................................................. 
# DB040: REGION (MACRO AREA).......................................................................................
#----------------------------------------------------------------------------------------------------------------
# select variables of interest      

datd <- subset(dat,select=c(DB010,DB030,DB040))
setnames(datd,c("DB010","DB030"),c("HB010","HB030"))
#----------------------------------------------------------------------------------------------------------------
# Merge data

# individual.....................................................................................................
dati<-merge(datp,datr,by=c("PB030","PB010"),all.y = T)


# Household......................................................................................................
datf<-merge(dath,datd,by=c("HB030","HB010"))

setnames(datf,c("HB010","HB030"),c("PB010","PB040"))
# Merge individual and HH data files.............................................................................

dat <- merge(dati,datf,by=c("PB010","PB040"))  

# Include mortality
setkeyv(dat,c("PB030","PB010"))

#get the id of those who are dead, order the data and add an extra row with values equal to the last available data per individual

id_deaths <- datmort[['PB030']]

datm <- dat[PB030%in%id_deaths,.SD[.N-1],by='PB030']
datm[,`:=` (PB010 = PB010 + 1, PH010 = 6, PH020 = 6, PH030 = 6 , RX010 = RX010 + 1,RX020 = RX020 + 1 )]


setkeyv(dat,c("PB030"))

dat <- rbind(dat[RB110!=6],datm)


rm(list=setdiff(ls(),"dat"))

setnames(dat,c("PH010","PH020","PH030"),c("SRH", "CRON","ADL"))

# ADL TR_FORMAT

dat[,FROM:= sapply(ADL,function(x) {if(x %in% 1:2) 1 else if (x %in% 3) 0 else if (x %in% 6) 2 else if (is.na(x)==T) NA  }),]

dat[,TO := c(FROM[-1L], NA),by=PB030]

# prepare EDU

dat[,edu_low:=0];dat[PE040%in%c(0:2,100,200),edu_low:=1]
dat[,edu_mid:=0];dat[PE040%in%c(3,300),edu_mid:=1]
dat[,edu_high:=0];dat[PE040%in%c(4,400,5,500),edu_high:=1]

dat[,edu:=NA_real_];dat[edu_low==1,edu:=0];dat[edu_mid==1,edu:=1];dat[edu_high==1,edu:=2]
dat[,edu:= factor(edu, levels = c(0,1,2),labels=c("low","mid","high"))]

# prepare Geo AREA

dat[,area3:=NA_character_];dat[DB040%in%c("ITC","ITH"),area3 := "North"]
dat[DB040%in%c("ITI"),area3 := "Centre"];dat[DB040%in%c("ITF","ITG"),area3 := "South"]

dat[,area3:=factor(area3,levels = c("North","Centre","South"))]

dat[,Centre:=ifelse(area3=="Centre",1,0)] 
dat[,South:=ifelse(area3=="South",1,0)]
dat[,North:=ifelse(area3=="North",1,0)]

# prepare dummies for HH equiv income quintiles

dat[,INQ_I:= ifelse(HX100 == 1,1,0)]
dat[,INQ_II:= ifelse(HX100 == 2,1,0)]
dat[,INQ_III:= ifelse(HX100 == 3,1,0)]
dat[,INQ_IV:= ifelse(HX100 == 4,1,0)]
dat[,INQ_V:= ifelse(HX100 == 5,1,0)]

dat[,TER_I:=ifelse(HX100==1,1,0)]
dat[,TER_II:=ifelse(HX100==2,1,0)]
dat[,TER_III:=ifelse(HX100==3,1,0)]

dat[, IDmax := .N, by = "PB030"]

# TR: should be path in repo, i.e. relative to repo folder

#saveRDS(dat,"./raw_silc_data/SILC_panel_12_15_spells.RDS")

saveRDS(dat,"Data/Application2/SILC_panel_12_15_spells.RDS")

