library(haven);library(readxl);library(dplyr); remove(list=ls())
setwd("U:/Cloud/Spells/Spells/Data/Castro")

# Data frame with the names of the DHS raw files saved in .DTA (Stata) format
dsets<-data.frame(filenw=c("COIR01FL.DTA", "coir22FL.DTA", "COIR31FL.DTA", "COIR41FL.DTA",
                           "COIR53FL.DTA", "COIR61FL.DTA", "COIR72FL.DTA"))

# External file with the names of the variables
vars<-read_xlsx("variables.xlsx", sheet="var_wom")

# Empty data set with necessary columns
dv<-data.frame(matrix(NA, ncol=nrow(vars)))
colnames(dv)<-vars$Variable; dv$filenw<-NA

# Reading original DHS files; runs in 6 minutes
db<-NULL
for(i in 1:nrow(dsets)){
# Reads original data set into a tibble object  
  dt<-read_dta(paste(dsets$filenw[i]))
  
  # Converts to base-R factors (this is necessary to further enforce class character!) 
  for(v in 1:length(vars$Variable)){
    if(vars$typeR[v]=='factor' & 
       length(grep(paste("\\b",vars$Variable[v], "\\b", sep=''), colnames(dt)))>0)
      dt[, paste(vars$Variable[v])]<-as_factor(dt[, paste(vars$Variable[v])])
  }
  
# Adds a column with the file name (easy further merge)    
  filenw<-c(paste(rep(dsets$filenw[i],dim(dt)[1])))
  dt$filenw<-factor(filenw)
  dt<-bind_rows(dt, dv)

# Keeps selected variables and append data set    
  dt<-dt[,c('filenw', vars$Variable)]
  dt$ident<-paste(dt$filenw, gsub(" ",0, dt$caseid), sep='_')
  
# Forces factors into character -only way of keeping all kinds of codifications
  dt<-data.frame(dt, stringsAsFactors = FALSE)
  for(v in 1:length(vars$typeR)){
    if(class(dt[,vars$Variable[v]])=='numeric' & vars$typeR[v]=='factor')
      dt[,vars$Variable[v]]<-as.factor(dt[,vars$Variable[v]])
  }
  db<-data.frame(bind_rows(dt, db), stringsAsFactors = FALSE)
}
db<-db[!is.na(db$filenw),]

# Removing unnecessary objects, there may be warnings!
remove(list=ls()[-c(grep("db",ls()), grep("vars",ls()))])

# Saving Work-space
save.image('cas_wom_dhs_raw_colombia.RData')
# Saved objects: db: 170,281 row and 107 columns, vars: list of variables

